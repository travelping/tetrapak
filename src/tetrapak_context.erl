% Copyright 2010-2011, Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

-module(tetrapak_context).
-export([new/1, run_sequentially/2, shutdown/1,
         wait_for/2, wait_shutdown/1, task_done/3, task_wants_output/1,
         register_io_worker/1, register_tasks/2, get_tasks/1,
         import_config/2]).
-export([init/2, loop/1]).

-include("tetrapak.hrl").
-define(TIMEOUT, 10000).

task_done(Ctx, Task, Result) ->
    call(Ctx, {done, Task, Result}).

task_wants_output(Ctx) ->
    cast(Ctx, want_output).

register_io_worker(Ctx) ->
    link(Ctx),
    cast(Ctx, register_io_worker).

register_tasks(Ctx, TList) ->
    call(Ctx, {register_tasks, TList}).

get_tasks(Ctx) ->
    call(Ctx, get_tasks).

import_config(Ctx, Config = #config{}) ->
    call(Ctx, {import_config, Config}).

run_sequentially(Context, []) ->
    shutdown(Context);
run_sequentially(Context, [Task | Rest]) ->
    case wait_for(Context, [Task]) of
        ok ->
            run_sequentially(Context, Rest);
        {error, {unknown_key, Key}} ->
            shutdown(Context),
            {error, {unknown_key, Key}};
        {error, {failed, Key}} ->
            wait_shutdown(Context),
            ?DEBUG("run_sequentially/1: wait shutdown complete"),
            {error, {failed, Key}}
    end.

shutdown(Context) ->
    cast(Context, shutdown),
    wait_shutdown(Context).

wait_for(Ctx, Keys) ->
    case call(Ctx, {wait_for, Keys}) of
        {error, Error} ->
            {error, Error};
        {wait, Running, Started} ->
            RunningMRefs = [monitor(process, Pid) || Pid <- Running],
            AllMRefs = wait_tasks_started(Started, RunningMRefs),
            wait_tasks_down(Ctx, AllMRefs, ok)
    end.

wait_tasks_started([], MRefAcc) ->
    MRefAcc;
wait_tasks_started([Pid | Rest], MRefAcc) ->
    receive
        {Pid, task_started} ->
            Pid ! {self(), proceed},
            wait_tasks_started(Rest, [monitor(process, Pid) | MRefAcc])
    end.

wait_tasks_down(_Ctx, [], Result) ->
    Result;
wait_tasks_down(Ctx, [MRef | WaitRefs], Result) ->
    receive
        {'DOWN', MRef, process, _DownPid, noproc} ->
            exit(badmonitor);
        {'DOWN', MRef, process, _DownPid, {?TASK_DONE, _Name}} ->
            wait_tasks_down(Ctx, WaitRefs, Result);
        {'DOWN', MRef, process, _DownPid, {?TASK_FAIL, Name}} ->
            wait_tasks_down(Ctx, WaitRefs, {error, {failed, Name}})
    end.

wait_shutdown(Process) ->
    MRef = monitor(process, Process),
    receive
        {'DOWN', MRef, process, Process, _Info} -> ok
    end.

%% ------------------------------------------------------------
%% -- server loop
-record(st, {
    directory                  :: string(),
    tasks                      :: [{string(), #task{}}],
    cache                      :: ets:tid(),
    running    = dict:new()    :: dict(),
    done       = gb_sets:new() :: set(),
    io_queue   = queue:new()   :: queue(),
    io_workers = ordsets:new() :: queue()
}).

new(Directory) ->
    proc_lib:start(?MODULE, init, [self(), Directory], 150).

init(Parent, Directory) ->
    process_flag(trap_exit, true),
    Tasks        = tetrapak_task_boot:initial_tmap(),
    CacheTab     = ets:new(?MODULE, [protected, ordered_set, {read_concurrency, true}]),
    InitialState = #st{directory = Directory, tasks = Tasks, cache = CacheTab},
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(InitialState).

loop(LoopState = #st{cache = CacheTable, tasks = TaskMap, running = Running, done = Done, io_queue = IOQueue}) ->
    receive
        {request, FromPid, {register_tasks, TList}} ->
            reply(FromPid, ok),
            loop(LoopState#st{tasks = lists:keymerge(1, lists:ukeysort(1, TList), TaskMap)});

        {request, FromPid, {import_config, #config{values = ConfigValues, objects = ConfigObjects}}} ->
            lists:foreach(fun ({Key, Value}) ->
                                  ets:insert(CacheTable, {{config_value, Key}, Value})
                          end, ConfigValues),
            lists:foreach(fun ({{ObjType, ObjInstance}, ObjValues}) ->
                                  ets:insert(CacheTable, {{config_object, ObjType, ObjInstance}, ObjValues})
                          end, ConfigObjects),
            reply(FromPid, ok),
            loop(LoopState);

        {request, FromPid, get_tasks} ->
            reply(FromPid, TaskMap),
            loop(LoopState);

        {request, FromPid, {wait_for, Keys}} ->
            case resolve_keys(TaskMap, Keys) of
                {unknown_key, Key} ->
                    reply(FromPid, {error, {unknown_key, Key}}),
                    loop(LoopState);
                TaskList ->
                    {NewLoopState, ReplyRunning, ReplyStarted} =
                        lists:foldl(fun (Task, {LoopStateAcc, RunningAcc, StartedAcc}) ->
                                            maybe_start_task(Task, FromPid, LoopStateAcc, RunningAcc, StartedAcc)
                                    end, {LoopState, [], []}, TaskList),
                    reply(FromPid, {wait, ReplyRunning, ReplyStarted}),
                    loop(NewLoopState)
            end;

        {request, FromPid, {done, Task, Variables}} ->
            lists:foreach(fun ({Key, Value}) ->
                                  ets:insert(CacheTable, {{return_value, Key}, Value})
                          end, Variables),
            NewRunning = dict:erase(Task, Running),
            NewDone    = gb_sets:insert(Task, Done),

            reply(FromPid, ok),
            loop(LoopState#st{done = NewDone, running = NewRunning});

        {cast, FromPid, want_output} ->
            loop(LoopState#st{io_queue = push_ioqueue(IOQueue, FromPid)});

        {cast, FromPid, register_io_worker} ->
            loop(LoopState#st{io_workers = ordsets:add_element(FromPid, LoopState#st.io_workers)});

        {cast, _FromPid, shutdown} ->
            do_shutdown(LoopState, undefined, undefined);

        {'EXIT', _DeadPid, {?TASK_DONE, _Name}} ->
            loop(LoopState);

        {'EXIT', DeadPid, {?TASK_FAIL, FailedTask}} ->
            do_shutdown(LoopState, DeadPid, FailedTask);

        {'EXIT', DeadPid, Reason} ->
            ?DEBUG("ctx EXIT ~p ~p", [DeadPid, Reason]),
            case ordsets:is_element(DeadPid, LoopState#st.io_workers) of
                true ->
                    loop(LoopState#st{io_queue = pop_ioqueue(IOQueue, DeadPid),
                                      io_workers = ordsets:del_element(DeadPid, LoopState#st.io_workers)});
                false ->
                    loop(LoopState)
            end;
        Other ->
            ?DEBUG("ctx other ~p", [Other])
    end.

do_shutdown(#st{running = Running, io_workers = IOWorkers, io_queue = IOQueue}, FailedPid, FailedTask) ->
   RList   = dict:to_list(Running),
   Workers = [Pid || {_Name, Pid} <- RList, Pid /= FailedPid],
   shutdown_loop(Workers ++ IOWorkers, Running, IOQueue, FailedTask).

shutdown_loop([], _Running, _IOQueue, _FailedTask) ->
    ok;
shutdown_loop(Workers, Running, IOQueue, FailedTask) ->
    receive
        {'EXIT', Pid, _Reason} ->
            case queue:member(Pid, IOQueue) of
                true ->
                    shutdown_loop(lists:delete(Pid, Workers), Running, pop_ioqueue(IOQueue, Pid), FailedTask);
                false ->
                    shutdown_loop(lists:delete(Pid, Workers), Running, IOQueue, FailedTask)
            end;
        {cast, FromPid, want_output} ->
            shutdown_loop(Workers, Running, push_ioqueue(IOQueue, FromPid), FailedTask);
        {request, FromPid, {wait_for, _Keys}} ->
            reply(FromPid, {error, {failed, FailedTask}}),
            shutdown_loop(Workers, Running, IOQueue, FailedTask);
        Other ->
            ?DEBUG("shutdown other ~p", [Other]),
            shutdown_loop(Workers, Running, IOQueue, FailedTask)
    end.

pop_ioqueue(IOQueue, DoneIOProc) ->
    case queue:out(IOQueue) of
        {empty, NewIOQueue} ->
            NewIOQueue;
        {{value, DoneIOProc}, NewIOQueue} ->
            case queue:out(NewIOQueue) of
                {empty, ReturnIOQueue} ->
                    ReturnIOQueue;
                {{value, NextIOProc}, _Q} ->
                    reply(NextIOProc, output_ok),
                    NewIOQueue %% Next stays in the queue!!
            end;
        {{value, OtherIOProc}, _} ->
            %% was not head, but we're removing it anyway
            queue:filter(fun (X) -> X /= OtherIOProc end, IOQueue)
    end.

push_ioqueue(IOQueue, FromPid) ->
    case queue:is_empty(IOQueue) of
        true  -> reply(FromPid, output_ok);
        false -> ok
    end,
    queue:in(FromPid, IOQueue).

maybe_start_task(Task = #task{name = TaskName}, CallerPid, State, RunningAcc, StartedAcc) ->
    case dict:find(TaskName, State#st.running) of
        {ok, WorkerPid} ->
            {State, [WorkerPid | RunningAcc], StartedAcc};
        error ->
            case gb_sets:is_member(TaskName, State#st.done) of
                false ->
                    %% task has not been run yet, the caller needs to wait
                    WorkerPid = spawn_link(tetrapak_task, worker, [Task, self(), CallerPid, State#st.directory, State#st.cache]),
                    NewRunning = dict:store(TaskName, WorkerPid, State#st.running),
                    NewState = State#st{running = NewRunning},
                    {NewState, RunningAcc, [WorkerPid | StartedAcc]};
                true ->
                    %% task already did it's job so it's not added to the wait list
                    {State, RunningAcc, StartedAcc}
            end
    end.

resolve_keys(TaskMap, Keys) ->
    try
        lists:foldl(fun (Key, Acc) ->
                          [First | Rest] = tetrapak_task:split_name(Key),
                          NewTasks = descending_lookup(TaskMap, [First], Rest),
                          lists:keymerge(#task.name, NewTasks, Acc)
                    end, [], Keys)
    catch
        throw:{unknown, Key} ->
            {unknown_key, Key}
    end.

descending_lookup(TaskMap, Prefix, KeyRest) ->
    Matches = [{SKey, Task} || {SKey, Task} <- TaskMap, lists:prefix(Prefix, SKey)],
    case {Matches, KeyRest} of
        {[], _}           -> throw({unknown, string:join(Prefix ++ KeyRest, ":")});
        {_, []}           -> [Task || {_K, Task} <- Matches];
        {[{_K, Task}], _} -> [Task]; %% required key is in output variables
        {_, [Next | KR]}  -> descending_lookup(Matches, Prefix ++ [Next], KR)
    end.

%% ------------------------------------------------------------
%% -- micro gen_server
call(Ctx, Request) ->
    call(Ctx, Request, infinity).
call(Ctx, Request, Timeout) ->
    Ctx ! {request, self(), Request},
    receive
        {reply, Ctx, Reply} -> Reply
    after
        Timeout -> exit(context_reply_timeout)
    end.

cast(Ctx, Cast) ->
    Ctx ! {cast, self(), Cast}.

reply(Pid, Reply) ->
    Pid ! {reply, self(), Reply}.
