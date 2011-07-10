%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_context).
-export([new/1, run_sequentially/2, shutdown/1,
         get_cached/2, wait_for/2, wait_shutdown/1,
         task_done/3, task_wants_output/1, task_output_done/1,
         register_io_worker/1, register_tasks/2, get_tasks/1]).
-export([init/1, loop/1]).

-include("tetrapak.hrl").
-define(TIMEOUT, 10000).

task_done(Ctx, Task, Result) ->
    cast(Ctx, {done, Task, Result}).

task_wants_output(Ctx) ->
    cast(Ctx, want_output).

task_output_done(Ctx) ->
    cast(Ctx, output_done).

register_io_worker(Ctx) ->
    link(Ctx),
    cast(Ctx, register_io_worker).

register_tasks(Ctx, TList) ->
    call(Ctx, {register_tasks, TList}).

get_tasks(Ctx) ->
    call(Ctx, get_tasks).

get_cached(Ctx, Key) ->
    call(Ctx, {get_cached, Key}, ?TIMEOUT).

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
            {error, {failed, Key}}
    end.

shutdown(Context) ->
    cast(Context, shutdown),
    wait_shutdown(Context).

wait_for(Ctx, Keys) ->
    case call(Ctx, {wait_for, Keys}) of
        {unknown_key, Key} ->
            {error, {unknown_key, Key}};
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
    cache      = dict:new()    :: dict(),
    running    = dict:new()    :: dict(),
    done       = gb_sets:new() :: set(),
    io_queue   = queue:new()   :: queue(),
    io_workers = ordsets:new() :: queue()
}).

new(Directory) ->
    spawn_link(?MODULE, init, [Directory]).

init(Directory) ->
    process_flag(trap_exit, true),
    Tasks        = tetrapak_task_boot:initial_tmap(),
    InitialState = #st{directory = Directory, tasks = Tasks},
    loop(InitialState).

loop(LoopState = #st{cache = Cache, tasks = TaskMap, running = Running, done = Done, io_queue = IOQueue}) ->
    receive
        {request, FromPid, {register_tasks, TList}} ->
            reply(FromPid, ok),
            loop(LoopState#st{tasks = lists:keymerge(1, lists:ukeysort(1, TList), TaskMap)});

        {request, FromPid, get_tasks} ->
            reply(FromPid, TaskMap),
            loop(LoopState);

        {request, FromPid, {get_cached, Key}} ->
            case dict:find(Key, Cache) of
                {ok, Value} -> reply(FromPid, {ok, Value});
                error       -> reply(FromPid, {error, unknown_key})
            end,
            loop(LoopState);

        {request, FromPid, {wait_for, Keys}} ->
            case resolve_keys(TaskMap, Keys) of
                {unknown_key, Key} ->
                    reply(FromPid, {unknown_key, Key}),
                    loop(LoopState);
                TaskList ->
                    {NewLoopState, ReplyRunning, ReplyStarted} =
                        lists:foldl(fun (Task, {LoopStateAcc, RunningAcc, StartedAcc}) ->
                                            maybe_start_task(Task, FromPid, LoopStateAcc, RunningAcc, StartedAcc)
                                    end, {LoopState, [], []}, TaskList),
                    reply(FromPid, {wait, ReplyRunning, ReplyStarted}),
                    loop(NewLoopState)
            end;

        {cast, FromPid, want_output} ->
            loop(LoopState#st{io_queue = push_ioqueue(IOQueue, FromPid)});

        {cast, FromPid, output_done} ->
            loop(LoopState#st{io_queue = pop_ioqueue(IOQueue, FromPid)});

        {cast, FromPid, register_io_worker} ->
            loop(LoopState#st{io_workers = ordsets:add_element(FromPid, LoopState#st.io_workers)});

        {cast, _FromPid, {done, Task, Variables}} ->
            NewCache   = dict:merge(fun (_Key, _V1, V2) -> V2 end, Cache, Variables),
            NewRunning = dict:erase(Task, Running),
            NewDone    = gb_sets:insert(Task, Done),

            loop(LoopState#st{cache = NewCache, done = NewDone, running = NewRunning});

        {cast, _FromPid, shutdown} ->
            do_shutdown(LoopState, undefined);

        {'EXIT', _DeadPid, {?TASK_DONE, _Name}} ->
            loop(LoopState);

        {'EXIT', DeadPid, {?TASK_FAIL, _FailedTask}} ->
            do_shutdown(LoopState, DeadPid);

        {'EXIT', DeadPid, Reason} ->
            ?DEBUG("ctx EXIT ~p ~p", [DeadPid, Reason]),
            loop(LoopState#st{io_workers = ordsets:del_element(DeadPid, LoopState#st.io_workers)});

        Other ->
            ?DEBUG("ctx other ~p", [Other])
    end.

do_shutdown(#st{running = Running, io_workers = IOWorkers, io_queue = IOQueue}, FailedPid) ->
   RList   = dict:to_list(Running),
   Workers = [Pid || {_Name, Pid} <- RList, Pid /= FailedPid],
   shutdown_loop(Workers ++ IOWorkers, Running, IOQueue).

shutdown_loop([], _, _) ->
    ok;
shutdown_loop(Workers, Running, IOQueue) ->
    receive
        {'EXIT', Pid, _Reason} ->
            shutdown_loop(lists:delete(Pid, Workers), Running, IOQueue);
        {cast, FromPid, output_done} ->
            shutdown_loop(Workers, Running, pop_ioqueue(IOQueue, FromPid));
        {cast, FromPid, want_output} ->
            shutdown_loop(Workers, Running, push_ioqueue(IOQueue, FromPid));
        Other ->
            ?DEBUG("shutdown other ~p", [Other]),
            shutdown_loop(Workers, Running, IOQueue)
    end.

pop_ioqueue(IOQueue, DoneIOProc) ->
    case queue:out(IOQueue) of
        {empty, NewIOQueue}               -> NewIOQueue;
        {{value, DoneIOProc}, NewIOQueue} ->
            case queue:out(NewIOQueue) of
                {empty, ReturnIOQueue}    -> ReturnIOQueue;
                {{value, NextIOProc}, _Q} ->
                    reply(NextIOProc, output_ok),
                    NewIOQueue %% Next stays in the queue!!
            end
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
                    WorkerPid = spawn_link(tetrapak_task, worker, [Task, self(), CallerPid, State#st.directory]),
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
