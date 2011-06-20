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
         task_done/3, task_wants_output/2, task_output_done/2,
         register_tasks/2, get_tasks/1]).
-export([init/1, loop/1]).

-include("tetrapak.hrl").
-define(TIMEOUT, 10000).

task_done(Ctx, Task, Result) ->
    cast(Ctx, {done, Task, Result}).

task_wants_output(Ctx, WorkerPid) ->
    cast(Ctx, {want_output, WorkerPid}).
task_output_done(Ctx, WorkerPid) ->
    cast(Ctx, {output_done, WorkerPid}).

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
        {wait, WaitList} ->
            wait_loop(Ctx, [monitor(process, Pid) || Pid <- WaitList])
    end.

wait_loop(_Ctx, []) ->
    ok;
wait_loop(Ctx, WaitList) ->
    receive
        {'DOWN', MRef, process, _DownPid, {?TASK_DONE, _Name}} ->
            wait_loop(Ctx, lists:delete(MRef, WaitList));
        {'DOWN', _MRef, process, _DownPid, {?TASK_FAIL, Name}} ->
            {error, {failed, Name}}
    end.

wait_shutdown(Process) ->
    MRef = monitor(process, Process),
    receive
        {'DOWN', MRef, process, Process, _Info} -> ok
    end.

%% ------------------------------------------------------------
%% -- server loop
-record(st, {
    directory                 :: string(),
    tasks                     :: [{string(), #task{}}],
    cache     = dict:new()    :: dict(),
    running   = dict:new()    :: dict(),
    done      = gb_sets:new() :: set(),
    io_queue  = queue:new()   :: queue()
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
            loop(LoopState#st{tasks = TList ++ TaskMap});

        {request, FromPid, get_tasks} ->
            reply(FromPid, TaskMap),
            loop(LoopState);

        {request, FromPid, {get_cached, Key}} ->
            tpk_log:debug("ctx get: ~p ~p", [FromPid, Key]),
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
                    {NewLoopState, Wait} = lists:foldl(fun ({_, Task}, {LS, S}) ->
                                                              maybe_start_task(Task, LS, S)
                                                       end, {LoopState, []}, TaskList),
                    reply(FromPid, {wait, Wait}),
                    loop(NewLoopState)
            end;

        {cast, FromPid, {want_output, TaskWorker}} ->
            loop(LoopState#st{io_queue = push_ioqueue(IOQueue, FromPid, TaskWorker)});

        {cast, _FromPid, {output_done, _TaskWorker}} ->
            loop(LoopState#st{io_queue = pop_ioqueue(IOQueue)});

        {cast, _FromPid, {done, Task, Variables}} ->
            NewCache  = dict:merge(fun (Key, _V1, V2) ->
                                           tpk_log:debug("ctx var merge conflict ~p", [Key]),
                                           V2
                                   end, Cache, Variables),
            NewRunning = dict:erase(Task, Running),
            NewDone    = gb_sets:insert(Task, Done),

            loop(LoopState#st{cache = NewCache, done = NewDone, running = NewRunning});

        {cast, _FromPid, shutdown} ->
            do_shutdown(LoopState, undefined);

        {'EXIT', _DeadPid, {?TASK_DONE, _Name}} ->
            loop(LoopState);

        {'EXIT', DeadPid, {?TASK_FAIL, _FailedTask}} ->
            do_shutdown(LoopState, DeadPid);

        Other ->
            tpk_log:debug("ctx other ~p", [Other])
    end.

do_shutdown(#st{running = Running, io_queue = IOQueue}, FailedPid) ->
   RList   = dict:to_list(Running),
   Workers = [Pid || {_Name, Pid} <- RList, Pid /= FailedPid],
   shutdown_loop(Workers, Running, IOQueue).

shutdown_loop([], _Running, IOQueue) ->
    finish_output(IOQueue);
shutdown_loop(Workers, Running, IOQueue) ->
    receive
        {'EXIT', Pid, {Reason, _TaskName}} when Reason == ?TASK_FAIL; Reason == ?TASK_DONE ->
            shutdown_loop(lists:delete(Pid, Workers), Running, IOQueue);
        {cast, _FromPid, {output_done, _TaskWorker}} ->
            shutdown_loop(Workers, Running, pop_ioqueue(IOQueue));
        {cast, FromPid, {want_output, TaskWorker}} ->
            shutdown_loop(Workers, Running, push_ioqueue(IOQueue, FromPid, TaskWorker));
        Other ->
            tpk_log:debug("shutdown other ~p", [Other]),
            shutdown_loop(Workers, Running, IOQueue)
    end.

pop_ioqueue(IOQueue) ->
    case queue:out(IOQueue) of
        {empty, NewIOQueue}                              -> ok;
        {{value, {_NextWorker, NextIOProc}}, NewIOQueue} -> reply(NextIOProc, output_ok)
    end,
    NewIOQueue.

push_ioqueue(IOQueue, FromPid, TaskWorker) ->
    case queue:is_empty(IOQueue) of
        true  -> reply(FromPid, output_ok);
        false -> ok
    end,
    queue:in({TaskWorker, FromPid}, IOQueue).

finish_output(IOQueue) ->
    tpk_log:debug("finish output: ~p", [IOQueue]),
    case queue:peek(IOQueue) of
        empty -> ok;
        {value, {Worker, IOProc}} ->
            reply(IOProc, output_ok),
            receive
                {cast, IOProc, {output_done, Worker}} ->
                    finish_output(queue:drop(IOQueue));
                Other ->
                    tpk_log:debug("finish output other: ~p", [Other]),
                    finish_output(IOQueue)
            end
    end.

maybe_start_task(Task = #task{name = TaskName}, State, CallerWaitList) ->
    case dict:find(TaskName, State#st.running) of
        {ok, WorkerPid} ->
            {State, [WorkerPid | CallerWaitList]};
        error ->
            case gb_sets:is_member(TaskName, State#st.done) of
                false ->
                    %% task has not been run yet, the caller needs to wait
                    WorkerPid = spawn_link(tetrapak_task, worker, [Task, self(), State#st.directory]),
                    NewRunning = dict:store(TaskName, WorkerPid, State#st.running),
                    {State#st{running = NewRunning}, [WorkerPid | CallerWaitList]};
                true ->
                    %% task already did it's job so it's not added to the wait list
                    {State, CallerWaitList}
            end
    end.

resolve_keys(TaskMap, Keys) ->
    try
        lists:foldl(fun (Key, Acc) ->
                          [First | Rest] = tetrapak_task:split_name(Key),
                          case descending_lookup(TaskMap, [First], Rest) of
                              error  -> throw({unknown, Key});
                              Assocs -> lists:keymerge(1, Assocs, Acc)
                          end
                    end, [], Keys)
    catch
        throw:{unknown, Key} ->
            {unknown_key, Key}
    end.

descending_lookup(TaskMap, Prefix, KeyRest) ->
    Matches = lists:filter(fun ({Name, _}) -> lists:prefix(Prefix, Name) end, TaskMap),
    case {Matches, KeyRest} of
        {[], _}      -> error;
        {_, []}      -> Matches;
        {[Match], _} -> [Match]; %% required key is in output variables
        {_, [Next | KR]} ->
            descending_lookup(Matches, Prefix ++ [Next], KR)
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
