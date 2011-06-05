%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_context).
-export([new/1, get_cached/2, wait_for/2, wait_shutdown/1, get_directory/1, signal_done/3]).
-export([init/1, loop/1]).

-include("tetrapak.hrl").
-define(TIMEOUT, 10000).

get_directory(Ctx) ->
    call(Ctx, get_directory, ?TIMEOUT).

signal_done(Ctx, Task, Result) ->
    cast(Ctx, {done, Task, Result}).

get_cached(Ctx, Key) ->
    call(Ctx, {get_cached, Key}, ?TIMEOUT).

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
        {'DOWN', MRef, process, _DownPid, {?TASK_DONE, Name}} ->
            wait_loop(Ctx, lists:delete(MRef, WaitList));
        {'DOWN', _MRef, process, _DownPid, {?TASK_FAIL, Name}} ->
            {error, {failed, Name}}
    end.

wait_shutdown(Ctx) ->
    MRef = monitor(process, Ctx),
    receive
        {'DOWN', MRef, process, Ctx, _Info} -> ok
    end.

%% ------------------------------------------------------------
%% -- server loop
-record(st, {
    directory                 :: string(),
    tasks                     :: [{string(), #task{}}],
    cache     = dict:new()    :: dict(),
    running   = dict:new()    :: dict(),
    done      = gb_sets:new() :: set()
}).

new(Directory) ->
    spawn_link(?MODULE, init, [Directory]).

init(Directory) ->
    process_flag(trap_exit, true),
    loop(#st{directory = Directory, tasks = tetrapak_task:find_tasks()}).

loop(LoopState = #st{cache = Cache, tasks = TaskMap, running = Running, done = Done}) ->
    receive
        {request, FromPid, get_directory} ->
            reply(FromPid, LoopState#st.directory),
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

        {cast, _FromPid, {done, Task, Variables}} ->
            NewCache  = dict:merge(fun (Key, _V1, V2) ->
                                           tpk_log:debug("ctx var merge conflict ~p", [Key]),
                                           V2
                                   end, Cache, Variables),
            NewRunning = dict:erase(Task, Running),
            NewDone    = gb_sets:insert(Task, Done),
            loop(LoopState#st{cache = NewCache, done = NewDone, running = NewRunning});

        {'EXIT', _DeadPid, {?TASK_DONE, _Name}} ->
            loop(LoopState);

        {'EXIT', DeadPid, {?TASK_FAIL, _FailedTask}} ->
            shutdown(LoopState, DeadPid);

        Other ->
            tpk_log:debug("ctx other ~p", [Other])
    end.

shutdown(#st{running = Running}, FailedPid) ->
   RList   = dict:to_list(Running),
   Workers = [Pid || {_Name, Pid} <- RList, Pid /= FailedPid],
   shutdown_loop(Workers, Running).

shutdown_loop([], _Running) -> ok;
shutdown_loop(Workers, Running) ->
    receive
        {'EXIT', Pid, {?TASK_FAIL, _TaskName}} ->
            shutdown_loop(lists:delete(Pid, Workers), Running);
        OtherMsg ->
            tpk_log:debug("ctx shutdown other ~p", [OtherMsg])
    end.

maybe_start_task(Task = #task{name = TaskName}, State, CallerWaitList) ->
    case dict:find(TaskName, State#st.running) of
        {ok, WorkerPid} ->
            {State, [WorkerPid | CallerWaitList]};
        error ->
            case gb_sets:is_member(TaskName, State#st.done) of
                false ->
                    %% task has not been run yet, the caller needs to wait
                    WorkerPid = spawn_link(tetrapak_task, worker, [Task, self()]),
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
    get_response(Ctx, Timeout).

get_response(Ctx, Timeout) ->
    receive
        {reply, Ctx, Reply} -> Reply
    after
        Timeout -> exit(context_reply_timeout)
    end.

cast(Ctx, Cast) ->
    Ctx ! {cast, self(), Cast}.

reply(Pid, Reply) ->
    Pid ! {reply, self(), Reply}.
