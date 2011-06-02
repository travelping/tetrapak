%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_context).
-export([new/1, get_cached/2, wait_for/2, get_directory/1, signal_done/3]).
-export([init/1, loop/1]).
-compile(export_all).

-include("tetrapak.hrl").
-define(TIMEOUT, 10000).

get_directory(Ctx) ->
    call(Ctx, get_directory, ?TIMEOUT).

signal_done(Ctx, Pass, Result) ->
    cast(Ctx, {ready, Pass, Result}).

get_cached(Ctx, Key) ->
    call(Ctx, {get_cached, Key}, ?TIMEOUT).

wait_for(Ctx, Keys) ->
    case call(Ctx, {wait_for, Keys}) of
        {unknown_key, Key} ->
            {error, {unknown_key, Key}};
        {error, {failed, Pass}} ->
            {error, {failed, Pass}};
        {wait, WaitList} ->
            wait_loop(Ctx, WaitList)
    end.

wait_loop(_Ctx, []) ->
    ok;
wait_loop(Ctx, WaitList) ->
    case get_response(Ctx, infinity) of
        {done, Pass} ->
            wait_loop(Ctx, lists:delete(Pass, WaitList));
        {failed, Pass} ->
            {error, {failed, Pass}};
        wait_shutdown ->
            wait_shutdown(Ctx)
    end.

wait_shutdown(Ctx) ->
    MRef = erlang:monitor(process, Ctx),
    receive
        {'DOWN', MRef, process, Ctx, _Info} ->
            {error, shutdown}
    end.

%% ------------------------------------------------------------
%% -- server loop
-record(st, {
    directory                 :: string(),
    passes                    :: [{string(), #pass{}}],
    cache     = dict:new()    :: dict(),
    running   = dict:new()    :: dict(),
	done      = gb_sets:new() :: set()
}).

new(Directory) ->
    spawn_link(?MODULE, init, [Directory]).

init(Directory) ->
    process_flag(trap_exit, true),
    loop(#st{directory = Directory, passes = tep_pass:find_passes()}).

loop(LoopState = #st{cache = Cache, passes = PassMap, running = Running, done = Done}) ->
    receive
        {request, FromPid, get_directory} ->
            reply(FromPid, LoopState#st.directory),
            loop(LoopState);

        {request, FromPid, {get_cached, Key}} ->
            tep_log:debug("ctx get: ~p ~p", [FromPid, Key]),
            case dict:find(Key, Cache) of
                {ok, Value} -> reply(FromPid, {ok, Value});
                error       -> reply(FromPid, {error, unknown_key})
            end,
            loop(LoopState);

        {request, FromPid, {wait_for, Keys}} ->
            case resolve_keys(PassMap, Keys) of
                {unknown_key, Key} ->
                    reply(FromPid, {unknown_key, Key}),
                    loop(LoopState);
                PassList ->
                    {NewLoopState, Wait} = lists:foldl(fun ({_, Pass}, {LS, S}) ->
                                                              maybe_start_pass(FromPid, Pass, LS, S)
                                                      end, {LoopState, []}, PassList),
                    reply(FromPid, {wait, Wait}),
                    loop(NewLoopState)
            end;

        {cast, FromPid, {ready, Pass, Variables}} ->
            tep_log:info("done: ~s", [Pass]),
            {FromPid, Waiting} = dict:fetch(Pass, Running),
            lists:foreach(fun (WaitingPid) -> reply(WaitingPid, {done, Pass}) end, Waiting),
            NewCache   = dict:merge(fun (Key, _V1, V2) ->
					   	                    tep_log:debug("ctx var merge conflict ~p", [Key]),
											V2
									end, Cache, Variables),
            NewRunning = dict:erase(Pass, Running),
			NewDone    = gb_sets:insert(Pass, Done),
            loop(LoopState#st{cache = NewCache, done = NewDone, running = NewRunning});

        {'EXIT', _DeadPid, normal} ->
            loop(LoopState);

        {'EXIT', DeadPid, {pass_failed, FailedPass}} ->
            shutdown(LoopState, FailedPass, DeadPid);

        Other ->
            tep_log:debug("ctx other ~p", [Other])
    end.

shutdown(#st{running = Running}, FailedPass, FailedPid) ->
   RList   = dict:to_list(Running),
   Workers = [Pid || {_Name, {Pid, _}} <- RList, Pid /= FailedPid],
   Others  = [Pid || {_Name, {_, Waiting}} <- RList,
                     Pid <- Waiting, not lists:member(Pid, Workers)],

   {FailedPid, WaitingForFailed} = dict:fetch(FailedPass, Running),
   [reply(P, {failed, FailedPass}) || P <- WaitingForFailed, not lists:member(P, Others)],

   lists:foreach(fun (P) -> reply(P, wait_shutdown) end, Others),
   shutdown_loop(Workers, Others, Running).

shutdown_loop([], _Others, _Running) -> ok;
shutdown_loop(Workers, Others, Running) ->
    receive
        {'EXIT', Pid, {pass_failed, PassName}} ->
            {Pid, Waiting} = dict:fetch(PassName, Running),
            [reply(P, {failed, PassName}) || P <- Waiting, not lists:member(P, Others)],
            shutdown_loop(lists:delete(Pid, Workers), Others, Running);
        OtherMsg ->
            tep_log:debug("ctx shutdown other ~p", [OtherMsg])
    end.

maybe_start_pass(FromPid, Pass = #pass{name = PassName}, State, CallerWaitList) ->
    case dict:find(PassName, State#st.running) of
        {ok, {WorkerPid, WaitList}} ->
            %% pass is already running, add the caller to it's wait list
            NewRunning = dict:store(PassName, {WorkerPid, [FromPid | WaitList]}, State#st.running),
            {State#st{running = NewRunning}, [PassName | CallerWaitList]};
        error ->
			case gb_sets:is_member(PassName, State#st.done) of
				false ->
					%% pass has not been run yet, the caller needs to wait
					WorkerPid = spawn_link(tep_pass, worker, [Pass, self()]),
					NewRunning = dict:store(PassName, {WorkerPid, [FromPid]}, State#st.running),
					{State#st{running = NewRunning}, [PassName | CallerWaitList]};
				true ->
					%% pass already did it's job so it's not added to the wait list
					{State, CallerWaitList}
			end
    end.

resolve_keys(PassMap, Keys) ->
    try
        lists:foldl(fun (Key, Acc) ->
                          [First | Rest] = tep_pass:split_name(Key),
                          case descending_lookup(PassMap, [First], Rest) of
                              error  -> throw({unknown, Key});
                              Assocs -> lists:keymerge(1, Assocs, Acc)
                          end
                    end, [], Keys)
    catch
        throw:{unknown, Key} ->
            {unknown_key, Key}
    end.

descending_lookup(PassMap, Prefix, KeyRest) ->
    Matches = lists:filter(fun ({Name, _}) -> lists:prefix(Prefix, Name) end, PassMap),
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
