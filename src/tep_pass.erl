%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_pass).
-include("tetrapak.hrl").

%% pass behaviour functions
-export([behaviour_info/1]).
-export([fail/1, fail/2, require/1, require_all/1]).
%% scheduler_api
-export([run_passes/3]).
%% scheduler gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% misc
-export([find_passes/0]).

%% ------------------------------------------------------------
%% -- tep_pass API
behaviour_info(exports) ->
    [{pass_options, 0}, {pass_run, 2}].

%% ------------------------------------------------------------
%% -- scheduler
-record(sched, {
    done    = gb_sets:new() :: set(),
    running = gb_sets:new() :: set(),
    waiting = []       :: list({pid(), [string()]}),
    project            :: tuple(),
    caller             :: pid(),
    want    = []       :: list(string()),
    map                :: dict(),
    config             :: dict()
}).

run_passes(PassMap, Project, PassNames) ->
    {ok, Sched} = gen_server:start(?MODULE, [PassMap, Project], []),
    case gen_server:call(Sched, {run_passes, PassNames}) of
        {ok, done} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

init([PassMap, Project]) ->
    State = #sched{map = PassMap, project = Project},
    {ok, State}.

handle_call({run_passes, PassNames}, Caller, State = #sched{caller = undefined}) ->
    tep_log:debug("sched: run_passes ~p", [PassNames]),
    case lookup_all(PassNames, State#sched.map) of
        {error, Error} ->
            {reply, {error, Error}, State};
        Names ->
            NewState = lists:foldl(fun do_start_pass/2, State, Names),
            {noreply, NewState#sched{caller = Caller, want = Names}}
    end;

handle_call({wait_for, PassNames}, WorkerPid, State = #sched{waiting = Waiting}) ->
    case lookup_all(PassNames, State#sched.map) of
        {error, _Error} ->
            {stop, start_error, State};
        Required ->
            case lists:filter(fun (P) -> not is_done(P, State) end, Required) of
                []      ->
                    {reply, go_on, State};
                NotDone ->
                    NeedStart  = lists:filter(fun (P) -> not is_running(P, State) end, NotDone),
                    NewState   = lists:foldl(fun do_start_pass/2, State, NeedStart),
                    NewWaiting = [{WorkerPid, NotDone} | Waiting],
                    {noreply, NewState#sched{waiting = NewWaiting}}
            end
    end;

handle_call({done, PassName}, WorkerPid, State = #sched{waiting = Waiting}) ->
    tep_log:info("pass '~s' finished", [PassName]),
    gen_server:reply(WorkerPid, ok),

    NewWaiting = lists:foldl(fun (Tup, Acc) -> do_mgr_done(PassName, WorkerPid, Tup, Acc) end, [], Waiting),
    NewState = State#sched{waiting = NewWaiting,
                           running = gb_sets:del_element(PassName, State#sched.running),
                           done    = gb_sets:add_element(PassName, State#sched.done)},

    case lists:all(fun (PName) -> is_done(PName, NewState) end, State#sched.want) of
        true  ->
            %% all requested passes are done, return
            gen_server:reply(State#sched.caller, {ok, done}),
            {stop, normal, State};
        false ->
            {noreply, NewState}
    end;

handle_call({fail, PassName, Message}, _WorkerPid, State) ->
    tep_log:warn("pass '~s' failed", [PassName]),
    gen_server:reply(State#sched.caller, {error, {fail, PassName, Message}}),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

do_mgr_done(PassName, {DonePid, _Ctok}, {Worker, WaitFor}, Acc) ->
    % remove PassName from the waiting-for list
    % of any worker, unblocking the worker if possible
    case Worker of
        {DonePid, _Ctok2} ->
            Acc;
        _ ->
            NewWaiting = lists:keydelete(PassName, #pass.fullname, WaitFor),
            case NewWaiting of
                []    -> gen_server:reply(Worker, go_on);
                _List -> ok
            end,
            [{Worker, NewWaiting} | Acc]
    end.

do_start_pass(Pass = #pass{}, State = #sched{project = Project, running = Running}) ->
    spawn_worker(Pass, Project, []),
    NewRunning = gb_sets:add_element(Pass#pass.fullname, Running),
    State#sched{running = NewRunning}.

is_done(#pass{fullname = Name}, State) ->
    is_done(Name, State);
is_done(Name, State) ->
    gb_sets:is_element(Name, State#sched.done).

is_running(#pass{fullname = Name}, State) ->
    gb_sets:is_element(Name, State#sched.running).

%% unused callbacks
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------
%% -- worker API
spawn_worker(Pass, Project, Options) ->
    Sched = self(),
    spawn_link(fun () -> init_worker(Pass, Sched, Project, Options) end).

init_worker(Pass = #pass{fullname = Name, module = PassModule}, Scheduler, Project, Options) ->
    tep_log:debug("worker: pass ~s starting", [Name]),
    put('$__tep_pass_scheduler', Scheduler),
    try
        PassModule:pass_run({Pass#pass.group, Pass#pass.name}, Project, Options),
        gen_server:call(Scheduler, {done, Name})
    catch
        throw:{'$tep_pass_fail', Message} ->
            gen_server:call(Scheduler, {fail, Name, Message});
        exit:{normal, _} ->
            tep_log:debug("worker: '~s' exit:normal", [Name]);
        Class:Reason ->
            tep_log:warn("pass ~s crashed: ~p:~p ~p", [Name, Class, Reason, erlang:get_stacktrace()]),
            gen_server:call(Scheduler, {fail, Name, "crashed"})
    end.

require(Pass) ->
    require_all([Pass]).

require_all(Passes) when is_list(Passes) ->
    do_require(get('$__tep_pass_scheduler'), Passes).

do_require(Scheduler, Passes) ->
    Plist = lists:map(fun str/1, Passes),
    tep_log:debug("worker: require ~p", [Plist]),
    gen_server:call(Scheduler, {wait_for, Plist}).

fail(Reason) ->
    fail(Reason, []).
fail(Fmt, Args) ->
    throw({'$tep_pass_fail', tep_util:f(Fmt, Args)}).

%% ------------------------------------------------------------
%% -- Helpers
find_passes() ->
    find_passes([code:lib_dir(tetrapak, ebin)]).
find_passes(Directories) ->
    lists:foldl(fun (Dir, OuterModAcc) ->
                   tep_log:debug("checking for pass modules in ~s", [Dir]),
                   tep_file:walk(fun (File, ModAcc) ->
                                    case is_pass_module(File) of
                                        false              -> ModAcc;
                                        {Module, PassDefs} -> store_passdefs(Module, PassDefs, ModAcc)
                                    end
                                  end, OuterModAcc, Dir)
                end, dict:new(), Directories).

is_pass_module(Mfile) ->
    case filename:extension(Mfile) of
        ".beam" ->
            {ok, {ModuleName, Chunks}} = beam_lib:chunks(Mfile, [attributes]),
            Attributes = proplists:get_value(attributes, Chunks, []),
            IsPass = lists:member(?MODULE, proplists:get_value(behaviour, Attributes, [])) orelse
                     lists:member(?MODULE, proplists:get_value(behavior, Attributes, [])),

            if
                IsPass ->
                    case proplists:get_value(passinfo, Attributes) of
                        undefined -> false;
                        InfoList  -> {ModuleName, lists:flatten(InfoList)}
                    end;
                true ->
                    false
            end;
        _ ->
            false
    end.

store_passdefs(Module, List, Universe) ->
    Store = fun ({GroupName, Passes}, OuterAcc) ->
               lists:foldl(fun ({PassName, Desc}, Acc) ->
                              FullName = atom_to_list(GroupName)
                                         ++ ":" ++ atom_to_list(PassName),
                              Pass   = #pass{name   = PassName,
                                             group  = GroupName,
                                             module = Module,
                                             fullname = FullName,
                                             description = Desc},
                              PDict  = dict:from_list([{str(PassName), Pass}]),
                              Insert = fun (Existing) ->
                                          dict:merge(fun (_Old, New) -> New end, Existing, PDict)
                                       end,
                              dict:update(str(GroupName), Insert, PDict, Acc)
                           end, OuterAcc, Passes)
            end,
    lists:foldl(Store, Universe, List).

lookup(Name, Universe) ->
    case re:split(Name, ":", [{return, list}]) of
        [Group] ->
            case dict:find(Group, Universe) of
                {ok, PassDict} ->
                    {ok, #pass_group{name    = list_to_existing_atom(Group),
                                     members = lists:map(fun ({_K, Pass}) -> Pass end, dict:to_list(PassDict))}};
                error ->
                    {error, unknown_group}
            end;
        [Group, PassName] ->
            case dict:find(Group, Universe) of
                {ok, PassDict} ->
                    case dict:find(PassName, PassDict) of
                        error      -> {error, unknown_pass};
                        Ok         -> Ok
                    end;
                error ->
                    {error, unknown_group}
            end
    end.


lookup_all(Names, Universe) ->
    try
        lists:foldl(fun (Name, Acc) ->
                        case lookup(Name, Universe) of
                            {ok, #pass_group{members = Members}} ->
                                Members ++ Acc;
                            {ok, Pass} ->
                                [Pass | Acc];
                            {error, E} ->
                                throw({lookup_error, E})
                        end
                    end, [], Names)
    catch
        throw:{lookup_error, E} ->
            {error, E}
    end.

str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
str(Lis)                     -> Lis.
