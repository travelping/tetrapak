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
-export([fail/1, fail/2, require/1]).
%% scheduler_api
-export([start_sched/2, run/2]).
%% scheduler gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% misc
-export([find_passes/0]).

%% ------------------------------------------------------------
%% -- tep_pass API
behaviour_info(exports) ->
    [{pass_options, 0}, {pass_run, 2}].

%% ------------------------------------------------------------
%% -- manager
-record(sched, {
    done    = sets:new() :: set(),
    running = sets:new() :: set(),
    waiting = []       :: list({pid(), [atom()]}),
    project            :: tuple(),
    caller             :: pid(),
    map                :: dict(),
    config             :: dict()
}).

start_sched(PassMap, Project) ->
    gen_server:start(?MODULE, [PassMap, Project], []).

run(Sched, PassName) ->
    case gen_server:call(Sched, {run_pass, PassName}) of
        {ok, done} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

init([PassMap, Project]) ->
    State = #sched{map = PassMap,
                   project = Project},
    {ok, State}.

handle_call({run_pass, PassName}, Caller, State = #sched{caller = undefined}) ->
    tep_log:debug("sched: run_pass ~p", [PassName]),
    case do_start_worker(PassName, State) of
        {error, unknown_pass} ->
            {reply, {error, unknown_pass}, State};
        {ok, NewState} ->
            {noreply, NewState#sched{caller = Caller}}
    end;
handle_call({run_pass, _Name}, _Caller, State = #sched{caller = _SomePid}) ->
    {reply, {error, already_called}, State};

handle_call({wait_for, PassNames}, WorkerPid, State = #sched{waiting = Waiting}) ->
    NeedStart = lists:filter(fun (Name) -> not is_active(Name, State) end, PassNames),
    case NeedStart of
        [] ->
            {reply, go_on, State};
        _  ->
            try
                NewState =
                   lists:foldl(fun (Name, SubST) ->
                                   case do_start_worker(Name, SubST) of
                                       {ok, NewSubState} ->
                                           NewSubState;
                                       {error, _Error} ->
                                           throw({start_error, SubST})
                                   end
                               end,
                               State, NeedStart),
                NewWaiting = [{WorkerPid, PassNames} | Waiting],
                tep_log:debug("waiting ~p ==> ~p", [Waiting, NewWaiting]),
                {noreply, NewState#sched{waiting = NewWaiting}}
            catch
                throw:{start_error, State} ->
                    tep_log:debug("sched: start_error"),
                    {reply, start_error, State}
            end
    end;

handle_call({done, PassName}, WorkerPid, State = #sched{waiting = Waiting, done = DoneList}) ->
    tep_log:info("pass '~s' finished", [PassName]),
    NewWaiting = lists:foldl(fun (Tup, Acc) ->
                                 do_mgr_done(PassName, WorkerPid, Tup, Acc)
                             end,
                             [], Waiting),
    tep_log:debug("waiting ~p ==> ~p", [Waiting, NewWaiting]),
    case NewWaiting of
        [] ->
            gen_server:reply(WorkerPid, ok),
            gen_server:reply(State#sched.caller, {ok, done}),
            {stop, normal, State};
        _  ->
            NewState = State#sched{waiting = NewWaiting,
                                   running = sets:del_element(PassName, State#sched.running),
                                   done    = sets:add_element(PassName, DoneList)},
            {noreply, NewState}
    end;

handle_call({fail, PassName, Message}, _WorkerPid, State) ->
    tep_log:warn("pass '~s' failed", [PassName]),
    gen_server:reply(State#sched.caller, {error, {fail, PassName, Message}}),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

is_active(PassName, #sched{running = Running, done = Done}) ->
    sets:is_element(PassName, Done) orelse sets:is_element(PassName, Running).

do_mgr_done(PassName, {DonePid, _Ctok}, {Worker, WaitFor}, Acc) ->
    % remove PassName from the waiting-for list
    % of any worker, unblocking the worker if possible
    case Worker of
        {DonePid, _Ctok2} ->
            Acc;
        _ ->
            NewWaiting = lists:delete(PassName, WaitFor),
            case NewWaiting of
                [] -> gen_server:reply(Worker, go_on);
                _  -> ok
            end,
            [{Worker, NewWaiting} | Acc]
    end.

do_start_worker(PassName, State = #sched{map = Map, project = Project}) ->
    case dict:find(PassName, Map) of
        error ->
            {error, unknown_pass};
        {ok, PassMod} ->
            Sched = self(),
            Pid   = spawn_link(fun () -> init_worker(Sched, PassName, PassMod, Project, []) end),
            tep_log:debug("sched: spawned worker ~p", [Pid]),
            {ok, State#sched{running = sets:add_element(PassName, State#sched.running)}}
    end.

%% unused callbacks
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------
%% -- worker API
init_worker(Scheduler, PassName, PassModule, Project, Options) ->
    tep_log:debug("worker: pass ~s starting", [PassName]),
    put('$__tep_pass_scheduler', Scheduler),
    try
        PassModule:pass_run(Project, Options),
        tep_log:debug("worker: pass ~s finished", [PassName]),
        gen_server:call(Scheduler, {done, PassName})
    catch
        throw:{'$tep_pass_fail', Message} ->
            gen_server:call(Scheduler, {fail, PassName, Message});
        exit:{normal, _} ->
            tep_log:debug("worker: exit:normal", []);
        Class:Reason ->
            tep_log:warn("pass ~s crashed: ~p:~p ~p", [PassName, Class, Reason, erlang:get_stacktrace()]),
            gen_server:call(Scheduler, {fail, PassName, "crashed"})
    end.

require(Pass) when is_atom(Pass) ->
    require([Pass]);
require(Passes) when is_list(Passes) ->
    require(get('$__tep_pass_scheduler'), Passes).

require(Scheduler, Passes) ->
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
                                        false          -> ModAcc;
                                        {Name, Module} -> dict:store(Name, Module, ModAcc)
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
                    case proplists:get_value(pass_name, Attributes) of
                        undefined -> {ModuleName, ModuleName};
                        Name      -> {atom_to_list(lists:last(Name)), ModuleName}
                    end;
                true ->
                    false
            end;
        _ ->
            false
    end.

str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
str(Lis) -> Lis.
