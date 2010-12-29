%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak).
-behaviour(gen_server).

%% API
-export([start/1, all_commands/1, run/2, run/3]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("tetrapak.hrl").

increase_version(#tep_project{vsn = Version, app_file = AppFile}) ->
    VComps = re:split(Version, "\\.", [{return, list}]),
    NewLast = integer_to_list(list_to_integer(lists:last(VComps)) + 1),
    NewVL = lists:append(lists:sublist(VComps, length(VComps) - 1), [NewLast]),
    NewV = string:join(NewVL, "."),
    tep_log:info("increasing version in project app file from ~s to ~s", [Version, NewV]),
    {ok, AppContents} = file:read_file(AppFile),
    NewContents = re:replace(AppContents,
                             "(\\{\\s*vsn\\s*,\\s*)(\"[^\"]+\")(\\s*\\})",
                             "\\1\"" ++ NewV ++ "\"\\3",
                             [{return, binary}]),
    file:write_file(AppFile, NewContents).

%% ------------------------------------------------------------
%% -- API
start(Dir) ->
    case tep_config:project_info(Dir) of
        {error, E} ->
            {error, E};
        {ok, Project} ->
            gen_server:start(?MODULE, [Project], [])
    end.

all_commands(Server) ->
    gen_server:call(Server, all_commands).

run(Server, PassCmd) ->
    run(Server, PassCmd, []).
run(Server, PassCmd, Options) ->
    gen_server:call(Server, {run_command, PassCmd, Options}).

%% ------------------------------------------------------------
%% -- gen_server callbacks
-record(tpk, {passmap, project}).

init([Project]) ->
    Map = tep_pass:find_passes(),
    State = #tpk{project = Project, passmap = Map},
    {ok, State}.

handle_call(all_commands, _From, State = #tpk{passmap = PDict}) ->
    Dict  = dict:to_list(PDict),
    CList = lists:foldl(fun ({_Group, Passes}, Pl) ->
                           PNames = [{Pass#pass.fullname, Pass#pass.description} || {_, Pass} <- dict:to_list(Passes)],
                           PNames ++ Pl
                        end, [], Dict),
    {reply, CList, State};

handle_call({run_command, PassCmd, Options}, _From, State = #tpk{project = Project, passmap = PMap}) ->
    {ok, Sched} = tep_pass:start_sched(PMap, Project),
    Reply = tep_pass:run(Sched, PassCmd),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% unused callbacks
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
