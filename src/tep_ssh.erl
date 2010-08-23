%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_ssh).
-export([login/3, run/2, close/1]).

%% internal
-export([init/1]).
-define(TIMEOUT, 200).
-define(EXEC_TIMEOUT, 200).

%% ------------------------------------------------------------ 
%% -- API

login(Host, Port, Options) ->
  case ssh:connect(Host, 22, Options) of
    {ok, CM} ->
      tep_log:debug("ssh: connected to ~s:~p", [Host, Port]),
      Pid = spawn_link(?MODULE, init, [CM]),
      {ok, Pid};
    Error ->
      Error
  end.

run(SessionPid, CmdLine) ->
  SessionPid ! {self(), run, CmdLine},
  receive
    {SessionPid, proc_finish, Return} -> Return
  end.

close(SessionPid) ->
  SessionPid ! terminate.

%% ------------------------------------------------------------ 
%% -- Implementation

init(CM) ->
  session_loop(CM).
 
session_loop(CM) ->
  receive
    {From, run, CmdLine} ->
      Return = run_remote(CM, CmdLine), 
      From ! {self(), proc_finish, Return},
      session_loop(CM);
    terminate -> 
      tep_log:debug("ssh: closing session"),
      ssh:close(CM);
    Msg ->
      tep_log:warn("ssh: session got unknown: ~p", [Msg]),
      session_loop(CM)
  end.

run_remote(CM, CmdLine) ->
  case ssh_connection:session_channel(CM, ?TIMEOUT) of
    {ok, Channel} ->
      tep_log:debug("ssh: running remote command: ~s", [CmdLine]),
      case ssh_connection:exec(CM, Channel, CmdLine ++ "\n", ?EXEC_TIMEOUT) of
        success -> 
          Result = run_loop(CM, Channel, <<>>, 0),
          ssh_connection:close(CM, Channel),
          Result;
        failure -> 
          tep_log:warn("ssh: could not send exec for command: ~s", [CmdLine]),
          {error, exec_fail}
      end;
    Error ->
      tep_log:warn("ssh: could not open channel"),
      Error
  end.

run_loop(CM, Channel, Buf, ExitCode) ->
  receive 
    {ssh_cm, CM, {data, Channel, _Type, Data}} ->
      run_loop(CM, Channel, <<Buf/binary, Data/binary>>, ExitCode);
    {ssh_cm, CM, {eof, Channel}} ->
      tep_log:debug("ssh: got eof"),
      run_loop(CM, Channel, Buf, ExitCode);
    {ssh_cm, CM, {exit_status, Channel, Sig}} ->
      tep_log:debug("ssh: got exit code ~p", [Sig]),
      run_loop(CM, Channel, Buf, Sig);
    {ssh_cm, CM, {closed, Channel}} ->
      tep_log:debug("ssh: channel closed"),
      {ok, ExitCode, Buf}
  end.
