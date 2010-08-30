%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_ssh).
-export([login/3, close/1]).
-export([run/2, ls/2, scp/3, mv/3, file_info/2, is_dir/2]).

%% internal
-export([init/1]).
-define(TIMEOUT, 200).
-define(EXEC_TIMEOUT, 200).

-include_lib("kernel/include/file.hrl").

%% ------------------------------------------------------------ 
%% -- API

login(Host, Port, Options) ->
  application:start(crypto),
  application:start(ssh),
  case ssh:connect(Host, 22, Options) of
    {ok, CM} ->
      tep_log:debug("ssh: connected to ~s:~p", [Host, Port]),
      Pid = spawn_link(?MODULE, init, [CM]),
      {ok, Pid};
    Error ->
      Error
  end.

call(Session, Type, Call) ->
  Session ! {self(), Type, Call},
  receive
    {Session, finish, Type, Return} -> Return
  end.

run(Session, CmdLine) ->
  call(Session, run, CmdLine).
ls(Session, RemoteDir) ->
  call(Session, sftp, {ls, RemoteDir}).
scp(Session, Local, Remote) ->
  call(Session, sftp, {scp, Local, Remote}).
mv(Session, From, To) ->
  call(Session, sftp, {mv, From, To}).
file_info(Session, Path) ->
  call(Session, sftp, {file_info, Path}).

is_dir(Session, RemotePath) ->
  case file_info(Session, RemotePath) of 
    {error, _} -> false;
    {ok, #file_info{type = Type}} -> Type == directory
  end.

close(SessionPid) ->
  SessionPid ! terminate.

%% ------------------------------------------------------------ 
%% -- Implementation

init(CM) ->
  case ssh_sftp:start_channel(CM) of
    {ok, Pid} ->
      tep_log:debug("ssh: sftp channel started"),
      session_loop(CM, Pid);
    Error ->
      tep_log:debug("ssh: could not start sftp channel: ~p", [Error])
  end.
 
session_loop(CM, SFTP) ->
  receive
    {From, run, CmdLine} ->
      Return = run_remote(CM, CmdLine), 
      From ! {self(), finish, run, Return},
      session_loop(CM, SFTP);
    {From, sftp, Cmd} ->
      Return = do_sftp_cmd(SFTP, Cmd), 
      From ! {self(), finish, sftp, Return},
      session_loop(CM, SFTP);
    terminate -> 
      tep_log:debug("ssh: closing session"),
      ssh:close(CM);
    Msg ->
      tep_log:warn("ssh: session got unknown: ~p", [Msg]),
      session_loop(CM, SFTP)
  end.

%% SFTP
do_sftp_cmd(Conn, {ls, RemoteDir}) ->
  tep_log:debug("ssh: sftp ls: ~p", [RemoteDir]),
  ssh_sftp:list_dir(Conn, RemoteDir, 1000);

do_sftp_cmd(Conn, {scp, Local, Remote}) ->
  tep_log:debug("ssh: sftp scp ~p -> ~p", [Local, Remote]),
  case ssh_sftp:open(Conn, Remote, [write,binary]) of
    {ok, RemHandle} ->
      case file:open(Local, [read,binary]) of
        {ok, LocalHandle} ->
          do_scp_copy(Conn, LocalHandle, RemHandle);
        {error, Error} ->
          tep_log:warn("ssh_scp: cannot open local file ~p: ~p", [Local, Error]),
          {error, Error} 
      end;
    {error, Error} ->
      tep_log:warn("ssh_scp: cannot open remote file ~p for writing, ~p", [Remote, Error]),
      {error, Error}
  end;

do_sftp_cmd(Conn, {file_info, Path}) ->
  tep_log:debug("ssh: sftp file_info ~p", [Path]),
  ssh_sftp:read_file_info(Conn, Path, 1000);

do_sftp_cmd(Conn, {mv, From, To}) ->
  tep_log:debug("ssh: sftp mv ~p -> ~p", [From, To]),
  ssh_sftp:rename(Conn, From, To);

do_sftp_cmd(_Conn, Cmd) ->
  tep_log:warn("ssh: unknown sftp command: ~p", [Cmd]),
  {error, bad_sftp_cmd}.

do_scp_copy(Conn, LocalHandle, RemoteHandle) ->
  tep_log:output("scp copy: "),
  do_scp_copy(Conn, LocalHandle, RemoteHandle, 0).
do_scp_copy(Conn, LocalHandle, RemoteHandle, Cursor) ->
  CopyBytes = 1000,
  case file:pread(LocalHandle, Cursor, CopyBytes) of
    {ok, Data} -> 
      ssh_sftp:write(Conn, RemoteHandle, Data),
      tep_log:output("."),
      do_scp_copy(Conn, LocalHandle, RemoteHandle, CopyBytes + Cursor);
    Msg ->
      tep_log:output("~n"),
      ssh_sftp:close(Conn, RemoteHandle),
      file:close(LocalHandle),
      if Msg /= eof ->
        tep_log:warn("ssh_scp: read error: ~p", Msg);
        true -> ok
      end,
      Msg
  end.

%% Shell commands
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
