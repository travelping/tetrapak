%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_util).
-export([f/1, f/2, match/2, find_module/1]). 
-export([run_proc/2, run_proc/3]).

f(Str) -> f(Str,[]).
f(Str, Args) -> lists:flatten(io_lib:format(Str, Args)).

match(Re, String) -> 
  case re:run(String, Re, [{capture, none}]) of
    match -> true;
    nomatch -> false
  end.

find_module(Name) ->
  case catch Name:module_info() of
    {'EXIT', _} -> {error, bad_module};
    _ -> {ok, Name}
  end.

run_proc(Prog, Args) ->
  {ok, Cwd} = file:get_cwd(),
  run_proc(Prog, Args, Cwd).
run_proc(Prog, Args, Dir) ->
  tep_log:info("running ~s ~s", [Prog, string:join(Args, " ")]),
  case os:find_executable(Prog) of
    false -> {error, no_such_program};
    Cmd ->
      Port = erlang:open_port({spawn_executable, Cmd}, 
                              [{cd, Dir}, in, exit_status, stderr_to_stdout,
                               {args, Args}, {line, 400}]),
      display_output(Port)
  end. 

display_output(Port) ->
  receive
    {Port, {data, {Eol, Line}}} ->
      tep_log:output(case Eol of
          eol -> "~s~n";
          noeol -> "~s"
        end, [Line]),
      display_output(Port);
    {Port, {exit_status, 0}} -> 
      {exit, ok};
    {Port, {exit_status, _}} ->
      {exit, error};
    {Port, closed} ->
      {exit, error}
  end.
