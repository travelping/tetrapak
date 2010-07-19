%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_util).
-export([f/1, f/2, find_module/1, run_proc/2, run_proc/3]).
-export([basename/1, filename_rebase/3]).
-export([with_temp_dir/1, dir_contents/2, delete_any/1,
         copy_dir_contents/2, copy_dir_contents/3]).

f(Str) -> f(Str,[]).
f(Str, Args) -> lists:flatten(io_lib:format(Str, Args)).

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

temp_file_name() ->
  {A,B,C} = now(),
  f("/tmp/tetrapak-tmp-~p-~p-~p", [A,B,C]).

copy_dir_contents(Dir, NewDir) ->
  copy_dir_contents(Dir, NewDir, ".*").
copy_dir_contents(Dir, NewDir, Mask) ->
  CpFun = fun(F, _) -> safe_copy(F, filename_rebase(F, Dir, NewDir)) end,
  filelib:fold_files(Dir, Mask, true, CpFun, 1).

safe_copy(From, To) ->
  ok = filelib:ensure_dir(To),  
  case file:copy(From, To) of 
    {ok, _} -> ok;
    {error, Reason} -> throw({file_copy_error, Reason})
  end.

basename(Filename) ->
  Abs = filename:absname(Filename),
  case filename:basename(Abs) of
    "." -> filename:dirname(Abs);
    Other -> Other
  end.

filename_rebase(FName, FromDir, ToDir) ->
  FromDirPath = filename:split(FromDir),
  FPath = filename:split(FName), 
  case lists:prefix(FromDirPath, FPath) of
    true -> 
      RP = FPath -- FromDirPath,
      filename:join([ToDir|RP]);
    false ->
      exit(bad_filename)
  end.

dir_contents(Dir, Mask) ->
  AddL = fun (F, Acc) -> [F|Acc] end,
  filelib:fold_files(Dir, Mask, true, AddL, []).

delete_any(File) ->
  case filelib:is_dir(File) of
    true -> 
      {ok, DList} = file:list_dir(File),
      lists:foreach(fun (F) -> delete_any(filename:join(File, F)) end, DList),
      file:del_dir(File);
    false ->
      file:delete(File)
  end.

with_temp_dir(DoSomething) ->
  Temp = temp_file_name(),
  file:make_dir(Temp),
  try DoSomething(Temp) 
  after 
    tep_log:info("deleting directory ~s", [Temp]),
    delete_any(Temp)
  end.
