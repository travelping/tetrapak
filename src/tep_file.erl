%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_file).
-export([size/1, basename/1, rebase_filename/3]).
-export([temp_name/0, temp_name/1, mkdir/1, with_temp_dir/1, 
         dir_contents/1, dir_contents/2, dir_contents/3,
         wildcard/2]).
-export([copy/2, copy/3, delete/1, delete/2, walk/3, walk/4]).
-export([make_tarball/4, make_tarball_from_files/4, varsubst/3]).

-include_lib("kernel/include/file.hrl").

basename(Filename) ->
  Abs = filename:absname(Filename),
  case filename:basename(Abs) of
    "." -> filename:dirname(Abs);
    Other -> Other
  end.

size(Filename) ->
  {ok, #file_info{size = Size}} = file:read_file_info(Filename), 
  Size.

rebase_filename(FName, FromDir, ToDir) ->
  FromDirPath = filename:split(FromDir),
  FPath = filename:split(FName), 
  case lists:prefix(FromDirPath, FPath) of
    true -> 
      RP = FPath -- FromDirPath,
      filename:join([ToDir|RP]);
    false ->
      exit(bad_filename)
  end.

temp_name() -> temp_name("/tmp").
temp_name(Dir) ->
  {A,B,C} = now(),
  Pid = re:replace(erlang:pid_to_list(self()), "<|>", "", [global, {return, list}]),
  filename:join(Dir, tep_util:f("tetrapak-tmp-~p-~p-~p-~s", [A,B,C,Pid])).

with_temp_dir(DoSomething) ->
  Temp = temp_name(),
  file:make_dir(Temp),
  try DoSomething(Temp) 
  after 
    tep_log:info("deleting directory ~s", [Temp]),
    delete(Temp)
  end.

dir_contents(Dir) -> dir_contents(Dir, ".*").
dir_contents(Dir, Mask) -> dir_contents(Dir, Mask, no_dir).
dir_contents(Dir, Mask, DirOpt) ->
  AddL = fun (F, Acc) -> 
      case tep_util:match(Mask, F) of
        true -> [F|Acc];
        false -> Acc
      end
  end,
  case filelib:is_dir(Dir) of
    true -> lists:reverse(walk(AddL, [], Dir, DirOpt));
    false -> []
  end.

wildcard(Dir, Wildcard) ->
  Files = filelib:wildcard(Wildcard, Dir),
  lists:map(fun filename:absname/1, Files).

mkdir(Path) ->
  filelib:ensure_dir(filename:join(Path, ".")).

copy(From, To) -> copy(".*", From, To).
copy(Mask, From, To) ->
  CP = fun (F, _) -> 
      case tep_util:match(Mask, F) of
       true ->
         T = rebase_filename(F, From, To),
         ok = filelib:ensure_dir(T), 
         case file:copy(F, T) of 
           {ok, _} -> 
             {ok, #file_info{mode = Mode}} = file:read_file_info(F),
             file:change_mode(T, Mode);
           {error, Reason} -> throw({file_copy_error, Reason})
         end;
       false -> nomatch
     end
 end,
 walk(CP, [], From, no_dir).

delete(Filename) -> delete(".*", Filename).
delete(Mask, Filename) -> 
  walk(fun (F, _) -> delete_if_match(Mask, F) end, [], Filename, dir_last).

delete_if_match(Mask, Path) ->
  case tep_util:match(Mask, Path) of
    true -> 
      case filelib:is_dir(Path) of
        true -> file:del_dir(Path);
        false -> file:delete(Path)
      end;
    false -> nomatch
  end.

walk(Fun, AccIn, Path) -> walk(Fun, AccIn, Path, no_dir).
walk(Fun, AccIn, Path, DirOpt) when (DirOpt == no_dir) or
                                    (DirOpt == dir_first) or
                                    (DirOpt == dir_last) ->
  walk(Fun, {walk, Path}, AccIn, [], DirOpt).
walk(Fun, {Walk, Path}, Acc, Queue, DirOpt) ->
  case {Walk, filelib:is_dir(Path)} of 
    {walk, true} ->
      {ok, List} = file:list_dir(Path), 
      AddPaths = lists:map(fun (Name) -> {walk, filename:join(Path, Name)} end, List),
      [Next|Rest] = case DirOpt of
        no_dir -> AddPaths ++ Queue;
        dir_first -> [{nowalk, Path}|AddPaths] ++ Queue;
        dir_last -> AddPaths ++ [{nowalk, Path}|Queue]
      end,
      walk(Fun, Next, Acc, Rest, DirOpt);
    {_, _} -> 
      case Queue of 
        [] -> Fun(Path,Acc);
        [Next|Rest] -> walk(Fun, Next, Fun(Path, Acc), Rest, DirOpt)
      end
  end.

make_tarball(Outfile, Root, Dir, Mask) ->
  Files = dir_contents(Dir, Mask, dir_first),
  make_tarball_from_files(Outfile, Root, Dir, Files).

make_tarball_from_files(Outfile, Root, Dir, Files) ->
  XFEsc = fun (P) -> re:replace(P, "([,])", "\\\\\\1", [global, {return, list}]) end,
  XForm = tep_util:f("s,~s,~s,", [XFEsc(filename:absname(Dir)), XFEsc(Root)]),
  tep_util:run("tar", ["--create", "--directory", Dir, "--file", Outfile, "--format=ustar", 
                       "--numeric-owner", "--owner=root", "--group=root", "--gzip", 
                       "--no-recursion", "--totals", "--touch", "--absolute-names", 
                       "--preserve-permissions", "--preserve-order",
                       "--transform", XForm | lists:map(fun filename:absname/1, Files)]).

varsubst(Variables, Infile, Outfile) ->
  tep_log:debug("varsubst: ~s -> ~s", [Infile, Outfile]),
  {ok, Content} = file:read_file(Infile),
  NewContent = tep_util:varsubst(Content, Variables),
  tep_log:debug("~p", [NewContent]),
  file:write_file(Outfile, NewContent).
