%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_file).
-export([basename/1, rebase_filename/3]).
-export([temp_name/0, with_temp_dir/1, dir_contents/2]).
-export([copy/2, copy/3, delete/1, delete/2, walk/3, walk/4]).
-export([make_tarball/4]).

basename(Filename) ->
  Abs = filename:absname(Filename),
  case filename:basename(Abs) of
    "." -> filename:dirname(Abs);
    Other -> Other
  end.

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

temp_name() ->
  {A,B,C} = now(),
  tep_util:f("/tmp/tetrapak-tmp-~p-~p-~p", [A,B,C]).

with_temp_dir(DoSomething) ->
  Temp = temp_name(),
  file:make_dir(Temp),
  try DoSomething(Temp) 
  after 
    tep_log:info("deleting directory ~s", [Temp]),
    delete(Temp)
  end.

dir_contents(Dir, Mask) ->
  AddL = fun (F, Acc) -> 
      case tep_util:match(Mask, F) of
        true -> [F|Acc];
        false -> Acc
      end
  end,
  walk(AddL, [], Dir).

dir_empty(Dir) ->
  case file:list_dir(Dir) of
    {ok, []} -> true;
    {ok, _} -> false;
    Error -> Error
  end.

copy(From, To) -> copy(".*", From, To).
copy(Mask, From, To) ->
  CP = fun (F, _) -> 
      case tep_util:match(Mask, F) of
       true ->
         T = rebase_filename(F, From, To),
         tep_log:debug("copy ~s -> ~s", [F, T]),
         ok = filelib:ensure_dir(T),  
         case file:copy(F, T) of 
           {ok, _} -> ok;
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
      tep_log:debug("delete ~s", [Path]),
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
      case {Queue, dir_empty(Path)} of
        {[], true} -> Fun(Path, Acc);
        {_, _} -> 
          {ok, List} = file:list_dir(Path), 
          AddPaths = lists:map(fun (Name) -> {walk, filename:join(Path, Name)} end, List),
          [Next|Rest] = case DirOpt of
            no_dir -> AddPaths ++ Queue;
            dir_first -> [{nowalk, Path}|AddPaths] ++ Queue;
            dir_last -> AddPaths ++ [{nowalk, Path}|Queue]
          end,
          walk(Fun, Next, Acc, Rest, DirOpt)
      end;
    {_, _} -> 
      case Queue of 
        [] -> Fun(Path,Acc);
        [Next|Rest] -> walk(Fun, Next, Fun(Path, Acc), Rest, DirOpt)
      end
  end.

make_tarball(Outfile, Root, Dir, Mask) ->
  Files = lists:map(fun filename:absname/1, dir_contents(Dir, Mask)),
  XFEsc = fun (P) -> re:replace(P, "([,])", "\\\\\\1", [global, {return, list}]) end,
  XForm = tep_util:f("s,~s,~s,", [XFEsc(filename:absname(Dir)), XFEsc(Root)]),
  tep_util:run("tar", ["--create", "--directory", Dir, "--file", Outfile, "--format=ustar", 
                       "--numeric-owner", "--owner=root", "--group=root", "--gzip",
                       "--totals", "--touch", "--absolute-names", "--transform", XForm | Files]).
