%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_tpl_deb).
-export([create_package/2]).

-include("tetrapak.hrl").

file_target(_Pkg, "/bin" ++ _) -> "usr/";
file_target(Pkg, _Path)        -> "usr/lib/erlang/lib/" ++ Pkg.

create_package(Project, SBPath) ->
  tep_file:with_temp_dir(fun (PkgDir) -> make_deb(Project, PkgDir, SBPath) end).

make_deb(#tep_project{name = Name, vsn = Vsn, desc = Desc, deps = Deps}, PkgDir, SBPath) ->
  PkgName = tep_util:f("erlang-~s", [Name]),
  tep_log:debug("creating debian-binary"),  
  file:write_file(filename:join(PkgDir, "debian-binary"), <<"2.0\n">>),
  
  tep_log:debug("creating data.tar.gz"),  
  DataDir = filename:join(PkgDir, "data"), tep_file:mkdir(DataDir),
  tep_file:walk(fun (P, _) ->
        File = tep_file:rebase_filename(P, SBPath, ""),
        Target = filename:join([DataDir, file_target(PkgName, File) ++ File]),
        tep_file:copy(P, Target)
    end, [], SBPath),
  tep_file:make_tarball(filename:join(PkgDir, "data.tar.gz"), ".", DataDir, ".*"),
  
  tep_log:debug("creating control.tar.gz"),
  ControlDir = filename:join(PkgDir, "control"), 
  DepString = lists:map(fun (S) -> tep_util:f(",erlang-~s", [S]) end, Deps),
  tep_file:copy(tetrapak:template_dir(?MODULE), ControlDir),
  tep_file:walk(fun (P, _) ->
        tep_file:varsubst([{"name", PkgName}, {"version", Vsn}, 
                           {"appdeps", DepString}, {"desc", Desc}], P, P)
    end, [], ControlDir),
  tep_file:make_tarball(filename:join(PkgDir, "control.tar.gz"), ".", ControlDir, ".*"),

  DebFile = PkgName ++ "-" ++ Vsn ++ ".deb",
  make_ar(DebFile, PkgDir, ["debian-binary", "control.tar.gz", "data.tar.gz"]).

make_ar(Outfile, Dir, Entries) ->
  {ok, ArFile} = file:open(Outfile, [write]),
  try 
    file:write(ArFile, <<"!<arch>\n">>),
    lists:foldl(fun (Name, Offset) ->
         File = filename:join(Dir, Name), 
         tep_log:debug("ar entry ~s ~s", [Name,File]),
         Size = tep_file:size(File),
         io:format(ArFile, "~-16s~-12s~-6s~-6s~-8s~-10B`\n",
           [Name, "1280174243", "0", "0", "000644", Size]),
         file:copy(File, ArFile),
         NewOffset = Offset + 60 + Size,
         if 
           NewOffset rem 2 =:= 0 -> NewOffset;
           true ->
             file:write(ArFile, <<"\n">>), % data section is 2-byte aligned
             NewOffset + 1
         end
      end, 0, Entries)
  after
    file:close(Outfile)
  end.
