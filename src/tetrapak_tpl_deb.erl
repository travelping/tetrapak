%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_tpl_deb).
-export([pkg_type/0, create_package/2]).

-include("tetrapak.hrl").

%% ------------------------------------------------------------
%% -- API

pkg_type() -> debian.

create_package(Project, Job) ->
  tep_file:with_temp_dir(fun (PkgDir) -> make_deb(Project, Job, PkgDir) end).

%% ------------------------------------------------------------
%% -- Implementation

file_target(_Pkg, "bin" ++ _) -> "usr/";
file_target(Pkg, _Path)       -> "usr/lib/erlang/lib/" ++ Pkg.

make_deb(#tep_project{name = Name, vsn = Vsn, desc = Desc, deps = Deps},
         #tep_job{files = PackageFiles, source_dir = SourceDir,
                  output_dir = OutDir, template_dir = TemplateDir},
         PkgDir) ->
  Pkg = tep_util:f("~s-~s", [Name, Vsn]),
  PkgName = tep_util:f("erlang-~s", [Name]),
  Arch = "all",
  DebianName = no_underscores(PkgName),

  tep_log:debug("creating debian-binary"),
  file:write_file(filename:join(PkgDir, "debian-binary"), <<"2.0\n">>),

  tep_log:debug("creating data.tar.gz"),
  DataDir = filename:join(PkgDir, "data"), tep_file:mkdir(DataDir),
  lists:foreach(fun (P) ->
        File = tep_file:rebase_filename(P, SourceDir, ""),
        Target = filename:join([DataDir, file_target(Pkg, File), File]),
        tep_file:copy(P, Target)
    end, PackageFiles),
  tep_file:make_tarball(filename:join(PkgDir, "data.tar.gz"), ".", DataDir, ".*"),

  tep_log:debug("creating control.tar.gz"),
  ControlDir = filename:join(PkgDir, "control"),

  %% copy control files with varsubst applied
  DepString = lists:map(fun (S) -> no_underscores(tep_util:f(", erlang-~s", [S])) end, Deps),
  tep_file:copy(TemplateDir, ControlDir),
  tep_file:walk(fun (P, _) ->
        tep_file:varsubst([{"name", DebianName}, {"version", Vsn},
                           {"appname", Name}, {"appdeps", DepString}, {"desc", Desc}],
                          P, P)
    end, [], ControlDir),

  %% generate md5 sums
  Md5_File = filename:join(ControlDir, "md5sums"),
  {ok, Md5} = file:open(Md5_File, [write]),
  tep_file:walk(fun (P, _) ->
        io:format(Md5, "~s ~s~n", [tep_file:md5sum(P),
				                   tep_file:rebase_filename(P, DataDir, "")])
    end, [], DataDir),
  file:close(Md5),

  %% tarballize control files
  tep_file:make_tarball(filename:join(PkgDir, "control.tar.gz"), ".", ControlDir, ".*"),

  DebFile = filename:join(OutDir, tep_util:f("~s_~s_~s.deb", [DebianName, Vsn, Arch])),
  make_ar(DebFile, PkgDir, ["debian-binary", "control.tar.gz", "data.tar.gz"]),
  {ok, DebFile}.

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

no_underscores(S) -> re:replace(S, "_", "-", [{return, list}]).
