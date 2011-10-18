% Copyright 2010-2011, Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

-module(tetrapak_task_pkg_ipkg).
-behaviour(tetrapak_task).
-export([check/1, run/2]).

-include("tetrapak.hrl").

%% ------------------------------------------------------------
%% -- Task API
check("clean:dist") ->
    filelib:is_dir(tetrapak:config_path("package.outdir")).

run("clean:dist", _) ->
    tpk_file:delete(tetrapak:config_path("package.outdir"));

run("pkg:ipkg", _) ->
    case tetrapak:config("package.include_doc") of
        true  -> ReqDoc = ["doc"];
        false -> ReqDoc = []
    end,
    tetrapak:require_all(["build", "check" | ReqDoc]),
    file:make_dir(tetrapak:config_path("package.outdir")),
    tpk_file:with_temp_dir(fun make_ipkg/1).

%% ------------------------------------------------------------
%% -- Implementation
is_useless(Filename) ->
    Name = tpk_file:basename(Filename),
    tpk_util:match(".*~$", Name) %% editor backups
    or tpk_util:match("\\..*\\.sw[po]", Name) %% vim swap files
    or tpk_util:match("\\.#.*", Name) %% emacs swap files
    or tpk_util:match("erl_crash.dump", Name) %% you know those...
    or (in_dir("tetrapak", Filename) and tpk_util:match(?LOCAL_CACHE, Name)) %% tetrapak task cache
    or in_dir(tetrapak:config_path("test.ct.logdir"), Filename)
    or tpk_util:match("^(.*/)*\\.git(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.svn(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.hg(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.bzr(/.*)?$", Filename)
    or tpk_util:match(tetrapak:config("package.exclude"), Filename).

file_mode("bin" ++ _) -> 8#755;
file_mode(_Path)      -> 8#644.

make_ipkg(PkgDir) ->
    Name    = atom_to_list(tetrapak:get("config:appfile:name")),
    Vsn     = tetrapak:get("config:appfile:vsn"),
    PkgName = "erlang-" ++ Name,
    Arch    = tetrapak:config("package.architecture"),
    DebianName = no_underscores(PkgName),

    %% debian-binary
    file:write_file(filename:join(PkgDir, "debian-binary"), <<"2.0\n">>),

    %% data.tar.gz
    {ok, DataTarball} = tpk_file:tarball_create(filename:join(PkgDir, "data.tar.gz")),
    InstallDir = "usr/lib/erlang/lib/" ++ tpk_util:f("~s-~s/", [Name, Vsn]),
    tpk_file:tarball_mkdir_parents(DataTarball, InstallDir, [{owner, "root"}, {group, "root"}]),
    IsExcluded = fun (Path) ->
                         is_useless(Path) orelse
                         in_dir("tetrapak", Path) orelse
                         in_dir("src", Path) orelse
                         in_dir("include", Path) orelse
                         in_dir(tetrapak:config("edoc.outdir"), Path) orelse
                         in_dir(tetrapak:config("package.outdir"), Path) orelse
                         in_dir("debian", Path)
                 end,
    PackageFiles1 = copy_files(DataTarball, InstallDir, IsExcluded),

    %% symlink binaries
    BinDir = "usr/bin",
    tpk_file:tarball_mkdir(DataTarball, BinDir, [{owner, "root"}, {group, "root"}]),
    _PackageFiles2 = lists:foldl(fun (ScriptName, Acc) ->
                                        Original = filename:join(tetrapak:subdir("bin"), ScriptName),
                                        case filelib:is_regular(Original) and (not is_useless(Original)) of
                                            true ->
                                                Target = "/" ++ InstallDir ++ "bin/" ++ ScriptName,
                                                Link = BinDir ++ "/" ++ ScriptName,
                                                tpk_file:tarball_add_link(DataTarball, Link, Target, [{owner, "root"}, {group, "root"}]),
                                                [{Original, Link} | Acc];
                                            false ->
                                                Acc
                                        end
                                end, PackageFiles1, filelib:wildcard("*", tetrapak:subdir("bin"))),

    tpk_file:tarball_close(DataTarball),

    %% control.tar.gz
    {ok, ControlTarball} = tpk_file:tarball_create(filename:join(PkgDir, "control.tar.gz")),

    case tetrapak:config("package.use_erlrc") of
        false -> Template = "ipkg";
        true  -> Template = "ipkg_erlrc"
    end,
    copy_control_template(ControlTarball, Template, "./", []),
    tpk_file:tarball_close(ControlTarball),

    %% write the actual .deb as an AR archive (sic!)
    IpkgFile = filename:join(tetrapak:config_path("package.outdir"), tpk_util:f("~s_~s_~s.ipk", [DebianName, Vsn, Arch])),
	tar_ipkg_files(IpkgFile, PkgDir, ["debian-binary", "data.tar.gz", "control.tar.gz"]),
    io:format("package: ~s~n", [IpkgFile]).

in_dir(Dir, Path) ->
    lists:prefix(filename:split(Dir), filename:split(Path)).

debian_deps() ->
    AppDeps   = tetrapak:get("config:appfile:deps") ++ tetrapak:config("package.extra_apps", []),
    OtherDeps = tetrapak:config("package.deb.dependencies", []),
    lists:usort(OtherDeps ++ [no_underscores(tpk_util:f("erlang-~s", [S])) || S <- AppDeps, not in_erlang_base(S)]).

in_erlang_base(Application) ->
    lists:member(Application, tetrapak:config("package.deb.erlang_base_apps")).

copy_files(Tarball, InstallDir, IsExcludedFunction) ->
    tpk_file:walk(fun (P, Acc) ->
                          File = tpk_file:rebase_filename(P, tetrapak:dir(), ""),
                          Target = InstallDir ++ File,
                          case IsExcludedFunction(File) of
                              true ->
                                  Acc;
                              false ->
                                  case filelib:is_dir(P) of
                                      true ->
                                          case Target of
                                              InstallDir -> Acc;
                                              _ ->
                                                  tpk_file:tarball_mkdir(Tarball, Target, [{owner, "root"}, {group, "root"}]),
                                                  Acc
                                          end;
                                      false ->
                                          io:format("add ~s~n", [File]),
                                          Mode = file_mode(File),
                                          tpk_file:tarball_add_file(Tarball, P, Target, [dereference, {mode, Mode}, {owner, "root"}, {group, "root"}]),
                                          [{P, Target} | Acc]
                                  end
                          end
                  end, [], tetrapak:dir(), dir_first).

copy_control_template(Tarball, Template, ExtractDir, Variables) ->
    Pkg = "erlang-" ++ no_underscores(atom_to_list(tetrapak:get("config:appfile:name"))),
    case debian_deps() of
        []   -> DepString = "";
        Deps -> DepString = ", " ++ string:join(Deps, ", ")
    end,
    FileOptions = [{mode, 8#0744}, {owner, "root"}, {group, "root"}],
    TemplateDir = filename:join([code:priv_dir(tetrapak), "templates", Template]),
    tpk_file:walk(fun (CFile, _) ->
                          Target = tpk_file:rebase_filename(CFile, TemplateDir, ExtractDir),
                          case filelib:is_dir(CFile) of
                              true ->
                                  tpk_file:tarball_mkdir(Tarball, Target, FileOptions);
                              false ->
                                  Content =
                                    tpk_util:varsubst_file(CFile,
                                                           Variables ++
                                                           [{"name", Pkg},
                                                            {"version", tetrapak:get("config:appfile:vsn")},
                                                            {"arch", tetrapak:config("package.architecture")},
                                                            {"appname", tetrapak:get("config:appfile:name")},
                                                            {"appdeps", DepString},
                                                            {"section", tetrapak:config("package.deb.section")},
                                                            {"priority", tetrapak:config("package.deb.priority")},
                                                            {"maintainer", tetrapak:config("package.maintainer")},
                                                            {"desc", tetrapak:get("config:appfile:desc", "")}]),
                                  tpk_file:tarball_add_binary(Tarball, Target, Content, FileOptions)
                          end
                  end, [], TemplateDir, dir_first).

tar_ipkg_files(IpkgFile, Dir, Entries) ->
	{ok, Tarball} = tpk_file:tarball_create(IpkgFile),
	try
		lists:foreach(fun (Name) ->
							  File = filename:join(Dir, Name),
							  tpk_file:tarball_add_file(Tarball, File, filename:join("./", Name), [dereference, {mode, 8#644}, {owner, "root"}, {group, "root"}])
					  end, Entries)
	after
		tpk_file:tarball_close(Tarball)
	end.

no_underscores(S) -> re:replace(S, "_", "-", [global, {return, list}]).
