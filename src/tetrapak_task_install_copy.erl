% Copyright 2012, Travelping GmbH <info@travelping.com>

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

-module(tetrapak_task_install_copy).
-behaviour(tetrapak_task).
-export([run/2]).

-include("tetrapak.hrl").

%% ------------------------------------------------------------
%% -- Task API
run("install:copy", _) ->
    LibDir = libdir(),
    case tpk_file:is_writable(LibDir) of
        true ->
            ReqDoc = ["doc" || tetrapak:config("package.include_doc")],
            tetrapak:require_all(["build", "check" | ReqDoc]),
            tpk_file:mkdir(LibDir),
            install_copy();
        false ->
            tetrapak:fail("Installation failed, permission denied: ~s~n", [LibDir])
    end.

%% ------------------------------------------------------------
%% -- Implementation

prefix() ->
    case init:get_argument(prefix) of
        {ok, [[Prefix]]} when is_list(Prefix) ->
            Prefix;
        _ -> ""
    end.

erlanglib_dir() ->
    case is_local() of
        true -> code:lib_dir();
        _    -> "/usr/lib/erlang/lib"
    end.

libdir() ->
    prefix() ++ erlanglib_dir().

is_local() ->
    (error =/= init:get_argument(local)).

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

install_copy() ->
    Name    = atom_to_list(tetrapak:get("config:appfile:name")),
    Vsn     = tetrapak:get("config:appfile:vsn"),

    AppErlangDir = tpk_util:f("/~s-~s/", [Name, Vsn]),
    InstallDir = libdir() ++ AppErlangDir,
    io:format("InstallDir: ~s~n", [InstallDir]),
    tpk_file:mkdir(InstallDir),
    IsExcluded = fun (Path) ->
                         is_useless(Path) orelse
                         in_dir("tetrapak", Path) orelse
             (in_dir(tetrapak:config("edoc.outdir"), Path) and not tetrapak:config("package.include_doc")) orelse
             in_dir(tetrapak:config("package.outdir"), Path) orelse
             in_dir("debian", Path)
                 end,
    PackageFiles1 = copy_files(InstallDir, IsExcluded),

    %% BinDir is symlink binaries
    {RelInstallDir, BinDir} =
        case is_local() of
            false -> {"../lib/erlang/lib", "usr/bin"};
            true  -> {"../lib", filename:join(code:root_dir(), "bin")}
        end,

    lists:foldl(fun (ScriptName, Acc) ->
        Original = filename:join(tetrapak:path("bin"), ScriptName),
        case filelib:is_regular(Original) and (not is_useless(Original)) of
            true ->
                tpk_file:mkdir(filename:join(prefix(), BinDir)),
                Target = prefix() ++ "/" ++ BinDir ++ "/" ++ ScriptName,
                Link = RelInstallDir ++ AppErlangDir ++ "bin/" ++ ScriptName,
                case file:make_symlink(Link, Target) of
                    ok ->
                        io:format("Link created: ~s -> ~s~n", [Link, Target]);
                    {error, eexist} ->
                        file:delete(Target),
                        file:make_symlink(Link, Target),
                        io:format("Link exists, new created: ~s -> ~s~n", [Link, Target]);
                    {error, eacces} ->
                        tetrapak:fail("Create link error, permission denied: ~s -> ~s~n", [Link, Target])
                end,
                [{Original, Link} | Acc];
            false ->
                Acc
        end
        end, PackageFiles1, filelib:wildcard("*", tetrapak:path("bin"))),
    ok.

in_dir(Dir, Path) ->
    lists:prefix(filename:split(Dir), filename:split(Path)).

copy_files(InstallDir, IsExcludedFunction) ->
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
                                                  tpk_file:mkdir(Target),
                                                  Acc
                                          end;
                                      false ->
                                          tpk_file:copy(P, Target),
                                          [{P, Target} | Acc]
                                  end
                          end
                  end, [], tetrapak:dir(), dir_first).
