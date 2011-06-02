%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_pkg_deb).
-behaviour(tetrapak_task).
-export([check/1, run/2]).

-task({"pkg:deb", "Create a binary debian package"}).
-task({"clean:pkg:deb", "Delete debian packages"}).

%% ------------------------------------------------------------
%% -- Task API
check("clean:pkg:deb") ->
    filelib:is_dir(tetrapak:subdir(tetrapak:get("config:ini:pkg:outdir", "dist"))).

run("pkg:deb", _) ->
    tetrapak_task:require_all(["build", "check"]),

    DistDir = tetrapak:subdir(tetrapak:get("config:pkg:outdir", "dist")),
    file:make_dir(DistDir),

    DebFile = tpk_file:with_temp_dir(fun make_deb/1),
    tpk_log:info("packaging done, package is at: ~s", [DebFile]);

run("clean:pkg:deb", _) ->
    tpk_file:delete("\\.deb$", tetrapak:subdir(tetrapak:get("config:ini:pkg:outdir", "dist"))).

%% ------------------------------------------------------------
%% -- Implementation
otp_related_files(D) ->
    tpk_file:wildcard(D, "ebin/*.beam") ++
    tpk_file:wildcard(D, "ebin/*.app") ++
    tpk_file:wildcard(D, "ebin/*.appup") ++
    tpk_file:wildcard(D, "include/*.hrl") ++
    tpk_file:filter_useless(tpk_file:dir_contents(filename:join(D, "bin"))) ++
    tpk_file:filter_useless(tpk_file:dir_contents(filename:join(D, "priv"))).

file_target(_Pkg, "bin" ++ _) -> "usr/";
file_target(Pkg, _Path)       -> "usr/lib/erlang/lib/" ++ Pkg.

make_deb(PkgDir) ->
    Name    = tetrapak:get("config:appfile:name"),
    Vsn     = tetrapak:get("config:appfile:vsn"),
    Pkg     = tpk_util:f("~s-~s", [Name, Vsn]),
    PkgName = tpk_util:f("erlang-~s", [Name]),
    Arch = "all",
    DebianName = no_underscores(PkgName),
    PackageFiles = otp_related_files(tetrapak:dir()),
    DistDir = tetrapak:subdir(tetrapak:get("config:pkg:outdir", "dist")),
    file:make_dir(DistDir),

    %% debian-binary
    file:write_file(filename:join(PkgDir, "debian-binary"), <<"2.0\n">>),

    %% data.tar.gz
    DataDir = filename:join(PkgDir, "data"), tpk_file:mkdir(DataDir),
    lists:foreach(fun (P) ->
                          File = tpk_file:rebase_filename(P, tetrapak:dir(), ""),
                          Target = filename:join([DataDir, file_target(Pkg, File), File]),
                          tpk_file:copy(P, Target)
                  end, PackageFiles),
    tpk_file:make_tarball(filename:join(PkgDir, "data.tar.gz"), ".", DataDir, ".*"),

    %% control.tar.gz
    ControlDir = filename:join(PkgDir, "control"),

    %% copy control files with varsubst applied
    DepString = lists:map(fun (S) -> no_underscores(tpk_util:f(", erlang-~s", [S])) end,
                          lists:sort(tetrapak:get("config:appfile:deps"))),
    TemplateDir = filename:join([code:priv_dir(tetrapak), "templates", "deb"]),
    tpk_file:copy(TemplateDir, ControlDir),
    tpk_file:walk(fun (P, _) ->
                          tpk_file:varsubst([{"name", DebianName}, {"version", Vsn},
                                             {"appname", Name}, {"appdeps", DepString},
                                             {"desc", tetrapak:get("config:appfile:desc", "")}],
                                            P, P)
                  end, [], ControlDir),

    %% generate md5sums
    Md5_File = filename:join(ControlDir, "md5sums"),
    {ok, Md5} = file:open(Md5_File, [write]),
    tpk_file:walk(fun (P, _) ->
                          io:format(Md5, "~s ~s~n", [tpk_file:md5sum(P), tpk_file:rebase_filename(P, DataDir, "")])
                  end, [], DataDir),
    file:close(Md5),

    %% tarballize control files
    tpk_file:make_tarball(filename:join(PkgDir, "control.tar.gz"), ".", ControlDir, ".*"),

    DebFile = filename:join(DistDir, tpk_util:f("~s_~s_~s.deb", [DebianName, Vsn, Arch])),
    make_ar(DebFile, PkgDir, ["debian-binary", "control.tar.gz", "data.tar.gz"]),
    DebFile.

make_ar(Outfile, Dir, Entries) ->
    {ok, ArFile} = file:open(Outfile, [write]),
    try
        file:write(ArFile, <<"!<arch>\n">>),
        lists:foldl(fun (Name, Offset) ->
                            File = filename:join(Dir, Name),
                            Size = tpk_file:size(File),
                            io:format(ArFile, "~-16s~-12s~-6s~-6s~-8s~-10B`\n", [Name, "1280174243", "0", "0", "000644", Size]),
                            file:copy(File, ArFile),
                            NewOffset = Offset + 60 + Size,
                            if
                                NewOffset rem 2 =:= 0 ->
                                    NewOffset;
                                true ->
                                    file:write(ArFile, <<"\n">>), % data section is 2-byte aligned
                                    NewOffset + 1
                            end
                    end, 0, Entries)
     after
         file:close(Outfile)
     end.

no_underscores(S) -> re:replace(S, "_", "-", [{return, list}]).
