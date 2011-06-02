%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_pass_package_debian).
-behaviour(tep_pass).
-export([check/1, run/2]).

-pass({"pkg:deb", "Create a binary debian package"}).
-pass({"clean:pkg:deb", "Delete debian packages"}).

%% ------------------------------------------------------------
%% -- Pass API
check("clean:pkg:deb") ->
    filelib:is_dir(tetrapak:subdir(tetrapak:get("config:ini:pkg:outdir", "dist"))).

run("pkg:deb", _) ->
    tep_pass:require_all(["build", "check"]),

    DistDir = tetrapak:subdir(tetrapak:get("config:pkg:outdir", "dist")),
    file:make_dir(DistDir),

    DebFile = tep_file:with_temp_dir(fun make_deb/1),
    tep_log:info("packaging done, package is at: ~s", [DebFile]);

run("clean:pkg:deb", _) ->
    tep_file:delete("\\.deb$", tetrapak:subdir(tetrapak:get("config:ini:pkg:outdir", "dist"))).

%% ------------------------------------------------------------
%% -- Implementation
otp_related_files(D) ->
    tep_file:wildcard(D, "ebin/*.beam") ++
    tep_file:wildcard(D, "ebin/*.app") ++
    tep_file:wildcard(D, "ebin/*.appup") ++
    tep_file:wildcard(D, "include/*.hrl") ++
    tep_file:filter_useless(tep_file:dir_contents(filename:join(D, "bin"))) ++
    tep_file:filter_useless(tep_file:dir_contents(filename:join(D, "priv"))).

file_target(_Pkg, "bin" ++ _) -> "usr/";
file_target(Pkg, _Path)       -> "usr/lib/erlang/lib/" ++ Pkg.

make_deb(PkgDir) ->
    Name    = tetrapak:get("config:appfile:name"),
    Vsn     = tetrapak:get("config:appfile:vsn"),
    Pkg     = tep_util:f("~s-~s", [Name, Vsn]),
    PkgName = tep_util:f("erlang-~s", [Name]),
    Arch = "all",
    DebianName = no_underscores(PkgName),
    PackageFiles = otp_related_files(tetrapak:dir()),
    DistDir = tetrapak:subdir(tetrapak:get("config:pkg:outdir", "dist")),
    file:make_dir(DistDir),

    %% debian-binary
    file:write_file(filename:join(PkgDir, "debian-binary"), <<"2.0\n">>),

    %% data.tar.gz
    DataDir = filename:join(PkgDir, "data"), tep_file:mkdir(DataDir),
    lists:foreach(fun (P) ->
                          File = tep_file:rebase_filename(P, tetrapak:dir(), ""),
                          Target = filename:join([DataDir, file_target(Pkg, File), File]),
                          tep_file:copy(P, Target)
                  end, PackageFiles),
    tep_file:make_tarball(filename:join(PkgDir, "data.tar.gz"), ".", DataDir, ".*"),

    %% control.tar.gz
    ControlDir = filename:join(PkgDir, "control"),

    %% copy control files with varsubst applied
    DepString = lists:map(fun (S) -> no_underscores(tep_util:f(", erlang-~s", [S])) end,
                          lists:sort(tetrapak:get("config:appfile:deps"))),
    TemplateDir = filename:join([code:priv_dir(tetrapak), "templates", "deb"]),
    tep_file:copy(TemplateDir, ControlDir),
    tep_file:walk(fun (P, _) ->
                          tep_file:varsubst([{"name", DebianName}, {"version", Vsn},
                                             {"appname", Name}, {"appdeps", DepString},
                                             {"desc", tetrapak:get("config:appfile:desc", "")}],
                                            P, P)
                  end, [], ControlDir),

    %% generate md5sums
    Md5_File = filename:join(ControlDir, "md5sums"),
    {ok, Md5} = file:open(Md5_File, [write]),
    tep_file:walk(fun (P, _) ->
                          io:format(Md5, "~s ~s~n", [tep_file:md5sum(P), tep_file:rebase_filename(P, DataDir, "")])
                  end, [], DataDir),
    file:close(Md5),

    %% tarballize control files
    tep_file:make_tarball(filename:join(PkgDir, "control.tar.gz"), ".", ControlDir, ".*"),

    DebFile = filename:join(DistDir, tep_util:f("~s_~s_~s.deb", [DebianName, Vsn, Arch])),
    make_ar(DebFile, PkgDir, ["debian-binary", "control.tar.gz", "data.tar.gz"]),
    DebFile.

make_ar(Outfile, Dir, Entries) ->
    {ok, ArFile} = file:open(Outfile, [write]),
    try
        file:write(ArFile, <<"!<arch>\n">>),
        lists:foldl(fun (Name, Offset) ->
                            File = filename:join(Dir, Name),
                            Size = tep_file:size(File),
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
