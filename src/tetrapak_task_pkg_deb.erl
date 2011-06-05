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
file_target(Pkg, _Path)       -> "usr/lib/erlang/lib/" ++ Pkg ++ "/".

make_deb(PkgDir) ->
    BaseDir = tetrapak:dir(),
    Name    = tetrapak:get("config:appfile:name"),
    Vsn     = tetrapak:get("config:appfile:vsn"),
    Pkg     = tpk_util:f("~s-~s", [Name, Vsn]),
    PkgName = tpk_util:f("erlang-~s", [Name]),
    Arch = "all",
    DebianName = no_underscores(PkgName),
    DistDir = tetrapak:subdir(tetrapak:get("config:pkg:outdir", "dist")),
    file:make_dir(DistDir),

    %% debian-binary
    file:write_file(filename:join(PkgDir, "debian-binary"), <<"2.0\n">>),

    %% data.tar.gz
    PackageFiles = lists:map(fun (P) ->
                                     File = tpk_file:rebase_filename(P, BaseDir, ""),
                                     Target = file_target(Pkg, File) ++ File,
                                     {P, Target}
                             end, otp_related_files(tetrapak:dir())),
    {ok, DataTarball} = tpk_file:tarball_create(filename:join(PkgDir, "data.tar.gz")),
    lists:foreach(fun ({P, Target}) -> tpk_file:tarball_add_file(DataTarball, P, Target, [dereference]) end, PackageFiles),
    tpk_file:tarball_close(DataTarball),

    %% control.tar.gz
    {ok, ControlTarball} = tpk_file:tarball_create(filename:join(PkgDir, "control.tar.gz")),

    %% copy control files with varsubst applied
    DepString = lists:map(fun (S) -> no_underscores(tpk_util:f(", erlang-~s", [S])) end,
                          lists:sort(tetrapak:get("config:appfile:deps"))),
    TemplateDir = filename:join([code:priv_dir(tetrapak), "templates", "deb"]),
    tpk_file:walk(fun (CFile, _) ->
                          Content = tpk_util:varsubst_file(CFile,
                                                           [{"name", DebianName}, {"version", Vsn},
                                                            {"appname", Name}, {"appdeps", DepString},
                                                            {"desc", tetrapak:get("config:appfile:desc", "")}]),
                          tpk_file:tarball_add_binary(ControlTarball, filename:basename(CFile), Content, [])
                  end, [], TemplateDir),

    %% generate md5sums
    Md5 = lists:foldl(fun ({P, Target}, Acc) ->
                              {ok, CkSum} = tpk_file:md5sum(P),
                              PN = list_to_binary(Target),
                              <<Acc/binary, CkSum/binary, " ", PN/binary, "\n">>
                      end, <<>>, PackageFiles),
    tpk_file:tarball_add_binary(ControlTarball, "md5sums", Md5, []),
    tpk_file:tarball_close(ControlTarball),

    tpk_file:copy(PkgDir ++ "/data.tar.gz", DistDir ++ "/data.tar.gz"),
    tpk_file:copy(PkgDir ++ "/control.tar.gz", DistDir ++ "/control.tar.gz"),

    %% write the actual .deb as an AR archive (sic!)
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

no_underscores(S) -> re:replace(S, "_", "-", [global, {return, list}]).
