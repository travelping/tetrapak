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

%% ------------------------------------------------------------
%% -- Task API
check("clean:pkg:deb") ->
    filelib:wildcard("*.deb", tetrapak:config_path("package.outdir")) /= [].

run("pkg:deb", _) ->
    ReqDoc = case tetrapak:config("package.include_doc") of
                 true  -> ["doc"];
                 false -> []
             end,
    tetrapak_task:require_all(["build", "check" | ReqDoc]),
    file:make_dir(tetrapak:config_path("package.outdir")),
    DebFile = tpk_file:with_temp_dir(fun make_deb/1),
    io:format("package: ~s~n", [DebFile]);

run("clean:pkg:deb", _) ->
    tpk_file:delete("\\.deb$", tetrapak:config_path("package.outdir")).

%% ------------------------------------------------------------
%% -- Implementation
is_file_excluded(Path) ->
    is_useless(Path) orelse
    (in_dir("tetrapak", Path) and not tetrapak:config("package.include_src")) orelse
    (in_dir("src", Path)      and not tetrapak:config("package.include_src")) orelse
    (in_dir(tetrapak:config("edoc.outdir"), Path) and not tetrapak:config("package.include_doc")) orelse
    in_dir(tetrapak:config("package.outdir"), Path).

in_dir(Dir, Path) ->
    lists:prefix(filename:split(Dir), filename:split(Path)).

in_erlang_base(Application) ->
    lists:member(Application, tetrapak:config("package.deb.erlang_base_apps")).

is_useless(Filename) ->
    Name = tpk_file:basename(Filename),
    tpk_util:match(".*~$", Name) %% editor backups
    or tpk_util:match("\\..*\\.sw[po]", Name) %% vim swap files
    or tpk_util:match("^(.*/)*\\.git(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.svn(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.hg(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.bzr(/.*)?$", Filename)
    or tpk_util:match(tetrapak:config("package.exclude"), Filename).

file_mode("bin" ++ _) -> 8#755;
file_mode(_Path)      -> 8#644.

make_deb(PkgDir) ->
    BaseDir = tetrapak:dir(),
    Name    = tetrapak:get("config:appfile:name"),
    Vsn     = tetrapak:get("config:appfile:vsn"),
    Pkg     = tpk_util:f("~s-~s", [Name, Vsn]),
    PkgName = tpk_util:f("erlang-~s", [Name]),
    Arch    = "all",
    DebianName = no_underscores(PkgName),

    %% debian-binary
    file:write_file(filename:join(PkgDir, "debian-binary"), <<"2.0\n">>),

    %% data.tar.gz
    {ok, DataTarball} = tpk_file:tarball_create(filename:join(PkgDir, "data.tar.gz")),
    InstallDir = "usr/lib/erlang/lib/",
    tpk_file:tarball_mkdir_parents(DataTarball, InstallDir, [{mode, 8#755}]),
    PackageFiles =
        tpk_file:walk(fun (P, Acc) ->
                        File    = tpk_file:rebase_filename(P, BaseDir, ""),
                        Target  = InstallDir ++ Pkg ++ "/" ++ File,
                        case is_file_excluded(File) of
                            true ->
                                Acc;
                            false ->
                                case filelib:is_dir(P) of
                                    true ->
                                        tpk_file:tarball_mkdir(DataTarball, Target, [{mode, 8#755}]),
                                        Acc;
                                    false ->
                                        io:format("add ~s~n", [File]),
                                        Mode = file_mode(File),
                                        tpk_file:tarball_add_file(DataTarball, P, Target, [dereference, {mode, Mode}]),
                                        [{P, Target} | Acc]
                                end
                        end
                     end, [], tetrapak:dir(), dir_first),
    tpk_file:tarball_close(DataTarball),

    %% control.tar.gz
    {ok, ControlTarball} = tpk_file:tarball_create(filename:join(PkgDir, "control.tar.gz")),
    ControlFileOptions = [{owner, "root"}, {group, "root"}],

    %% copy control files with varsubst applied
    Deps = [no_underscores(tpk_util:f("erlang-~s", [S])) ||
                S <- tetrapak:get("config:appfile:deps"),
                not in_erlang_base(S)],
    case tetrapak:config("package.use_erlrc") of
        false -> Template = "deb";
        true  -> Template = "deb_erlrc"
    end,
    TemplateDir = filename:join([code:priv_dir(tetrapak), "templates", Template]),
    tpk_file:walk(fun (CFile, _) ->
                          Content = tpk_util:varsubst_file(CFile,
                                                           [{"name", DebianName}, {"version", Vsn},
                                                            {"appname", Name}, {"appdeps", string:join(Deps, ", ")},
                                                            {"section", tetrapak:config("package.deb.section")},
                                                            {"priority", tetrapak:config("package.deb.priority")},
                                                            {"maintainer", tetrapak:config("package.maintainer")},
                                                            {"desc", tetrapak:get("config:appfile:desc", "")}]),
                          tpk_file:tarball_add_binary(ControlTarball, filename:basename(CFile), Content, [{mode, 8#0744} | ControlFileOptions])
                  end, [], TemplateDir),

    %% generate md5sums
    Md5 = lists:foldl(fun ({P, Target}, Acc) ->
                              {ok, CkSum} = tpk_file:md5sum(P),
                              PN = list_to_binary(Target),
                              <<Acc/binary, CkSum/binary, " ", PN/binary, "\n">>
                      end, <<>>, PackageFiles),
    tpk_file:tarball_add_binary(ControlTarball, "md5sums", Md5, [{mode, 8#0644} | ControlFileOptions]),
    tpk_file:tarball_close(ControlTarball),

    %% write the actual .deb as an AR archive (sic!)
    DebFile = filename:join(tetrapak:config_path("package.outdir"), tpk_util:f("~s_~s_~s.deb", [DebianName, Vsn, Arch])),
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
                            {ok, Size} = file:copy(File, ArFile),
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
