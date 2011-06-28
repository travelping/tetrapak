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

-include("tetrapak.hrl").

%% ------------------------------------------------------------
%% -- Task API
check("clean:pkg:deb") ->
    filelib:wildcard("*.deb", tetrapak:config_path("package.outdir")) /= [].

run("clean:pkg:deb", _) ->
    tpk_file:delete("\\.deb$", tetrapak:config_path("package.outdir"));

run("pkg:deb", _) ->
    case tetrapak:config("package.include_doc") of
        true  -> ReqDoc = ["doc"];
        false -> ReqDoc = []
    end,
    tetrapak:require_all(["build", "check" | ReqDoc]),
    file:make_dir(tetrapak:config_path("package.outdir")),
    tpk_file:with_temp_dir(fun make_deb/1);

run("pkg:debsrc", _) ->
    file:make_dir(tetrapak:config_path("package.outdir")),
    make_debsrc().

%% ------------------------------------------------------------
%% -- Implementation
is_useless(Filename) ->
    Name = tpk_file:basename(Filename),
    tpk_util:match(".*~$", Name) %% editor backups
    or tpk_util:match("\\..*\\.sw[po]", Name) %% vim swap files
    or tpk_util:match("\\.#.*", Name) %% emacs swap files
    or tpk_util:match("erl_crash.dump", Name) %% you know those...
    or (in_dir("tetrapak", Filename) and tpk_util:match(?LOCAL_CACHE, Name)) %% tetrapak task cache
    or tpk_util:match("^(.*/)*\\.git(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.svn(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.hg(/.*)?$", Filename)
    or tpk_util:match("^(.*/)*\\.bzr(/.*)?$", Filename)
    or tpk_util:match(tetrapak:config("package.exclude"), Filename).

file_mode("bin" ++ _) -> 8#755;
file_mode(_Path)      -> 8#644.

make_deb(PkgDir) ->
    Name    = atom_to_list(tetrapak:get("config:appfile:name")),
    Vsn     = tetrapak:get("config:appfile:vsn"),
    PkgName = "erlang-" ++ Name,
    Arch    = "all",
    DebianName = no_underscores(PkgName),

    %% debian-binary
    file:write_file(filename:join(PkgDir, "debian-binary"), <<"2.0\n">>),

    %% data.tar.gz
    {ok, DataTarball} = tpk_file:tarball_create(filename:join(PkgDir, "data.tar.gz")),
    InstallDir = "usr/lib/erlang/lib/" ++ tpk_util:f("~s-~s/", [Name, Vsn]),
    tpk_file:tarball_mkdir_parents(DataTarball, InstallDir, [{mode, 8#755}, {owner, "root"}, {group, "root"}]),
    IsExcluded = fun (Path) ->
                         is_useless(Path) orelse
                         (in_dir("tetrapak", Path) and not tetrapak:config("package.include_src")) orelse
                         (in_dir("src", Path)      and not tetrapak:config("package.include_src")) orelse
                         (in_dir(tetrapak:config("edoc.outdir"), Path) and not tetrapak:config("package.include_doc")) orelse
                         in_dir(tetrapak:config("package.outdir"), Path) orelse
                         in_dir("debian", Path)
                 end,
    PackageFiles1 = copy_files(DataTarball, InstallDir, IsExcluded),

    %% symlink binaries
    BinDir = "usr/bin",
    tpk_file:tarball_mkdir(DataTarball, BinDir, [{owner, "root"}, {group, "root"}]),
    PackageFiles2 = lists:foldl(fun (ScriptName, Acc) ->
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
        false -> Template = "deb";
        true  -> Template = "deb_erlrc"
    end,
    copy_control_template(ControlTarball, Template, "./", []),

    %% generate md5sums
    Md5 = lists:foldl(fun ({P, Target}, Acc) ->
                              {ok, CkSum} = tpk_file:md5sum(P),
                              PN = list_to_binary(Target),
                              <<Acc/binary, CkSum/binary, " ", PN/binary, "\n">>
                      end, <<>>, PackageFiles2),
    tpk_file:tarball_add_binary(ControlTarball, "md5sums", Md5, [{mode, 8#0644}, {owner, "root"}, {group, "root"}]),
    tpk_file:tarball_close(ControlTarball),

    %% write the actual .deb as an AR archive (sic!)
    DebFile = filename:join(tetrapak:config_path("package.outdir"), tpk_util:f("~s_~s_~s.deb", [DebianName, Vsn, Arch])),
    pack_ar(DebFile, PkgDir, ["debian-binary", "control.tar.gz", "data.tar.gz"]),
    io:format("package: ~s~n", [DebFile]).

make_debsrc() ->
    Version = tetrapak:get("config:appfile:vsn"),
    Pkg = "erlang-" ++ no_underscores(atom_to_list(tetrapak:get("config:appfile:name"))),
    ExtractDir = Pkg ++ "-" ++ Version ++ "/",

    %% <pkg>.tar.gz
    OrigTarballName = Pkg ++ "_" ++ Version ++ ".tar.gz",
    OrigTarballPath = filename:join(tetrapak:config_path("package.outdir"), OrigTarballName),
    {ok, OrigTarball} = tpk_file:tarball_create(OrigTarballPath),
    tpk_file:tarball_mkdir(OrigTarball, ExtractDir, [{mode, 8#744}, {owner, "root"}, {group, "root"}]),
    copy_control_template(OrigTarball, "deb_src", ExtractDir ++ "debian", [{"date", rfc_date(calendar:universal_time())}]),
    copy_files(OrigTarball, ExtractDir,
               fun (Path) ->
                       is_useless(Path)
                       orelse in_dir("debian", Path)
                       orelse in_dir(tetrapak:config("package.outdir"), Path)
               end),
    tpk_file:tarball_close(OrigTarball),

    %% <pkg>.dsc
    DscFileName = Pkg ++ "_" ++ Version ++ ".dsc",
    DscFile = filename:join(tetrapak:config_path("package.outdir"), DscFileName),
    {ok, OrigMd5} = tpk_file:md5sum(OrigTarballPath),
    {ok, Dsc} = file:open(DscFile, [write]),
    io:format(Dsc, "Format: 1.0~n", []),
    io:format(Dsc, "Architecture: any~n", []),
    io:format(Dsc, "Source: ~s~nBinary: ~s~n", [Pkg, Pkg]),
    io:format(Dsc, "Version: ~s~n", [Version]),
    io:format(Dsc, "Maintainer: ~s~n", [tetrapak:config("package.maintainer")]),
    io:format(Dsc, "Standards-Version: 3.9.1~n", []),
    io:format(Dsc, "Files:~n ~s ~b ~s~n", [OrigMd5, tpk_file:size(OrigTarballPath), OrigTarballName]),
    file:close(Dsc),

    done.

in_dir(Dir, Path) ->
    lists:prefix(filename:split(Dir), filename:split(Path)).

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
                                                  tpk_file:tarball_mkdir(Tarball, Target, [{mode, 8#755}, {owner, "root"}, {group, "root"}]),
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
    Deps = [no_underscores(tpk_util:f("erlang-~s", [S])) ||
                S <- tetrapak:get("config:appfile:deps"),
                not in_erlang_base(S)],
    case Deps of
        [] -> DepString = "";
        _  -> DepString = ", " ++ string:join(Deps, ", ")
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
                                                            {"appname", tetrapak:get("config:appfile:name")},
                                                            {"appdeps", DepString},
                                                            {"section", tetrapak:config("package.deb.section")},
                                                            {"priority", tetrapak:config("package.deb.priority")},
                                                            {"maintainer", tetrapak:config("package.maintainer")},
                                                            {"desc", tetrapak:get("config:appfile:desc", "")}]),
                                  tpk_file:tarball_add_binary(Tarball, Target, Content, FileOptions)
                          end
                  end, [], TemplateDir, dir_first).

pack_ar(Outfile, Dir, Entries) ->
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

rfc_date({{Year, Month, Day},{Hours, Minutes, Seconds}}) ->
     DayName = lists:nth(calendar:day_of_the_week(Year, Month, Day),
                         ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]),
     MonthNa = lists:nth(Month - 1,
                         ["Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]),
     io_lib:format("~s, ~2..0b ~s ~4..0b ~2..0b:~2..0b:~2..0b +0000",
                   [DayName, Day, MonthNa, Year, Hours, Minutes, Seconds]).
