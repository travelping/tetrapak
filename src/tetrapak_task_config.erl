%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_config).
-behaviour(tetrapak_task).
-export([read_ini_file/1]).
-export([run/2]).

run("config:appfile", _) ->
    Dir  = tetrapak:dir(),
    Ebin = filename:join(Dir, "ebin"),
    case find_app_file(Dir, Ebin) of
        {ok, Appfile} ->
            tpk_log:debug("found application resource file ~s", [Appfile]),
            case file:consult(Appfile) of
                {ok, [Attrs]} ->
                    {done, appfile_info(Appfile, Attrs)};
                {error, SyntaxError} ->
                    EDesc = file:format_error(SyntaxError),
                    tetrapak:fail("syntax error in application resource file:~n    ~s~n", [EDesc])
            end;
        {error, FileError} ->
            tetrapak:fail("app file could not be found: ~p", [FileError])
    end;

run("config:ini", _) ->
    BaseConfig = gb_trees:from_orddict(lists:keysort(1, tetrapak:get("tetrapak:appdata:defaults"))),
    HomeConfig = read_config(home_config_path("config.ini"), BaseConfig),
    ProjectConfig = read_config(project_config_path("config.ini"), HomeConfig),
    % io:format("~p~n", [ProjectConfig]),
    {done, ProjectConfig}.

%% ------------------------------------------------------------
%% -- Project info
appfile_info(File, {application, Name, Attrs}) ->
    [{"name",    Name},
     {"path",    File},
     {"vsn",     app_attr(File, vsn, Attrs)},
     {"deps",    app_attr(File, applications, Attrs) -- [stdlib,kernel]},
     {"desc",    proplists:get_value(description, Attrs, "")},
     {"modules", app_attr(File, modules, Attrs)}].

app_attr(File, Key, Attrs) ->
    case proplists:get_value(Key, Attrs) of
        undefined ->
            tpk_log:warn("required property ~s missing in app file ~s",
                [Key, File]),
            throw({error, application_prop_missing});
        Val -> Val
    end.

find_app_file(OrigDir, Ebin) ->
    Candidates = filelib:wildcard(filename:join(Ebin, "*.app")),
    case {Candidates, dir_to_appname(OrigDir)} of
        {[], _} ->
            {error, no_app_file};
        {Files, nomatch} ->
            tpk_log:warn("project directory name not OTP-compliant"),
            {ok, hd(Files)};
        {Files, Appname} ->
            case lists:filter(fun (F) -> filename:rootname(F) =:= Appname end, Files) of
                [] -> {ok, hd(Files)};
                [File|_] -> {ok, File}
            end
    end.

dir_to_appname(Dir) ->
    Base = tpk_file:basename(Dir),
    case re:run(Base,"([a-z_]+)(-.*)?", [caseless,{capture,first,list}]) of
        {match, [Appname]} -> Appname;
        nomatch            -> nomatch
    end.

%% ------------------------------------------------------------
%% -- Config files
home_config_path(File) ->
    HomeDir = os:getenv("HOME"),
    filename:join([HomeDir, ".tetrapak", File]).

project_config_path(Filename) ->
    filename:join(tetrapak:subdir("tetrapak"), Filename).

read_config(File, Tree) ->
    case read_ini_file(File, Tree) of
        {error, {file, enoent}} ->
            Tree;
        {ok, ConfigTree} ->
            ConfigTree;
        {error, Error} ->
            fmt_error(File, Error),
            tetrapak:fail()
    end.

read_ini_file(Filename) ->
    read_ini_file(Filename, gb_trees:empty()).
read_ini_file(Filename, Tree) ->
    case tetrapak_ini_lexer:file(Filename) of
        {ok, Tokens, _Endl} ->
            case tetrapak_ini_parser:parse(Tokens) of
                {ok, Sections} -> {ok, do_sections(Sections, Tree)};
                Error          -> Error
            end;
        Error -> Error
    end.

do_sections(SList, Tree) ->
    lists:foldl(fun ({section, SName, Props}, OuterAcc) ->
                        lists:foldl(fun ({Key, Value}, InnerAcc) ->
                                            gb_trees:enter(ckey(SName, Key), Value, InnerAcc)
                                    end, OuterAcc, Props)
                end, Tree, SList).

ckey("", Key) -> Key;
ckey(Section, Key) -> Section ++ "." ++ Key.

fmt_error(File, {Line, Module, ErrorDesc}) ->
    English = Module:format_error(ErrorDesc),
    io:format("~s:~b: Error: ~s~n", [File, Line, English]);
fmt_error(File, {file, Error}) ->
    English = file:format_error(Error),
    io:format("~s: Error: ~s~n", [File, English]).
