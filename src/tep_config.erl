%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_config).
-behaviour(tep_pass).
-export([project_config/1, read_ini_file/1]).
-export([run/2]).

-pass({"config:appfile", "Read the application resource file"}).
-pass({"config:ini",     "Read the tetrapak config file"}).

-include("tetrapak.hrl").

run("config:appfile", _) ->
    Dir  = tetrapak:dir(),
    Ebin = filename:join(Dir, "ebin"),
    case find_app_file(Dir, Ebin) of
        {ok, Appfile} ->
            tep_log:debug("found application resource file ~s", [Appfile]),
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
    {done, gb_trees:to_list(project_config(tetrapak:dir()))}.

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
            tep_log:warn("required property ~s missing in app file ~s",
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
            tep_log:warn("project directory name not OTP-compliant"),
            {ok, hd(Files)};
        {Files, Appname} ->
            case lists:filter(fun (F) -> filename:rootname(F) =:= Appname end, Files) of
                [] -> {ok, hd(Files)};
                [File|_] -> {ok, File}
            end
    end.

dir_to_appname(Dir) ->
    Base = tep_file:basename(Dir),
    case re:run(Base,"([a-z_]+)(-.*)?", [caseless,{capture,first,list}]) of
        {match, [Appname]} -> Appname;
        nomatch            -> nomatch
    end.

%% ------------------------------------------------------------
%% -- Config files
home_config_path(File) ->
    HomeDir = os:getenv("HOME"),
    filename:join([HomeDir, ".tetrapak", File]).

project_config_file(Directory) ->
    filename:join([Directory, "tetrapak", "config.ini"]).

project_config(Directory) ->
    BaseCfg = case read_config(home_config_path("config.ini")) of
                  {error, _Error} -> gb_trees:empty();
                  {ok, Tree}      -> Tree
              end,
    case read_config(project_config_file(Directory), BaseCfg) of
        {error, _Error2} -> BaseCfg;
        {ok, Config}     -> Config
    end.

read_config(File) ->
    read_config(File, gb_trees:empty()).
read_config(File, Tree) ->
    case read_ini_file(File, Tree) of
        {error, enoent} ->
            tep_log:debug("config file ~s does not exist", [File]),
            {ok, gb_trees:empty()};
        {error, Error} ->
            {error, Error};
        {ok, ConfigTree} ->
            tep_log:debug("read config file ~s", [File]),
            {ok, ConfigTree}
    end.

ckey(Section, Key) ->
    Section ++ ":" ++ Key.

%% ------------------------------------------------------------
%% -- INI files
read_ini_file(Filename) ->
    read_ini_file(Filename, gb_trees:empty()).
read_ini_file(Filename, Tree) ->
    case file:open(Filename, [read]) of
        {ok, File} ->
            case read_ini_lines(File, 1, "", Tree) of
                {parse_error, Line, Message} ->
                    tep_log:warn("parse error in ~s:~b: ~s", [Filename, Line, Message]),
                    file:close(File),
                    {error, parse_error};
                {ok, Values} ->
                    file:close(File),
                    {ok, Values}
            end;
        {error, Error} ->
            {error, Error}
    end.

read_ini_lines(File, Line, Section, Acc) ->
    case file:read_line(File) of
        eof        -> {ok, Acc};
        {ok, Data} ->
            try ini_line(string:strip(string:strip(Data, right, $\n))) of
                {section, NewSection} ->
                    read_ini_lines(File, Line + 1, NewSection, Acc);
                {value, Key, Value} ->
                    NewAcc = gb_trees:enter(ckey(Section, Key), Value, Acc),
                    read_ini_lines(File, Line + 1, Section, NewAcc);
                comment ->
                    read_ini_lines(File, Line + 1, Section, Acc)
            catch
                {parse_error, Message} ->
                    {parse_error, Line, Message}
            end
    end.

ini_line("")                 -> comment;
ini_line(";" ++ _Comment)    -> comment;
ini_line("[" ++ SectionDecl) ->
    case re:run(SectionDecl, "([^\\]]+)\\].*", [{capture, all_but_first, list}]) of
        {match, [Section]} -> {section, Section};
        nomatch            -> throw({parse_error, "invalid section declaration"})
    end;
ini_line(Assignment) ->
    case re:run(Assignment, "^([\\w-]*)\\s*=\\s*([^;]*).*$", [{capture, all_but_first, list}]) of
        {match, ["", _Value]} -> throw({parse_error, "assignment to without key"});
        {match, [Key, Value]} -> {value, Key, ini_value(Value)};
        nomatch               -> throw({parse_error, "invalid assignment"})
    end.

ini_value(Value) ->
    case (catch list_to_integer(Value)) of
        Int when is_integer(Int) ->
            Int;
        {'EXIT', {badarg, _}} ->
            case Value of
                []    -> throw({parse_error, "assignment of empty value"});
                [Start | _] ->
                    case lists:member(Start, "\"[{") of
                        true ->
                            case read_term(Value) of
                                {ok, Term}       -> Term;
                                {error, Message} -> throw({parse_error, Message})
                            end;
                        false ->
                            string:strip(Value)
                    end
            end
    end.

read_term(String) ->
    case erl_scan:string(String) of
        {ok, Tokens, _End} ->
            case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
                {ok, Term} -> {ok, Term};
                {error, {_Loc, erl_parse, ParseError}} ->
                    {error, erl_parse:format_error(ParseError)}
            end;
        {error, {_Loc, erl_scan, ScanError}, _EndLoc} ->
            {error, erl_scan:format_error(ScanError)}
    end.
