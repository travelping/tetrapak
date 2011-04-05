%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_config).
-export([repositories/0, repository/1, list_repos/0, repo_prop/2]).
-export([project_info/1]).
-export([project_config/1, read_ini_file/1]).
-export([get_string/3]).

-include("tetrapak.hrl").

%% ------------------------------------------------------------
%% -- Repository specs
repositories() ->
    File = home_config_path("repositories"),
    [repo_def_to_record(T) || T <- tep_util:consult(File, "repository configuration")].

repo_def_to_record({repository, Name, Props}) ->
  case proplists:get_value(type, Props) of
    undefined ->
      tep_log:warn("repository ~s has no type defined", [Name]),
      throw({error, repo_def_invalid});
    Type ->
      #tep_repository{name = Name, type = Type, options = Props}
  end.

repository(Name) when is_list(Name) ->
  MName = re:replace(Name, "-", "_", [{return, list}]),
  repository(list_to_atom(MName));
repository(Name) when is_atom(Name) ->
  Repos = repositories(),
  case lists:keyfind(Name, 2, Repos) of
    false -> {error, not_found};
    Repo ->  {ok, Repo}
  end.

list_repos() ->
  Repos = tep_config:repositories(),
  case Repos of
    [] -> io:format("No repositories configured.~n");
    _  ->
      io:format("Available Repositories:~n"),
      lists:foreach(fun (#tep_repository{name = Name, type = Type}) ->
            io:format("   * ~s (~s) ~n", [Name, Type])
        end, Repos)
  end.

repo_prop(#tep_repository{name = Name, options = Props}, Key) ->
  case proplists:get_value(Key, Props) of
    undefined ->
      tep_log:warn("required option ~s missing in repository definition of '~s'",
        [Key, Name]),
      throw({error, repo_prop_missing});
    Val -> Val
  end.

%% ------------------------------------------------------------
%% -- Project info
project_info(Dir) ->
  Ebin = filename:join(Dir, "ebin"),
  case filelib:is_dir(Ebin) of
    true ->
      case find_app_file(Dir, Ebin) of
        {ok, Appfile} ->
          tep_log:debug("found application resource file ~s", [Appfile]),
          case file:consult(Appfile) of
            {ok, [Attrs]} -> {ok, app_to_project_info(Appfile, Dir, Attrs)};
            {error, E} -> {error, invalid_app_file, E}
          end;
        Error ->
          Error
      end;
    false ->
      {error, no_ebin_dir}
  end.

app_to_project_info(File, Dir, {application,Name,Attrs}) ->
  #tep_project{name = Name,
               app_file = File,
               directory = Dir,
               vsn  = app_attr(File, vsn, Attrs),
               deps = app_attr(File, applications, Attrs) -- [stdlib,kernel],
               desc = proplists:get_value(description, Attrs, ""),
               modules = app_attr(File, modules, Attrs)}.

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
    {[], _} -> {error, no_app_file};
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
    nomatch -> nomatch
  end.

%% ------------------------------------------------------------
%% -- Config files
home_config_path(File) ->
    HomeDir = os:getenv("HOME"),
    filename:join([HomeDir, ".tetrapak", File]).

project_config_file(#tep_project{directory = Dir}) ->
    filename:join([Dir, "tetrapak", "config.ini"]).

project_config(Project) ->
    BaseCfg = case read_config(home_config_path("config.ini")) of
                  {error, _Error} -> gb_trees:empty();
                  {ok, Tree}      -> Tree
              end,
    case read_config(project_config_file(Project), BaseCfg) of
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

get_string(Config, Key, Default) ->
    case gb_trees:lookup(ckey(Key), Config) of
        {value, Value} -> Value;
        none           -> Default
    end.

ckey(String) ->
    case re:split(String, "\\.", [{return, list}]) of
        []           -> error(badarg);
        [NoSection]  -> error(badarg);
        Parts ->
            [Key | Section] = lists:reverse(Parts),
            {string:join(lists:reverse(Section), ":"), Key}
    end.

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
                    NewAcc = gb_trees:enter({Section, Key}, Value, Acc),
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
