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

-include("tetrapak.hrl").

config_path(File) ->
  HomeDir = os:getenv("HOME"),
  filename:join([HomeDir, ".tetrapak", File]).

%% ------------------------------------------------------------ 
%% -- Repository specs

repositories() ->
  File = config_path("repositories"),
  case file:consult(File) of
    {ok, Terms} -> 
      [repo_def_to_record(T) || T <- Terms];
    {error, enoent} -> [];
    {error, Error} -> 
      tep_log:warn("could not read repository config file ~s: ~s", 
        [File, file:format_error(Error)]),
      []
  end.

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
%% -- Repository specs

project_info(Dir) ->
  Ebin = filename:join(Dir, "ebin"), 
  case filelib:is_dir(Ebin) of
    true ->
      case find_app_file(Dir, Ebin) of
        {ok, Appfile} ->
          tep_log:debug("found application resource file ~s", [Appfile]),
          case file:consult(Appfile) of
            {ok, [Attrs]} -> {ok, app_to_project_info(Appfile, Attrs)};
            {error, E} -> {error, invalid_app_file, E}
          end;
        Error ->
          Error
      end;
    false ->
      {error, no_ebin_dir}
  end.

app_to_project_info(File, {application,Name,Attrs}) ->
  #tep_project{name = Name,
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
