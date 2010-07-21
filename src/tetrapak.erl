%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak).
-compile(export_all).

-include("tetrapak.hrl").

create_package(Dir, Template) ->
  case find_template_mod(Template) of
    {ok, TMod} ->
      case filelib:is_dir(Dir) of
        true -> run_template(Dir, Template, TMod);
        false -> {error, bad_dir}
      end;
    {error, _} -> {error, bad_template}
  end.
 
is_template(Name) ->
  case find_template_mod(Name) of
    {ok, _} -> true;
    {error, _} -> false
  end.

find_template_mod(Name) when is_list(Name) ->
  MName = re:replace(Name, "-", "_", [{return, list}]), 
  tep_util:find_module(list_to_atom("tetrapak_tpl_" ++ MName));
find_template_mod(Name) when is_atom(Name) ->
  find_template_mod(atom_to_list(Name)).
  
run_template(Dir, TemplateName, TemplateMod) ->
  Build = fun(SandboxPath) ->   
      case build_source(SandboxPath) of
        {exit, ok} -> 
          case project_info(Dir, SandboxPath) of 
            {ok, Project} ->
              tep_log:info("deleting non-otp stuff..."),
              otp_clean_directory(SandboxPath),
              tep_log:info("applying template ~s", [TemplateName]),
              TemplateMod:create_package(Project, SandboxPath);
            Error -> Error
          end;
        Error -> Error
      end
  end,
  with_sandbox(Build, Dir).

with_sandbox(DoSomething, Dir) ->
  Run = fun (SandboxPath) -> 
      tep_log:info("copying ~s to ~s", [Dir,SandboxPath]),
      tep_file:copy(Dir, SandboxPath),
      DoSomething(SandboxPath)
  end,
  tep_file:with_temp_dir(Run).

% FIXME: allow configuration 
build_source(Dir) ->
  tep_util:run_proc("make", ["-C", Dir, "clean", "all"]).

project_info(OrigDir, Dir) ->
  Ebin = filename:join(Dir, "ebin"), 
  case filelib:is_dir(Ebin) of
    true ->
      case find_app_file(OrigDir, Ebin) of 
        {ok, Appfile} ->
          tep_log:info("found application resource file ~s", [Appfile]),
          case file:consult(Appfile) of 
            {ok, [Attrs]} -> {ok, app_to_project_info(Attrs)};
            {error, E} -> {error, invalid_app_file, E}
          end;
        Error ->
          Error
      end;
    false ->
      {error, no_ebin_dir}
  end.

app_to_project_info({application,Name,Attrs}) ->
  #tep_project{name = Name,
               vsn  = proplists:get_value(vsn,Attrs),
               deps = proplists:get_value(applications, Attrs) -- [stdlib,kernel]
              }.

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

otp_clean_directory(Dir) ->
  {ok, Files} = file:list_dir(Dir),
  Delete = Files -- ["ebin", "priv", "include", "bin"],
  lists:foreach(fun (F) ->
        JP = filename:join(Dir, F),
        tep_log:info("deleting ~s", [JP]),
        tep_file:delete(JP)
    end, Delete).

