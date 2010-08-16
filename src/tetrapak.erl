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
  {ok, OutDir} = file:get_cwd(),
  create_package(Dir, Template, OutDir).
create_package(Dir, Template, OutDir) ->
  case find_template_mod(Template) of
    {ok, TMod} ->
      case filelib:is_dir(Dir) of
        true -> run_template(TMod, Dir, OutDir);
        false -> {error, bad_dir}
      end;
    {error, _} -> 
      {error, bad_template}
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

template_dir(Module) ->
  "tetrapak_tpl_" ++ Name = atom_to_list(Module),
  filename:join([code:priv_dir(tetrapak), "templates", Name]).

run_template(TemplateMod, InDir, OutDir) ->
  case build_source(InDir) of
    {exit, ok} -> 
      case project_info(InDir) of 
        {ok, Project} ->
          Job = #tep_job{source_dir = InDir, 
                         template = TemplateMod,
                         template_dir = template_dir(TemplateMod),
                         files = otp_related_files(InDir),
                         output_dir = OutDir},
          TemplateMod:create_package(Project, Job);
        Error -> Error
      end;
    Error -> Error
  end.

% FIXME: allow configuration 
build_source(Dir) ->
  tep_util:run("make", ["-C", Dir]).

otp_related_files(D) ->
  tep_file:wildcard(D, "ebin/*.beam") ++
  tep_file:wildcard(D, "ebin/*.app") ++
  tep_file:wildcard(D, "ebin/*.appup") ++
  tep_file:wildcard(D, "include/*.hrl") ++
  tep_file:dir_contents(filename:join(D, "bin")) ++
  tep_file:dir_contents(filename:join(D, "priv")).

project_info(Dir) ->
  Ebin = filename:join(Dir, "ebin"), 
  case filelib:is_dir(Ebin) of
    true ->
      case find_app_file(Dir, Ebin) of
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
               deps = proplists:get_value(applications, Attrs, []) -- [stdlib,kernel],
               desc = proplists:get_value(description, Attrs, "")}.

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
