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

run(Dir, Template, Options) ->
  case create_package(Dir, Template) of
    {ok, PkgFile, Job} ->
      tep_log:info("finished packaging, package is at ~s", [PkgFile]),
      case proplists:get_value(publish, Options) of
        false -> PkgFile;
        RepoName -> 
          case tep_publish:publish(PkgFile, RepoName, Job) of
            ok -> 
              tep_log:info("finished publishing, deleting package ~s", [PkgFile]),
              tep_file:delete(PkgFile);
            {error, Reason} -> 
              tep_log:warn("erred while publishing: ~p", [Reason]),
              oops
          end
      end;
    {error, Reason} -> 
      tep_log:warn("erred while packaging: ~p", [Reason]),
      oops
  end.

create_package(Dir, Template) ->
  {ok, OutDir} = file:get_cwd(),
  create_package(Dir, Template, OutDir).
create_package(Dir, Template, OutDir) ->
  case find_template_mod(Template) of
    {ok, TMod} ->
      case filelib:is_dir(Dir) of
        true -> 
          run_template(Template, TMod, Dir, OutDir);
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

run_template(TName, TemplateMod, InDir, OutDir) ->
  case build_source(InDir) of
    {exit, ok} -> 
      case tep_config:project_info(InDir) of 
        {ok, Project} ->
          tep_log:info("applying template ~s", [TName]),
          Job = #tep_job{source_dir = InDir, 
                         template = TemplateMod,
                         template_dir = template_dir(TemplateMod),
                         files = otp_related_files(InDir),
                         output_dir = OutDir},
          case TemplateMod:create_package(Project, Job) of
            {ok, File} -> {ok, File, Job};
            {error, Err} -> {error, Err}
          end;
        Error -> Error
      end;
    Error -> Error
  end.

% FIXME: allow configuration 
build_source(Dir) ->
  tep_log:info("building project in ~s", [Dir]),
  tep_util:run("make", ["-C", Dir]).

otp_related_files(D) ->
  tep_file:wildcard(D, "ebin/*.beam") ++
  tep_file:wildcard(D, "ebin/*.app") ++
  tep_file:wildcard(D, "ebin/*.appup") ++
  tep_file:wildcard(D, "include/*.hrl") ++
  tep_file:dir_contents(filename:join(D, "bin")) ++
  tep_file:dir_contents(filename:join(D, "priv")).
