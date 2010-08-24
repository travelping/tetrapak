%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_publish).
-export([publish/3]).

-include("tetrapak.hrl").

publish(Package, RepoName, Job) when is_record(Job, tep_job) ->
  case tep_config:repository(RepoName) of
    {ok, Repo} ->
      TemplatePkgType = (Job#tep_job.template):pkg_type(),
      RepoCB = find_repo_mod(Repo),
      PubOK = case RepoCB:accept_pkg_type() of
        any -> true;
        TemplatePkgType -> true;
        _ ->
          tep_log:warn("repository ~s does not accept packages of type ~s",
            [Repo#tep_repository.name, TemplatePkgType]),
          false
      end,
      if PubOK -> do_publish(Package, RepoCB, Repo);
        true  -> {error, repo_type_mismatch}
      end;
    {error, not_found} ->
      tep_log:warn("repository ~s not found", [RepoName]),
      {error, repo_not_found}
  end.

find_repo_mod(#tep_repository{name = Name}) when is_atom(Name) ->
  SName = atom_to_list(Name),
  tep_util:find_module(list_to_atom("tetrapak_repo_" ++ SName)).

do_publish(Package, RepoCB, Repo) ->
  RepoCB:publish(Package, Repo).  
