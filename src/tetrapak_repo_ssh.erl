%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_repo_ssh).
-export([publish/2, accept_pkg_type/0]). 

-include("tetrapak.hrl").

accept_pkg_type() -> any.

publish(PackageFile, Repo = #tep_repository{options = Props}) ->
  Host = tep_config:repo_prop(Repo, host),
  Path = tep_config:repo_prop(Repo, path), 
  Port = case proplists:get_value(port, Props) of
    undefined -> 22;
    PVal      -> PVal
  end,
  UserCfg = case proplists:get_value(user, Props) of
    undefined -> [];
    UVal      -> [{user, UVal}] 
  end,
  {ok, SSH} = tep_ssh:login(Host, Port, UserCfg),
  Target = filename:join(Path, filename:basename(PackageFile)), 
  TargetTmp = Target ++ ".upload-tmp",
  case tep_ssh:is_dir(SSH,Path) of
    true ->
      tep_ssh:scp(SSH, PackageFile, TargetTmp),
      tep_ssh:mv(SSH, TargetTmp, Target);
    false -> tep_log:warn("remote repository path ~s is not a directory", [Path])
  end,
  tep_ssh:close(SSH),
  ok.
