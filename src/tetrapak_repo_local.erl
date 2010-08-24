%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_repo_local).
-export([publish/2, accept_pkg_type/0]). 

-include("tetrapak.hrl").

accept_pkg_type() -> any.

publish(PackageFile, #tep_repository{options = Props}) ->
  Path = proplists:get_value(path, Props),
  Target = filename:join(Path, filename:basename(PackageFile)), 
  case filelib:is_dir(Path) of
    true -> 
      tep_file:copy(PackageFile, Target),
      ok;
    false -> 
      tep_log:warn("local repository ~s is not a directory", [Path]),
      {error, repo_path_invalid}
  end.
