%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_tpl_tarball).
-export([create_package/2]).

-include("tetrapak.hrl").

create_package(#tep_project{name = Name, vsn = Vsn}, Path) -> 
  PkgName = tep_util:f("~s-~s", [Name, Vsn]),
  tep_file:make_tarball(PkgName ++ ".tar.gz", PkgName, Path, ".*").
