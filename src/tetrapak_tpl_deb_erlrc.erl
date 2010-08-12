%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_tpl_deb_erlrc).
-export([create_package/2]).
-include("tetrapak.hrl").

create_package(Project, Job) ->
  TemplateD = tetrapak:template_dir(?MODULE),
  tetrapak_tpl_deb:create_package(Project, Job#tep_job{template_dir = TemplateD}).
