%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_pass_compile).
-behaviour(tep_pass).
-pass_name(compile). 

-include("tetrapak.hrl").

-export([pass_options/0, pass_run/2]).

%% ------------------------------------------------------------
%% -- Pass API
pass_options() -> [].

pass_run(#tep_project{directory = Dir}, Options) ->
    file:set_cwd(Dir),
    case make:all() of
       up_to_date -> ok;
       error -> tep_pass:fail("emake failed")
    end.
