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

-include("tetrapak.hrl").

-export([pass_options/1, pass_run/3]).

-passinfo({build, [{erlang, "Build Erlang modules"}]}).
-passinfo({clean, [{erlang, "Delete compiled Erlang modules"}]}).

%% ------------------------------------------------------------
%% -- Pass API
pass_options(_Group) -> [].

pass_run({build, erlang}, #tep_project{directory = Dir}, _Options) ->
    file:set_cwd(Dir),
    case make:all() of
        up_to_date -> ok;
        error -> tep_pass:fail("emake failed")
    end;

pass_run({clean, erlang}, #tep_project{directory = Dir}, _Options) ->
    EbinDir = filename:join(Dir, "ebin"),
    tep_file:delete("\\.beam$", EbinDir).
