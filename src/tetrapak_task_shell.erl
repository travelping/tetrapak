%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_shell).
-behaviour(tetrapak_task).
-export([run/2]).

run("shell", _) ->
    code:add_patha(tetrapak:subdir("ebin")),
    case tetrapak_io:can_start_shell() of
        true ->
            tetrapak_io:start_shell(),
            timer:sleep(infinity);
        false ->
            tetrapak:fail("Cannot start shell")
    end.
