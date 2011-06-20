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
    end;

run("tetrapak:reload", _) ->
    tetrapak:require("build"),
    Modules = lists:map(fun list_to_atom/1, tetrapak:get("build:erlang:modules")),
    io:format("reloading changed modules:~n  ~p~n", [Modules]),
    lists:foreach(fun load/1, Modules).

load(Mod) ->
    code:purge(Mod),
    code:load_file(Mod).
