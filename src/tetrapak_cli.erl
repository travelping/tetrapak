%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_cli).
-export([boot/0, start/1]).
-export([shell_dummy/0]).

boot() ->
    start(init:get_plain_arguments()).

start([]) ->
    usage(),
    halt(1);
start(Commands) ->
    {ok, Cwd} = file:get_cwd(),
    case tetrapak:run(Cwd, Commands) of
        {unknown, Key} ->
            io:format(standard_error, "Error: no such command: ~s~n", [Key]),
            halt(1);
        error ->
            halt(2);
        ok ->
            halt(0)
    end.

usage() ->
    io:format(standard_error,
              "Usage: tetrapak <command> [ options ]~n~n"
              "Available Tasks~n~s",
              [tablist(tetrapak:all_commands())]).

tablist(Lis) ->
    MaxWidth = lists:foldl(fun ({K, _}, Max) -> erlang:max(iolist_size(K), Max) end, 0, Lis),
    lists:foldr(fun ({Name, Desc}, Acc) ->
                   Space = lists:duplicate(MaxWidth - iolist_size(Name), $ ),
                   ["  " ++ Name  ++ "  " ++ Space ++ "- " ++ Desc ++ "\n" | Acc]
                end, [], Lis).

shell_dummy() ->
    timer:sleep(infinity).
