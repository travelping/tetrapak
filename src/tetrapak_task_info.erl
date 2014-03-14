-module(tetrapak_task_info).
-export([run/2]).
-define(DEFAULT, [kernel, stdlib]).
-include("tetrapak.hrl").
% --------------------------------------------------------------------------------------------------
% -- Runs
run("info:application", _) ->
    {done, [{name, appname([{"src/", ".app.src"}, {"ebin/", ".app"}])}]};

run("info:deps:list", _) ->
    tetrapak:require_all(["build:erlang", "tetrapak:load"]),
    io:format("application list: ~p~n", [lists:reverse(deps([tetrapak:get("config:appfile:name")], [], ?DEFAULT))]);

run("info:deps", _) ->
    tetrapak:require_all(["build:erlang", "tetrapak:load"]),
    Tree = deps_tree(tetrapak:get("config:appfile:name"), normal, default()),
    show_tree([Tree], [], error =/= init:get_argument(path)).

% --------------------------------------------------------------------------------------------------
% -- Implementation
default() ->
    tetrapak:config("info:deps:default").


appname([]) ->
    tetrapak:fail("no application name found for the application directory~s~n", [tetrapak:dir()]);
appname([{Dir, Ext} | Rest]) ->
    case filelib:wildcard(filename:join(tetrapak:path(Dir), "*" ++ Ext)) of
        [AppFile] ->
            Name = filename:basename(AppFile, Ext),
            list_to_atom(Name);
        _ ->
            appname(Rest)
    end.

deps_tree(App, Type, Default) ->
    case load(App) of
        error ->
            {App, Type, not_found};
        [Apps, InlcudedApps] ->
            {App, Type, [deps_tree(AppDep, normal, Default) || AppDep <- (Apps -- Default)]
                        ++ [deps_tree(AppDep, include, Default) || AppDep <- InlcudedApps]}
    end.

deps([], Started, _Default) ->
    Started;
deps([App | ToStart], Started, Default) ->
    case lists:member(App, Started) of
        true ->
            deps(ToStart, Started, Default);
        false ->
            NewStarted = dep_start(App, Started, Default),
            deps(ToStart, [App | NewStarted], Default)
    end.

dep_start(App, Started, Default) ->
    case load(App) of
        error ->
            tetrapak:fail("Dependency ~s is not installed.~n", [App]);
        [Apps, _] ->
            deps(Apps -- Default, Started, Default)
    end.

load(App) ->
    case application:load(App) of
        Ok when ?CHECK_LOADED(Ok) == true ->
            [begin
                {ok, Apps} = application:get_key(App, Key),
                Apps
             end || Key <- [applications, included_applications]];
        _Error ->
            error
    end.

show_tree([], _Depth, _Path) ->
    ok;
show_tree([{App, Type, Dependencies} | Rest], Depth, Path) ->
    case Depth of
        [] -> ok;
        _ ->
            lists:foreach(fun (true) -> io:put_chars(" | ");
                              (false) -> io:put_chars("  ")
                          end, tl(lists:reverse(Depth))),
            case Rest of
                [] -> io:put_chars(" `-- ");
                _ -> io:put_chars(" |-- ")
            end
    end,
    case Dependencies of
        not_found ->
            io:format("~s [ NOT INSTALLED ]~n", [App]);
        _Deps ->
            io:format("~s~s~n", [App, type(Type)]),
            show_tree(Dependencies, [(Rest /= []) | Depth], Path)
    end,
    show_tree(Rest, Depth, Path).

type(normal) -> "";
type(include) -> " (included)".
