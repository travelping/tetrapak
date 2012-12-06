-module(tetrapak_task_info).
-export([run/2]).
-define(STDLIBS, [kernel, stdlib]).
-include("tetrapak.hrl").
% --------------------------------------------------------------------------------------------------
% -- Runs

run("info:deps", _) ->
    tetrapak:require("build:erlang"),
    start_deps_get_deps_as_list(tetrapak:get("config:appfile:name"));

run("info:deps:tree", _) ->
    tetrapak:require("build:erlang"),
    start_deps_get_deps_as_tree(tetrapak:get("config:appfile:name")).
% --------------------------------------------------------------------------------------------------
% -- Implementation

start_deps_get_deps_as_tree(App) ->
    Tree = start_deps_get_deps_first(App, tree_fun()),
    show_tree([Tree], []).

start_deps_get_deps_as_list(App) ->
    List = start_deps_get_deps_first(App, list_fun()),
    io:format(user, "application start list: ~p~n", [List]).

start_deps_get_deps_first(App, Fun) ->
    try start_deps_get_deps(App, Fun) of
        {Result, _StartedApps} ->
            Result
    catch
        throw:{failed, Error} ->
            Error
    end.

start_deps_get_deps(App, Fun) ->
    start_deps_get_deps(App, Fun, ?STDLIBS).

start_deps_get_deps(App, Fun, StartedApps) ->
    case application:load(App) of
        Ok when ?CHECK_LOADED(Ok) == true ->
            start_deps_get_deps(App, Fun, StartedApps, Fun(init, App));
        _Error ->
            {Fun(failed, App), StartedApps}
    end.
start_deps_get_deps(App, Fun, StartedApps, FunState) ->
    {ok, Deps} = application:get_key(App, applications),
    {NewFunState, NewStartedApps2} =
        lists:foldl(fun(DepApp, {State, OldStartedApps}) ->
                            {NewDepApp, NewStartedApps1} =
                                start_deps_get_deps(DepApp, Fun, OldStartedApps),
                            case lists:member(DepApp, OldStartedApps) of
                                false ->
                                    {Fun({add_dep, started, NewDepApp}, State), NewStartedApps1};
                                true ->
                                    {Fun({add_dep, not_started, NewDepApp}, State), OldStartedApps}
                            end
                    end, {FunState, StartedApps}, Deps -- ?STDLIBS),
    %% This is not a same function, because it can be a recursion function, that doesn't
    %% try to start a depencies at the same time
    {Fun(ready, NewFunState), [App | NewStartedApps2]}.

tree_fun() ->
    fun
        (init, App)                             -> {App, []};
        (failed, App)                           -> {App, not_found};
        ({add_dep, _Type, DepApp}, {App, Deps}) -> {App, [DepApp | Deps]};
        (ready, {_App, _Deps} = State)          -> State
    end.

list_fun() ->
    fun
        (init, App)                             -> [App];
        (failed, App)                           ->
            tetrapak:fail("Dependency ~s is not installed.~n", [App]);
        ({add_dep, not_started, _DepApp}, Deps) -> Deps;
        ({add_dep, started, DepApp}, Deps)      -> lists:flatten(Deps ++ [DepApp]);
        (ready, [Dep | Deps])                   -> Deps ++ [Dep]
    end.

show_tree([], _Depth) ->
    ok;
show_tree([{App, Dependencies} | Rest], Depth) ->
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
            io:format("~s [NOT INSTALLED]~n", [App]);
        _Deps ->
            io:format("~s~n", [App]),
            show_tree(Dependencies, [(Rest /= []) | Depth])
    end,
    show_tree(Rest, Depth).
