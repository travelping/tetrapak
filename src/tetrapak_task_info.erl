-module(tetrapak_task_info).
-export([run/2]).
-define(STDLIBS, [kernel, stdlib]).

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
    print_tree(Tree),
    {done, []}.

start_deps_get_deps_as_list(App) ->
    List = start_deps_get_deps_first(App, list_fun()),
    io:format(user, "application start list: ~p~n", [List]),
    {done, []}.

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
        ok ->
            start_deps_get_deps(App, Fun, StartedApps, Fun(init, App));
        {error, {already_loaded, _}} ->
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
        (failed, App)                           -> {App, failed};
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

print_tree(Tree) ->
    print_tree("*>", Tree).

print_tree(Ext, {App, Deps}) ->
    io:format("~s ~p~n", [Ext, App]),
    [print_tree("    " ++ Ext, Dep) || Dep <- Deps].

