-module(tetrapak_task_info).
-export([run/2]).

%% ------------------------------------------------------------
%% --

run("info:depsastree", _) ->
    tetrapak:require("build:erlang"),
    start_deps_get_deps_as_tree(tetrapak:get("config:appfile:name"));

run("info:depsaslist", _) ->
    tetrapak:require("build:erlang"),
    start_deps_get_deps_as_list(tetrapak:get("config:appfile:name")).

start_deps_get_deps_as_tree(App) ->
    Tree = start_deps_get_deps_first(false, App, tree_fun()),
    io:format(user, "application tree: ~p~n", [Tree]),
    {done, []}.

start_deps_get_deps_as_list(App) ->
    List = start_deps_get_deps_first(true, App, list_fun()),
    io:format(user, "application start list: ~p~n", [lists:reverse(List)]),
    {done, []}.

start_deps_get_deps_first(Unique, App, Fun) ->
    try start_deps_get_deps(Unique, App, Fun) of
        {Result, _StartedApps} ->
            Result
    catch
        throw:{failed, Error} ->
            Error
    end.

start_deps_get_deps(Unique, App, Fun) ->
    start_deps_get_deps(Unique, App, Fun, Fun(init, App)).

start_deps_get_deps(Unique, App, Fun, FunState) ->
    start_deps_get_deps(Unique, App, Fun, FunState, [kernel, stdlib]).
start_deps_get_deps(Unique, App, Fun, FunState, StartedApps) ->
    application:load(App),
    case application:get_key(App, applications) of
        {ok, Deps} ->
            NotStartedDeps = check_started(Deps, StartedApps),
            {StartedDeps, NewStartedApps2} =
                lists:foldl(fun(DepApp, {StartedDeps, OldStartedApps}) ->
                                    case lists:member(DepApp, OldStartedApps) and Unique of
                                        false ->
                                            {NewDeps, NewStartedApps1} =
                                                start_deps_get_deps(Unique, DepApp, Fun, Fun(init, DepApp), OldStartedApps),
                                            {[NewDeps | StartedDeps], NewStartedApps1};
                                        true ->
                                            {StartedDeps, OldStartedApps}
                                    end
                            end, {[], StartedApps}, NotStartedDeps),
            %% This is not a same function, because it can be a recursion function, that doesn't
            %% try to start a depencies at the same time
            {Fun(ready, Fun({add_dep, StartedDeps}, FunState)), [App | NewStartedApps2]};
        Error ->
            throw({failed, {App, Error}})
    end.

check_started(Deps, StartedApps) ->
    [Dep || Dep <- Deps, (not lists:member(Dep, StartedApps)) ].

tree_fun() ->
    fun (init, App) ->
            {App, []};
        ({add_dep, DepApp}, {App, Deps}) ->
            {App, lists:flatten([DepApp | Deps])};
        (ready, {_App, _Deps} = State) ->
            State;
        (Command, State) ->
            io:format(user, "command: ~p~n state: ~p~n", [Command, State])
    end.

list_fun() ->
    fun (init, App) ->
            [App];
        ({add_dep, DepApp}, Deps) ->
            lists:flatten(Deps ++ [DepApp]);
        (ready, Deps) ->
            Deps;
        (Command, State) ->
            io:format(user, "command: ~p~n state: ~p~n", [Command, State])
    end.
