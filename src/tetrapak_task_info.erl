-module(tetrapak_task_info).
-export([run/2]).
%%  App
%%   | ->
%%   | ->
%%
%% ------------------------------------------------------------
%% --
-define(STDLIBS, [kernel, stdlib]).

run("info:deps", _) ->
    tetrapak:require("build:erlang"),
    start_deps_get_deps_as_tree(tetrapak:get("config:appfile:name"));

run("info:deps:list", _) ->
    tetrapak:require("build:erlang"),
    start_deps_get_deps_as_list(tetrapak:get("config:appfile:name")).

start_deps_get_deps_as_tree(App) ->
    Tree = start_deps_get_deps_first(App, tree_fun()),
    print_tree(Tree),
    {done, []}.

print_tree(Tree) ->
    print_tree("*>", Tree).

print_tree(Ext, {App, Deps}) ->
    io:format("~s ~p~n", [Ext, App]),
    [print_tree("    " ++ Ext, Dep) || Dep <- Deps].

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
    start_deps_get_deps(App, Fun, Fun(init, App)).

start_deps_get_deps(App, Fun, FunState) ->
    start_deps_get_deps(App, Fun, FunState, ?STDLIBS).
start_deps_get_deps(App, Fun, FunState, StartedApps) ->
    application:load(App),
    case application:get_key(App, applications) of
        {ok, Deps} ->
            {NewFunState, NewStartedApps2} =
                lists:foldl(fun(DepApp, {State, OldStartedApps}) ->
                                    {NewDepApp, NewStartedApps1} =
                                        start_deps_get_deps(DepApp, Fun, Fun(init, DepApp), OldStartedApps),
                                    case lists:member(DepApp, OldStartedApps) of
                                        false ->
                                            {Fun({add_dep, started, NewDepApp}, State), NewStartedApps1};
                                        true ->
                                            {Fun({add_dep, not_started, NewDepApp}, State), OldStartedApps}
                                    end
                            end, {FunState, StartedApps}, Deps -- ?STDLIBS),
            %% This is not a same function, because it can be a recursion function, that doesn't
            %% try to start a depencies at the same time
            {Fun(ready, NewFunState), [App | NewStartedApps2]};
        Error ->
            throw({failed, {App, Error}})
    end.

tree_fun() ->
    fun (init, App) ->
            {App, []};
        ({add_dep, _Type, DepApp}, {App, Deps}) ->
            {App, [DepApp | Deps]};
        (ready, {_App, _Deps} = State) ->
            State;
        (Command, State) ->
            io:format(user, "command: ~p~n state: ~p~n", [Command, State])
    end.

list_fun() ->
    fun (init, App) ->
            [App];
        ({add_dep, not_started, _DepApp}, Deps) ->
            Deps;
        ({add_dep, started, DepApp}, Deps) ->
            lists:flatten(Deps ++ [DepApp]);
        (ready, [Dep | Deps]) ->
            Deps ++ [Dep];
        (Command, State) ->
            io:format(user, "command: ~p~n state: ~p~n", [Command, State])
    end.
