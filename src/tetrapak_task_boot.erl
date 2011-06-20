%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_boot).
-behaviour(tetrapak_task).
-export([initial_tmap/0, run/2]).

-include("tetrapak.hrl").

initial_tmap() ->
    [{["tetrapak", "boot"],    #task{name = "tetrapak:boot", module = ?MODULE}},
     {["tetrapak", "appdata"], #task{name = "tetrapak:appdata", module = ?MODULE}},
     {["tetrapak", "info"],    #task{name = "tetrapak:info", module = ?MODULE}}].

run("tetrapak:info", _) ->
    io:format("** version ~s~n~n", [tetrapak:get("tetrapak:appdata:version")]),
    Tasks = tetrapak_context:get_tasks(tetrapak_task:context()),
    io:format("Available Tasks~n~s", [show_tmap(Tasks)]);

run("tetrapak:appdata", _) ->
    AppFile = code:where_is_file("tetrapak.app"),
    {ok, [{application, tetrapak, Props}]} = file:consult(AppFile),
    Info = proplists:get_value(tetrapak, Props),
    {done, [{version,  proplists:get_value(vsn, Props)},
            {defaults, proplists:get_value(config, Info)}]};

run("tetrapak:boot", _) ->
    tetrapak_context:register_tasks(tetrapak_task:context(), builtin_tasks()),
    ok.

show_tmap(TMap) ->
    Lis = [{Task#task.name, Task#task.description} ||
             {Key, Task} <- lists:keysort(1, TMap),
             hd(Key) /= "tetrapak"], %% exclude internal tasks
    MaxWidth = lists:foldl(fun ({K, _}, Max) -> erlang:max(iolist_size(K), Max) end, 0, Lis),
    lists:foldr(fun ({Name, Desc}, Acc) ->
                   Space = lists:duplicate(MaxWidth - iolist_size(Name), $ ),
                   ["  " ++ Name  ++ "  " ++ Space ++ "- " ++ Desc ++ "\n" | Acc]
                end, [], Lis).

builtin_tasks() ->
    AppFile = code:where_is_file("tetrapak.app"),
    {ok, [{application, tetrapak, Props}]} = file:consult(AppFile),
    Tasks = proplists:get_value(tasks, proplists:get_value(tetrapak, Props, [])),
    lists:foldl(fun ({TaskName, Module, Desc}, Acc) ->
                        NewTask   = #task{name = tetrapak_task:normalize_name(TaskName),
                                          module = Module,
                                          description = Desc},
                        AddModule = fun (_) -> NewTask end,
                        pl_update(tetrapak_task:split_name(TaskName), AddModule, NewTask, Acc)
                end, [], Tasks).

pl_update(Key, AddItem, NewItem, Proplist) ->
    case proplists:get_value(Key, Proplist) of
        undefined -> [{Key, NewItem} | Proplist];
        Value     -> [{Key, AddItem(Value)} | proplists:delete(Key, Proplist)]
    end.
