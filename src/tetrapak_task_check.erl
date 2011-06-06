%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_check).
-behaviour(tetrapak_task).
-export([run/2]).

-task({"check:xref", "Check inter-module calls"}).
-task({"check:appmodules", "Check app file module list"}).

%% ------------------------------------------------------------
%% -- Task API
run("check:xref", _) ->
    tetrapak:require("build:erlang"),
    EbinDir = tetrapak:subdir("ebin"),
    case xref:d(EbinDir) of
        {error, Module, Reason} -> tetrapak:fail("xref error in ~p: ~p", [Module, Reason]);
        Result                  -> lists:foreach(fun xref_result/1, Result)
    end;

run("check:appmodules", _) ->
    tetrapak:require("build:erlang"),
    Mods  = tetrapak:get("config:appfile:modules"),
    Files = filelib:wildcard("*.beam", tetrapak:subdir("ebin")),

    {ShouldFiles, Dupli} = duplicates(lists:map(fun (M) -> atom_to_list(M) ++ ".beam" end, Mods)),
    BeamToMod = fun (List) -> lists:map(fun (F) -> lists:sublist(F, length(F) - 5) end, List) end,

    case length(Dupli) of
        0 -> ok;
        _ ->
            tetrapak:fail("duplicate modules in app file:~n   ~s", [string:join(BeamToMod(Dupli), ", ")])
    end,
    case ShouldFiles -- Files of
        [] -> ok;
        OnlyApp ->
            tetrapak:fail("modules listed in app file but not present in ebin/:~n   ~s", [string:join(BeamToMod(OnlyApp), ", ")])
    end,
    case Files -- ShouldFiles of
        [] -> ok;
        OnlyEbin ->
            tetrapak:fail("modules present in ebin/ but not listed in app file:~n   ~s", [string:join(BeamToMod(OnlyEbin), ", ")])
    end.

%% ------------------------------------------------------------
%% -- Implementation
duplicates(List) -> duplicates(List, sets:new(), sets:new()).
duplicates([], Seen, Dupli) -> {sets:to_list(Seen), sets:to_list(Dupli)};
duplicates([Head|Tail], Seen, Dupli) ->
    case sets:is_element(Head, Seen) of
        true  -> duplicates(Tail, Seen, sets:add_element(Head, Dupli));
        false -> duplicates(Tail, sets:add_element(Head, Seen), Dupli)
    end.

xref_result({_, []}) ->
    ok;
xref_result({undefined, Functions}) ->
    io:format("Undefined Functions called:~n"),
    fmt_functions(Functions),
    tetrapak:fail();
xref_result({deprecated, Functions}) ->
    io:format("Deprecated Functions called:~n"),
    fmt_functions(Functions);
xref_result({unused, Functions}) ->
    io:format("Unused functions called:~n"),
    fmt_functions(Functions).

fmt_functions(Functions) ->
    case Functions of
        [{T, _} | _] when is_tuple(T) ->
            %% detailed caller information is available
            SortedFunctions = lists:keysort(2, Functions),
            lists:foldl(fun ({{M1, F1, A1}, ThisCall}, LastCall) ->
                                case ThisCall of
                                    LastCall     -> [];
                                    {M2, F2, A2} -> io:format("  ~p:~p/~p~n", [M2,F2,A2])
                                end,
                                io:format("    by ~p:~p/~p~n", [M1,F1,A1])
                        end, undefined, SortedFunctions);
        _ ->
            %% no caller info
            lists:foreach(fun ({M, F, A}) -> io:format("  ~p:~p/~p~n", [M,F,A]) end, Functions)
    end.
