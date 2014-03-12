% Copyright 2010-2011, Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

-module(tetrapak_task_check).
-behaviour(tetrapak_task).
-export([run/2]).

-define(EXTENSION, "user_default.beam").
%% ------------------------------------------------------------
%% -- Task API
run("check:xref", _) ->
    tetrapak:require_all(["build:erlang", "tetrapak:load"]),
    EbinDir = tetrapak:path("ebin"),
    case xref:d(EbinDir) of
        {error, Module, Reason} -> tetrapak:fail("xref error in ~p: ~p", [Module, Reason]);
        Result                  -> lists:foreach(fun xref_result/1, Result)
    end;

run("check:appmodules", _) ->
    tetrapak:require("build:erlang"),
    Mods  = tetrapak:get("config:appfile:modules"),
    Files = filelib:wildcard("*.beam", tetrapak:path("ebin")),

    {ShouldFiles, Dupli} = duplicates(lists:map(fun (M) -> atom_to_list(M) ++ ".beam" end, Mods)),
    BeamToMod = fun (List) -> lists:map(fun (F) -> lists:sublist(F, length(F) - 5) end, List) end,

    case length(Dupli) of
        0 -> ok;
        _ ->
            tetrapak:fail("duplicate modules in app file:~n   ~s", [string:join(BeamToMod(Dupli), ", ")])
    end,
    case ShouldFiles -- Files of
        [] -> ok;
       [?EXTENSION] -> ok;
        OnlyApp ->
            tetrapak:fail("modules listed in app file but not present in ebin/:~n   ~s", [string:join(BeamToMod(OnlyApp), ", ")])
    end,
    case Files -- ShouldFiles of
        [] -> ok;
        OnlyEbin ->
            tetrapak:fail("modules present in ebin/ but not listed in app file:~n   ~s", [string:join(BeamToMod(OnlyEbin), ", ")])
    end;

run("check:packageable", _) ->
    %IgnorePlugin = [tetrapak:extract_app_name(App, "tetrapak_") || App <- tetrapak:config("tetrapak.ignore_plugins", [])],
    ExtraBuildApps = [tetrapak:extract_app_name(App, "tetrapak_") || App <- tetrapak:config("tetrapak.plugins", [])],
    case (tetrapak:get("tetrapak:boot:sbplugins") -- ExtraBuildApps) of
        [] -> ok;
        Deps ->
            tetrapak:fail("needed plugins are not included in tetrapak.plugins~n   ~p", [[string:sub_string(atom_to_list(Dep), 10) || Dep <- Deps]])
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

    UndefAllowed = tetrapak:config("xref.ignore_undef"),
    DontFail     = lists:all(fun ({_, {M, F, A}}) -> lists:member({M, F, A}, UndefAllowed);
                                 ({M, F, A})      -> lists:member({M, F, A}, UndefAllowed)
                             end, Functions),
    DontFail orelse tetrapak:fail("xref error");
xref_result({deprecated, Functions}) ->
    io:format("Deprecated Functions called:~n"),
    fmt_functions(Functions);
xref_result({unused, Functions}) ->
    io:format("Unused functions:~n"),
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
                                io:format("    by ~p:~p/~p~n", [M1,F1,A1]),
                                ThisCall
                        end, undefined, SortedFunctions);
        _ ->
            %% no caller info
            lists:foreach(fun ({M, F, A}) -> io:format("  ~p:~p/~p~n", [M,F,A]) end, Functions)
    end.
