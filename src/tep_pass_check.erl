%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_pass_check).
-behaviour(tep_pass).

-include("tetrapak.hrl").

-export([pass_run/3]).

-passinfo({check, [
    {xref,       "Check inter-module calls"},
    {appmodules, "Check app file module list"}
]}).

%% ------------------------------------------------------------
%% -- Pass API
pass_run({check, xref}, Project, _Options) ->
    tep_pass:require("build:erlang"),
    EbinDir = filename:join(Project#tep_project.directory, "ebin"),
    case xref:d(EbinDir) of
        {error, Module, Reason} -> tep_pass:fail("xref error in ~p: ~p", [Module, Reason]);
        Result                  -> lists:foreach(fun xref_result/1, Result)
    end;

pass_run({check, appmodules}, Project, _Options) ->
    tep_pass:require("build:erlang"),
    check_modules(Project).

%% ------------------------------------------------------------
%% -- Implementation
check_modules(#tep_project{modules = Mods, directory = Dir}) ->
    Files = filelib:wildcard("*.beam", filename:join(Dir, "ebin")),
    {ShouldFiles, Dupli} = duplicates(lists:map(fun (M) -> atom_to_list(M) ++ ".beam" end, Mods)),
    BeamToMod = fun (List) -> lists:map(fun (F) -> lists:sublist(F, length(F) - 5) end, List) end,

    case length(Dupli) of
        0 -> ok;
        _ ->
            tep_pass:fail("duplicate modules in app file:~n   ~s", [string:join(BeamToMod(Dupli), ", ")])
    end,
    case ShouldFiles -- Files of
        [] -> ok;
        OnlyApp ->
            tep_pass:fail("modules listed in app file but not present in ebin/:~n   ~s", [string:join(BeamToMod(OnlyApp), ", ")])
    end,
    case Files -- ShouldFiles of
        [] -> ok;
        OnlyEbin ->
            tep_pass:fail("modules present in ebin/ but not listed in app file:~n   ~s", [string:join(BeamToMod(OnlyEbin), ", ")])
    end.

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
    tep_pass:fail("Undefined Functions called:~n~s", [fmt_functions(Functions)]);
xref_result({deprecated, Functions}) ->
    tep_log:warn("Deprecated Functions called:~n~s", [fmt_functions(Functions)]);
xref_result({unused, Functions}) ->
    tep_log:warn("Unused functions:~n~s", [fmt_functions(Functions)]).

fmt_functions(Functions) ->
    case Functions of
        [{T, _} | _] when is_tuple(T) ->
            %% detailed caller information is available
            {Output, _} = lists:foldr(
                fun ({{M2, F2, A2}, Call}, {Acc, LastCall}) ->
                    C1 = case Call of
                             LastCall     -> [];
                             {M1, F1, A1} -> io_lib:format("    ~p:~p/~p~n", [M1,F1,A1])
                         end,
                    C2 = io_lib:format("      by ~p:~p/~p~n", [M2,F2,A2]),
                    {[C1, C2 | Acc], Call}
                end, {[], undefined}, lists:keysort(2, Functions)),
            Output;
        _ ->
            %% no caller info
            lists:map(fun ({M, F, A}) -> io_lib:format("    ~p:~p/~p~n", [M,F,A]) end, Functions)
    end.
