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
        {error, Module, Reason} ->
            tep_pass:fail("xref error in ~p: ~p", [Module, Reason]);
        Result ->
            case proplists:get_value(undefined, Result) of
                []    -> ok;
                Undef ->
                    Listing = lists:foldl(fun ({M,F,A}, Acc) -> tep_util:f("~s~n    ~s:~s/~b", [Acc, M, F, A]) end, "", Undef),
                    tep_pass:fail("Undefined functions:~s", [Listing])
            end
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
