%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_pass_sanity).
-behaviour(tep_pass).
-pass_name(sanity).

-include("tetrapak.hrl").

-export([pass_options/0, pass_run/2]).

%% ------------------------------------------------------------
%% -- Pass API
pass_options() ->
    [#option{name = xref,
             type = bool,
             default = true},
     #option{name = app_modules,
             type = bool,
             default = true}
    ].

pass_run(Project, Options) ->
    tep_pass:require(compile),

    case proplists:get_value(xref, Options) of
        true  -> run_xref(Project);
        false -> tep_log:debug("skipping XREF check...");
        undefined -> ok
    end,

    case proplists:get_value(app_modules, Options) of
        true  -> check_modules(Project);
        false -> tep_log:debug("skipping app_modules check...");
        undefined -> ok 
    end.

%% ------------------------------------------------------------
%% -- Implementation
run_xref(_Project) ->
    tep_pass:fail("XREF not_implemented").

check_modules(#tep_project{modules = Mods, directory = Dir}) ->
    Files = filelib:wildcard("*.beam", filename:join(Dir, "ebin")),
    {ShouldFiles, Dupli} = duplicates(lists:map(fun (M) -> atom_to_list(M) ++ ".beam" end, Mods)),
    BeamToMod = fun (List) -> lists:map(fun (F) -> lists:sublist(F, length(F) - 5) end, List) end,

    case length(Dupli) of
        0 -> ok;
        _ ->
            tep_pass:fail("duplicate modules in app file: ~s", [string:join(BeamToMod(Dupli), ", ")])
    end,
    case ShouldFiles -- Files of
        [] -> ok;
        OnlyApp ->
            tep_pass:fail("modules listed in app file but not present in ebin/: ~s", [string:join(BeamToMod(OnlyApp), ", ")])
    end,
    case Files -- ShouldFiles of
        [] -> ok;
        OnlyEbin ->
            tep_pass:fail("modules present in ebin/ but not listed in app file: ~s", [string:join(BeamToMod(OnlyEbin), ", ")])
    end.

duplicates(List) -> duplicates(List, sets:new(), sets:new()).
duplicates([], Seen, Dupli) -> {sets:to_list(Seen), sets:to_list(Dupli)};
duplicates([Head|Tail], Seen, Dupli) ->
    case sets:is_element(Head, Seen) of
        true  -> duplicates(Tail, Seen, sets:add_element(Head, Dupli));
        false -> duplicates(Tail, sets:add_element(Head, Seen), Dupli)
    end.
