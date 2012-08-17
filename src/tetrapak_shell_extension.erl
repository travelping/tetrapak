-module(tetrapak_shell_extension).
-compile([export_all]).
-import(io, [format/1]).

-define(STD_PARAMS, [{return_trace}, {exception_trace}]).

help() ->
     shell_default:help(),
     format("** tetrapak extended commands **~n"),
     format("l()        -- reloads changed modules\n"),
     format("start()    -- starts the current application\n"),
     format("start(App) -- starts an application and all its dependencies\n"),
     format("bl()       -- runs \"build\"\n"),
     format("bll()      -- runs \"build\" and reloads modules\n"),
     format("dbg(M)     -- enable dbg tracer on all functions in module M\n"),
     format("dbg(M, F)  -- enable dbg tracer on M:F functions\n"),
     format("dbgloc(M)  -- enable dbg tracer on all local functions in module M\n"),
     format("dbgdel(M)  -- disable call tracer for module M\n"),
     format("dbgdel(M,F)-- disable call tracer for function M:F\n"),
     format("dbgoff()   -- disable dbg tracer (calls dbg:stop_clear/0) to delete all debug information\n"),
     case assert_loaded_redbug() of
        true ->
            format("dbg(S)     -- enable dbg tracer with redbug RTP(restricted trace pattern)\n"),
            format("              Please note, that not all original patterns are supported\n"),
            format("              the RTP has the form: \"<mfa> when <guards>\"\n"),
            format("              \"mod\", \"mod:fun\", \"mod:fun/3\" or \"mod:fun('_',atom,X)\"\n"),
            format("              <guard> is something like:\n"),
            format("              \"X==1\" or \"is_atom(A)\" or \"(X==2) or (Y==2)\"\n");
        false ->
            ok
    end.


l()           -> run(["tetrapak:reload"]).
bl()          -> run(["build"]).
bll()         -> run(["build", "tetrapak:reload"]).
start()       -> run(["tetrapak:startapp"]).
start(App)    -> tetrapak_task_shell:start_deps(App).

dbg(Module) when is_atom(Module) ->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tp(Module, [{'_',[],[{return_trace}]}]),
    ok;

dbg(String) when is_list(String) ->
    NeededMod = redbug_msc,
    case assert_loaded_redbug() of
        true ->
            assert_dbg_started(),
            {{M, F, _}, [{Variables, Conditions, _}],_} = NeededMod:transform(String),
            dbg:tpl(M, F, [{Variables, Conditions, ?STD_PARAMS}]),
            ok;
        false ->
            format("redbug isn't installed")
    end.

dbgloc(Module) when is_atom(Module) ->
    assert_dbg_started(),
    dbg:tpl(Module, [{'_', [], ?STD_PARAMS}]),
    ok.

dbg(Module, Fun) ->
    assert_dbg_started(),
    dbg:tpl(Module, Fun, [{'_', [], ?STD_PARAMS }]),
    ok.

dbgdel(Module) ->
    dbg:ctpl(Module),
    ok.

dbgdel(Module, Fun) ->
    dbg:ctpl(Module, Fun),
    ok.

dbgoff() ->
    dbg:stop_clear().

% --------------------------------------------------------------------------------------------------
% -- helpers

run(Tasks) ->
    {ok, Cwd} = file:get_cwd(),
    tetrapak:run(Cwd, Tasks).

assert_loaded_redbug() ->
    case application:load(redbug) of
        ok ->
            true;
        {error, {already_loaded, _App}} ->
            true;
        _ ->
            false
    end.

assert_dbg_started() ->
    dbg:tracer(),
    dbg:p(all, c).
