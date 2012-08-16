-module(tetrapak_user_default).
-compile([export_all]).
-import(io, [format/1]).

help() ->
     shell_default:help(),
     format("** tetrapak extended commands **~n"),
     format("load()     -- reloads changed modules\n"),
     format("start()    -- starts the current application\n"),
     format("start(App) -- starts an application and all its dependencies\n"),
     format("bd()       -- runs \"build\"\n"),
     format("bdl()      -- runs \"build\" and reloads modules\n"),
     format("dbg(M)     -- enable dbg tracer on all functions in module M\n"),
     format("dbg(M, F)  -- enable dbg tracer on M:F functions\n"),
     format("dbgloc(M)  -- enable dbg tracer on all local functions in module M\n"),
     format("dbgdel(M)  -- disable call tracer for module M\n"),
     format("dbgdel(M,F)-- disable call tracer for function M:F\n"),
     format("dbgoff()   -- disable dbg tracer (calls dbg:stop_clear/0) to delete all debug information\n").

load()        -> run(["tetrapak:reload"]).
bd()          -> run(["build"]).
bdl()         -> run(["build", "tetrapak:reload"]).
start()       -> run(["tetrapak:startapp"]).
start(App)    -> tetrapak_task_shell:start_deps(App).

dbg(Module) ->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tp(Module, [{'_',[],[{return_trace}]}]),
    ok.

dbgloc(Module) ->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
    ok.

dbg(Module, Fun) ->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace},{exception_trace}]}]),
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
