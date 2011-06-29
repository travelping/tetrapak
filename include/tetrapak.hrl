-record(task, {
    name             :: string(),
    module           :: atom(),
    description = "" :: string(),
    origin = builtin :: local | library | builtin
}).

-define(TASK_FAIL, '$__tetrapak_task_fail').
-define(TASK_DONE, '$__tetrapak_task_done').
-define(LOCAL_CACHE, ".local.cache").

-define(DEBUG(Msg), tpk_util:debug_log_to_stderr(Msg, [])).
-define(DEBUG(Fmt, Args), tpk_util:debug_log_to_stderr(Fmt, Args)).
