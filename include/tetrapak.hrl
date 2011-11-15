-record(task, {
    name              :: string(),
    module            :: atom(),
    description = ""  :: string(),
    origin = builtin  :: local | library | builtin,
    pre_hooks = []    :: [string(), ...],
    post_hooks = []   :: [string(), ...],
    must_run_before = [] :: [string(), ...],
    must_run_after  = [] :: [string(), ...]
}).

-record(config, {
    objects = [] :: [{{string(), string()}, [{string(), term()}]}],
    values  = [] :: [{string(), term()}]
}).

-define(TASK_FAIL, '$__tetrapak_task_fail').
-define(TASK_DONE, '$__tetrapak_task_done').
-define(LOCAL_CACHE, ".local.cache").

-define(DEBUG(Msg), tpk_util:debug_log_to_stderr(Msg, [])).
-define(DEBUG(Fmt, Args), tpk_util:debug_log_to_stderr(Fmt, Args)).

% -define(DEBUG(Msg), io:format(standard_error, "~p: " Msg "~n", [self()])).
% -define(DEBUG(Fmt, Args), io:format(standard_error, "~p: " Fmt "~n", [self() | Args])).