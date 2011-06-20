-record(task, {
    name             :: string(),
    module           :: atom(),
    description = "" :: string()
}).

-define(TASK_FAIL, '$__tetrapak_task_fail').
-define(TASK_DONE, '$__tetrapak_task_done').
