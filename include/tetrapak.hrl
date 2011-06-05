-record(task, {
    name             :: string(),
    modules          :: [atom()],
    description = "" :: string()
}).

-define(TASK_FAIL, '$__tetrapak_task_fail').
-define(TASK_DONE, '$__tetrapak_task_done').
