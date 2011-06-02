-record(task, {
    name             :: string(),
    modules          :: [atom()],
    description = "" :: string()
}).
