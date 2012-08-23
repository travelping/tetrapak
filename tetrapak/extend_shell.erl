-task({"build:extend:shell", "build extend shell commands"}).

run("build:extend:shell", _) ->
    tetrapak:require("build:erlang"),
    case tetrapak_task_shell:extended_shell_build() of
        error ->
            io:format("Cann't compile tetrapak shell extension~n", []);
        _ ->
            io:format("Recompile: tetrapak shell extensions~n", [])
    end.
