-task({"build:extendshell", "build extend shell commands"}).

run("build:extendshell", _) ->
    tetrapak:require("build:erlang"),
    tetrapak_task_shell:extended_shell_build().
