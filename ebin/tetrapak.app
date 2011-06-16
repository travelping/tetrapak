% vim: filetype=erlang
{application, tetrapak,
 [{description, "Erlang application packaging tool"},
  {vsn, "0.2.3"},
  {modules, [tetrapak,
             tetrapak_context,
             tetrapak_task,
             tetrapak_cli,
             tetrapak_io,
             tpk_util,
             tpk_file,
             tpk_log,

             %% tasks
             tetrapak_task_config,
             tetrapak_task_erlc,
             tetrapak_task_check,
             tetrapak_task_pkg_deb,
             tetrapak_task_doc,
             tetrapak_task_shell
    ]},
  {applications, [kernel, stdlib, compiler, edoc, tools]},
  {registered, [tetrapak_io]}
]}.
