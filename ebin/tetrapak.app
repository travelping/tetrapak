% vim: filetype=erlang
{application, tetrapak,
 [{description, "Erlang application packaging tool"},
  {vsn, "0.2.3"},
  {modules, [tetrapak,
             tetrapak_context,
             tetrapak_task,
             tpk_util,
             tpk_file,
             tpk_log,

             %% passes
             tetrapak_task_config,
             tetrapak_task_erlc,
             tetrapak_task_check,
             tetrapak_task_pkg_deb,
             tetrapak_task_doc
    ]},
  {applications, [kernel, stdlib, compiler, edoc, tools]},
  {registered, []}
]}.
