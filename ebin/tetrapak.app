% vim: filetype=erlang
{application, tetrapak,
 [{description, "Erlang application packaging tool"},
  {vsn, "0.1"},
  {modules, [tetrapak,
             tep_util,
             tep_log,

             %% templates
             tetrapak_tpl_tarball
    ]},
  {applications, [kernel, stdlib]},
  {registered, []}
]}.
