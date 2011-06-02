% vim: filetype=erlang
{application, tetrapak,
 [{description, "Erlang application packaging tool"},
  {vsn, "0.2.3"},
  {modules, [tetrapak,
             tep_util,
             tep_log,
             tep_file,
             tep_pass,
             tep_context,

             %% passes
             tep_config,
             tep_pass_compile,
             tep_pass_check,
             tep_pass_package_debian,
             tep_pass_edoc
    ]},
  {applications, [kernel, stdlib, crypto, edoc, tools]},
  {registered, []}
]}.
