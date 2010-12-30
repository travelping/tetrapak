% vim: filetype=erlang
{application, tetrapak,
 [{description, "Erlang application packaging tool"},
  {vsn, "0.2.3"},
  {modules, [tetrapak,
             tep_util,
             tep_log,
             tep_file,
             tep_ssh,
             tep_config,
             tep_publish,
             tep_pass,

             %% templates
             tetrapak_tpl_tarball,
             tetrapak_tpl_deb,
             tetrapak_tpl_deb_erlrc,

             %% passes
             tep_pass_compile,
             tep_pass_check,
             tep_pass_package,
             tep_pass_edoc,

             %% repos
             tetrapak_repo_local,
             tetrapak_repo_ssh
    ]},
  {applications, [kernel, stdlib, crypto, ssh, edoc, tools]},
  {registered, []}
]}.
