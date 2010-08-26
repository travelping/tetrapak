% vim: filetype=erlang
{application, tetrapak,
 [{description, "Erlang application packaging tool"},
  {vsn, "0.2.1"},
  {modules, [tetrapak,
             tep_util,
             tep_log,
             tep_file,
             tep_ssh,
             tep_config,
             tep_publish,

             %% templates
             tetrapak_tpl_tarball,
             tetrapak_tpl_deb,
             tetrapak_tpl_deb_erlrc,

             %% repos
             tetrapak_repo_local,
             tetrapak_repo_ssh
    ]},
  {applications, [kernel, stdlib, crypto, ssh]},
  {registered, []}
]}.
