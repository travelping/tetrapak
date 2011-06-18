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
  {registered, [tetrapak_io]},
  {tetrapak, [
    {tasks, [
      {"doc:edoc", tetrapak_task_doc, "Generate edoc documentation"},
      {"clean:edoc", tetrapak_task_doc, "Delete generated documentation"},
      {"check:xref", tetrapak_task_check, "Check inter-module calls"},
      {"check:appmodules", tetrapak_task_check, "Check app file module list"},
      {"config:appfile", tetrapak_task_config, "Read the application resource file"},
      {"config:ini", tetrapak_task_config, "Read the tetrapak config file"},
      {"build:erlang", tetrapak_task_erlc, "Compile Erlang modules"},
      {"build:yecc", tetrapak_task_erlc, "Compile yecc parsers (.yrl) to Erlang"},
      {"build:leex", tetrapak_task_erlc, "Compile lexical analysers (.xrl) to Erlang"},
      {"clean:erlang", tetrapak_task_erlc, "Delete compiled Erlang modules"},
      {"pkg:deb", tetrapak_task_pkg_deb, "Create a binary debian package"},
      {"clean:pkg:deb", tetrapak_task_pkg_deb, "Delete debian packages"},
      {"shell", tetrapak_task_shell, "Start the Erlang shell"}
    ]}]}
]}.
