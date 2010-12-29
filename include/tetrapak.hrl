-record(tep_project, {name, vsn, deps, desc, modules, app_file, directory}).
-record(tep_job, {template, files, template_dir, source_dir, output_dir}).

-record(tep_repository, {name, type, options}).

%% used for pass options
-record(option, {
    name             :: atom(),
    type             :: atom(),
    default          :: term(),
    required = false :: term()
}).

-record(pass, {
    name             :: atom(),
    group            :: atom(),
    module           :: atom(),
    fullname         :: string(),
    description = "" :: string()
}).

-record(pass_group, {
    name    :: atom(),
    members :: list()
}).
