%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_pass_edoc).
-behaviour(tep_pass).

-include("tetrapak.hrl").

-export([pass_run/3]).

-passinfo({doc,   [{edoc, "Generate edoc documentation"}]}).
-passinfo({clean, [{edoc, "Delete generated documentation"}]}).

pass_run({doc, edoc}, Project, Options) ->
    DocDir = doc_dir(Project, Options),
    tep_file:mkdir(DocDir),
    edoc:application(Project#tep_project.name, Project#tep_project.directory, [{dir, DocDir}]);

pass_run({clean, edoc}, Project, Options) ->
    DocDir = doc_dir(Project, Options),
    tep_file:delete("(\\.(html|css|png)$)|edoc-info", DocDir).

doc_dir(Project, Options) ->
    filename:join(Project#tep_project.directory, tep_config:get_string(Options, "doc.directory", "doc")).
