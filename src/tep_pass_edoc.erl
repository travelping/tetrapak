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

-export([pass_options/1, pass_run/3]).

-passinfo({global, [{doc, "Generate edoc documentation"}]}).
-passinfo({clean,  [{doc, "Delete generated documentation"}]}).

pass_options(_) -> [].

pass_run({global, doc}, #tep_project{name = AppName, directory = Dir}, _Options) ->
    DocDir = filename:join(Dir, "doc"),
    tep_file:mkdir(DocDir),
    edoc:application(AppName, Dir, [{dir, DocDir}]);

pass_run({clean, doc}, #tep_project{directory = Dir}, _Options) ->
    DocDir = filename:join(Dir, "doc"),
    tep_file:delete("(\\.(html|css|png)$)|edoc-info", DocDir).
