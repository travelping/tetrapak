%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_common_test).
-behaviour(tetrapak_task).
-export([check/1, run/2]).

check("test:ct") ->
    filelib:is_dir(tetrapak:config_path("test.ct.srcdir"));

check("clean:testlog") ->
    filelib:is_dir(tetrapak:config_path("test.ct.logdir")).

run("test:ct", _) ->
    tetrapak:require("build"),
    LogDir = tetrapak:config_path("test.ct.logdir"),
    file:make_dir(LogDir),
    ct:run_test([{dir, tetrapak:config_path("test.ct.srcdir")},
                 {logdir, LogDir},
                 {suite, all},
                 {auto_compile, true},
                 {include, [tetrapak:subdir("include")]}]);

run("clean:testlog", _) ->
    tpk_file:delete(tetrapak:config_path("test.ct.logdir")).
