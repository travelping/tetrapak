% Copyright 2010-2011, Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

-module(tetrapak_task_common_test).
-behaviour(tetrapak_task).
-export([check/1, run/2]).

check("test:ct") ->
    filelib:is_dir(tetrapak:config_path("test.ct.srcdir"));

check("clean:testlog") ->
    filelib:is_dir(tetrapak:config_path("test.ct.logdir"));

check("clean:ct") ->
    SrcDir = tetrapak:path("test"),
    tpk_util:check_files_exist(SrcDir, ".beam", SrcDir, ".erl").

run("test:ct", _) ->
    tetrapak:require("build"),
    LogDir = tetrapak:config_path("test.ct.logdir"),
    file:make_dir(LogDir),
    CoverSpec = case tetrapak:config("test.ct.coverspec") of
		    none -> [];
		    Spec when is_list(Spec) ->
			[{cover, tetrapak:path(Spec)}];
		    Spec ->
			tetrapak:fail("invalid cover spec file: ~s", [Spec])
		end,
    Result = ct:run_test([{dir, tetrapak:config_path("test.ct.srcdir")},
			  {logdir, LogDir},
			  {suite, tetrapak:config("test.ct.suite")},
			  {auto_compile, true},
			  {include, [tetrapak:path("include")]}|CoverSpec]),
    case Result of
	ok ->
	    %% undocumented case, seems to happen when test suite compilation fails for all suites
	    ok;
	{error, _} ->
	    Result;
	{_Ok, Failed, {_UserSkipped, _AutoSkipped}}
	  when Failed /= 0 ->
	    tetrapak_task:fail("ct: ~p tests failed", [Failed]);
	{_Ok, Failed, {_UserSkipped, _AutoSkipped}}
	  when Failed == 0 ->
	    ok
    end;

run("clean:testlog", _) ->
    tpk_file:delete(tetrapak:config_path("test.ct.logdir"));

run("clean:ct", _) ->
    tpk_file:delete("\\.beam$", tetrapak:path("test")).
