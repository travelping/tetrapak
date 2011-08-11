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

-module(tetrapak_task_doc).
-behaviour(tetrapak_task).
-export([check/1, run/2]).

check("clean:edoc") ->
    filelib:is_dir(tetrapak:config_path("edoc.outdir")).

run("doc:edoc", _) ->
    case tetrapak:config("edoc.pretty_print") of
        true  -> PPOpts = [{pretty_printer, erl_pp}];
        false -> PPOpts = []
    end,

    DD = tetrapak:config_path("edoc.outdir"),
    tpk_file:mkdir(DD),
    edoc:application(tetrapak:get("config:appfile:name"),
                     tetrapak:dir(),
                     [{dir, DD},
                      {includes, [tetrapak:subdir("include")]},
                      {private, tetrapak:config("edoc.private")},
                      {hidden, tetrapak:config("edoc.hidden")},
                      {todo, tetrapak:config("edoc.todo")},

                      %% layout options
                      {sort_functions, tetrapak:config("edoc.sort_functions")}
                        | PPOpts]);

run("clean:edoc", _) ->
    DD = tetrapak:config_path("edoc.outdir"),
    tpk_file:delete("(\\.(html|css|png)$)|edoc-info", DD),
    file:del_dir(DD),
    ok.
