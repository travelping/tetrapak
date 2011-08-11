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

-module(tetrapak_task_shell).
-behaviour(tetrapak_task).
-export([run/2]).

run("shell", _) ->
    code:ensure_loaded(tpk),
    case tetrapak_io:can_start_shell() of
        true ->
            TestDir = tetrapak:config_path("test.ct.srcdir"),
            case filelib:is_dir(TestDir) of
                true  -> code:add_pathz(TestDir);
                false -> ok
            end,

            tetrapak:require("tetrapak:reload"),
            tetrapak_task:print_output_header(user, "shell"),
            tetrapak_io:start_shell(),

            timer:sleep(infinity);
        false ->
            tetrapak:fail("Cannot start shell")
    end;

run("tetrapak:reload", _) ->
    EbinDir = tetrapak:subdir("ebin"),
    Modules = lists:map(fun (F) -> {list_to_atom(filename:basename(F, ".beam")), filename:join(EbinDir, F)} end, filelib:wildcard("*.beam", EbinDir)),
    lists:foreach(fun ({Mod, Path}) ->
                          case code:is_loaded(Mod) of
                              {file, _} ->
                                  DiskMtime = tpk_file:mtime(Path),
                                  (DiskMtime > loaded_mtime(Mod)) andalso load(Mod);
                              false ->
                                  load(Mod)
                          end
                  end, Modules);

run("tetrapak:tpk-help", _) ->
    io:format("tpk:r(Task)  -- runs the given task\n"
              "tpk:h()      -- shows this help\n"
              "tpk:l()      -- reloads changed modules\n"
              "tpk:s()      -- starts the current application\n"
              "tpk:c()      -- runs \"check\"\n"
              "tpk:b()      -- runs \"build\"\n"
              "tpk:bl()     -- runs \"build\" and reloads modules\n");

run("tetrapak:startapp", _) ->
    start_deps(tetrapak:get("config:appfile:name")).

start_deps(App) ->
    case application:start(App) of
        ok ->
            io:format("Started ~s~n", [App]),
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, DepApp}} ->
            start_deps(DepApp),
            start_deps(App);
        {error, _Error} ->
            tetrapak:fail("failed to start ~s", [App])
    end.

loaded_mtime(Mod) ->
    {Y,Mon,D,H,Min,S} = proplists:get_value(time, Mod:module_info(compile)),
    calendar:universal_time_to_local_time({{Y,Mon,D},{H,Min,S}}).

load(Mod) ->
    io:format("load ~s~n", [Mod]),
    code:purge(Mod),
    code:load_file(Mod).
