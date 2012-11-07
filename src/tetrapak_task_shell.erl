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
-export([run/2, start_deps/1, extended_shell_build/0]).

run("shell", _) ->
    start_shell();

run("start:dev", _) ->
    tetrapak:require("tetrapak:startapp"),
    start_shell();

run("tetrapak:extend:shell", _) ->
    load_user_default();

run("tetrapak:reload", _) ->
    EbinDir = tetrapak:path("ebin"),
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
              "tpk:s(App)   -- starts an application and all its dependencies\n"
              "tpk:c()      -- runs \"check\"\n"
              "tpk:b()      -- runs \"build\"\n"
              "tpk:bl()     -- runs \"build\" and reloads modules\n");

run("tetrapak:startapp", _) ->
    case start_deps(tetrapak:get("config:appfile:name")) of
        ok ->
            done;
        {failed, App, Error} ->
            tetrapak:fail("failed to start ~s: ~p", [App, Error])
    end.

start_shell() ->
    code:ensure_loaded(tpk),
    case tetrapak_io:can_start_shell() of
        true ->
            TestDir = tetrapak:config_path("test.ct.srcdir"),
            case filelib:is_dir(TestDir) of
                true  -> code:add_pathz(TestDir);
                false -> ok
            end,

            tetrapak:require("tetrapak:extend:shell"),
            tetrapak:require("tetrapak:reload"),
            tetrapak_task:print_output_header(user, "shell"),
            tetrapak_io:start_shell(),

            timer:sleep(infinity);
        false ->
            tetrapak:fail("Cannot start shell")
    end.

start_deps(App) ->
    case application:start(App) of
        ok ->
            io:format("Started ~s~n", [App]),
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, DepApp}} ->
            case start_deps(DepApp) of
                ok ->
                    start_deps(App);
                Error ->
                    Error
            end;
        {error, Error} ->
            {failed, App, Error}
    end.

loaded_mtime(Mod) ->
    {Y,Mon,D,H,Min,S} = proplists:get_value(time, Mod:module_info(compile)),
    calendar:universal_time_to_local_time({{Y,Mon,D},{H,Min,S}}).

load(Mod) ->
    io:format("load ~s~n", [Mod]),
    code:purge(Mod),
    code:load_file(Mod).

% --------------------------------------------------------------------------------------------------
% -- use user_default hack
-record(tmod, {file, md5, module, code}).

load_user_default() ->
    case code:load_file(user_default) of
        {module, _} ->
            io:format("Loaded tetrapak extra shell commands~n");
        _ ->
            io:format("Cann't compile/load tetrapak extra shell commands~n", [])
    end.

extended_shell_build() ->
    case get_source() of
        error ->
            error;
        File ->
            TMod = compile_user_default(File),
            code:purge(user_default),
            code:load_binary(user_default, File, TMod#tmod.code),
            file:write_file(tetrapak:path("ebin") ++ "/user_default.beam", TMod#tmod.code),
            ok
    end.

get_source() ->
    File = proplists:get_value(source, tetrapak_shell_extension:module_info(compile)),
    case filelib:is_file(File) of
        true ->
            File;
        false ->
            error
    end.

compile_user_default(PathToFile) ->
    {PreDefMacros, ModuleName} = predef_macros(),
    case epp:parse_file(PathToFile, [], PreDefMacros) of
        {ok, Forms} ->
            TMod = process_forms(PathToFile, ModuleName, Forms),
            CompileOptions = [return, debug_info, export_all],
            case compile:forms(TMod#tmod.code, CompileOptions) of
                {ok, ModuleName, BeamCode, Warnings} ->
                    tetrapak_task_erlc:show_errors(tetrapak:dir(), "Warning: ", Warnings),
                    TMod#tmod{code = BeamCode};
                {error, Errors, Warnings} ->
                    tetrapak_task_erlc:show_errors(tetrapak:dir(), "Error: ", Errors),
                    tetrapak_task_erlc:show_errors(tetrapak:dir(), "Warning: ", Warnings),
                    tetrapak:fail("user_default compilation failed")
            end;
        {error, Error} ->
            tetrapak:fail("failed to open tetrapak_shell_extension ~s: ~s", [PathToFile, file:format_error(Error)])
    end.

predef_macros() ->
   PreDefMacros = [{'MODULE', user_default, redefine},
                   {'MODULE_STRING', "user_default", redefine}],
   {PreDefMacros, user_default}.

process_forms(File, ModuleName, Forms) ->
    {Found, TMod, Code} = lists:foldl(fun (Form, {FoundM, TMod, CodeAcc}) ->
                                              do_form(File, Form, FoundM, TMod, CodeAcc)
                                      end, {false, #tmod{module = ModuleName}, []}, Forms),
    case Found of
        true  -> TMod#tmod{code = lists:reverse(Code)};
        false -> TMod#tmod{code = [{attribute, 1, module, ModuleName} | lists:reverse(Code)]}
    end.

do_form(_File, {attribute, L, module, _ModName}, _FoundM, TMod, Code) ->
    {true, TMod, [{attribute, L, module, TMod#tmod.module} | Code]};
do_form(_File, OtherForm, FoundM, TMod, Code) ->
    {FoundM, TMod, [OtherForm | Code]}.
