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

-module(tetrapak_task_boot).
-behaviour(tetrapak_task).
-export([initial_tmap/0, run/2]).

-include("tetrapak.hrl").

initial_tmap() ->
    [{["clean", "taskcache"], #task{name = "clean:taskcache", module = ?MODULE, description = "Delete the local task cache"}},
     {["tetrapak", "boot"],   #task{name = "tetrapak:boot", module = ?MODULE,   description = "The root of all evil"}},
     {["tetrapak", "info"],   #task{name = "tetrapak:info", module = ?MODULE,   description = "Show version and tasks"}}].

run("tetrapak:boot", _) ->
    Props   = load_appdata(),
    Version = proplists:get_value(vsn, Props, "UNKNOWN"),
    Env     = proplists:get_value(env, Props),

    %% configuration
    BaseConfig     = #config{values = proplists:get_value(config, Env)},
    ProjectConfig1 = read_config(project_config_path("config.ini"), BaseConfig),
    ProjectConfig2 = read_config(project_config_path("local.ini"), ProjectConfig1),

    CliOptions = getopt(config, o, 2),
    TheConfig  = add_cli_config(CliOptions, ProjectConfig2),

    tetrapak_context:import_config(tetrapak_task:context(), TheConfig),

    %% scan tasks
    tetrapak_context:register_tasks(tetrapak_task:context(), builtin_tasks(proplists:get_value(tasks, Env))),
    tetrapak_context:register_tasks(tetrapak_task:context(), scan_local_tasks(tetrapak:subdir("tetrapak"))),

    {done, [{version, Version}]};

run("tetrapak:info", _) ->
    io:format("** version ~s~n~n", [tetrapak:get("tetrapak:boot:version")]),
    Tasks = tetrapak_context:get_tasks(tetrapak_task:context()),
    io:format("Available Tasks~n~s", [show_tmap(Tasks)]);

run("clean:taskcache", _) ->
    tpk_file:delete(filename:join(tetrapak:subdir("tetrapak"), ?LOCAL_CACHE)),
    ok.

show_tmap(TMap) ->
    Lis = [{Task#task.name, Task#task.description, Task#task.origin} ||
             {Key, Task} <- lists:keysort(1, TMap),
             hd(Key) /= "tetrapak"], %% exclude internal tasks
    MaxWidth = lists:foldl(fun ({K, _, _}, Max) -> erlang:max(iolist_size(K), Max) end, 0, Lis),
    lists:foldr(fun ({Name, Desc, Origin}, Acc) ->
                        Space = lists:duplicate(MaxWidth - iolist_size(Name), $ ),
                        ["  " ++ Name  ++ "  " ++ Space ++ ostr(Origin) ++ " - " ++ Desc ++ "\n" | Acc]
                end, [], Lis).

ostr(builtin) -> " ";
ostr(local)   -> "*";
ostr(library) -> "+".

builtin_tasks(Tasks) ->
    lists:foldl(fun add_builtin_task/2, [], Tasks).

add_builtin_task({TaskName, Module, Desc}, Acc) ->
    add_builtin_task({TaskName, Module, Desc, []}, Acc);
add_builtin_task({TaskName, Module, Desc, Options}, Acc) ->
    NewTask1 = #task{name = tetrapak_task:normalize_name(TaskName),
                     module = Module,
                     description = Desc,
                     origin = builtin},
    NewTask2 = apply_hook_options(Options, NewTask1),
    AddModule = fun (_) -> NewTask2 end,
    pl_update(tetrapak_task:split_name(TaskName), AddModule, NewTask2, Acc);
add_builtin_task(TaskTuple, _Acc) ->
    tetrapak:fail("bad task definition tuple in application environment: ~n  ~p", [TaskTuple]).

apply_hook_options([{run_before, HookList} | Rest], Task) ->
    apply_hook_options(Rest, Task#task{must_run_before = HookList});
apply_hook_options([{run_after, HookList} | Rest], Task) ->
    apply_hook_options(Rest, Task#task{must_run_after = HookList});
apply_hook_options([Option | _], Task) ->
    tetrapak:fail("(task: ~s) unknown task option: ~p", [Task#task.name, Option]);
apply_hook_options([], Task) ->
    Task.

pl_update(Key, AddItem, NewItem, Proplist) ->
    case proplists:get_value(Key, Proplist) of
        undefined -> [{Key, NewItem} | Proplist];
        Value     -> [{Key, AddItem(Value)} | proplists:delete(Key, Proplist)]
    end.

load_appdata() ->
    case application:load(tetrapak) of
        {error, {already_loaded, tetrapak}} ->
            {ok, Props} = application:get_all_key(tetrapak);
        ok ->
            {ok, Props} = application:get_all_key(tetrapak);
        {error, _} ->
            %% fallback to .app.src
            Dir = filename:join([filename:dirname(code:which(?MODULE)), ".."]),
            AppSrcPath = filename:join([Dir, "src", "tetrapak.app.src"]),
            {ok, [{application, tetrapak, Props}]} = file:consult(AppSrcPath)
    end,
    Props.

%% --------------------------------------------------------------------------------
%% -- LOCAL TASKS

%% changing this record def will invalidate all caches
-record(tmod, {file, md5, module, tasks = [], hooks = [], includes = [], code}).

scan_local_tasks(Dir) ->
    case filelib:wildcard(filename:join(Dir, "*.erl")) of
        []    -> [];
        Files ->
            CachePath = filename:join(Dir, ?LOCAL_CACHE),
            CacheName = make_ref(),
            DetsOpts  = [{file, CachePath}, {repair, false}, {keypos, #tmod.file}],
            case dets:open_file(CacheName, DetsOpts) of
                {ok, CacheName} -> ok;
                {error, {needs_repair, CachePath}} ->
                    io:format("task cache corrupted, deleting it...~n", []),
                    tpk_file:delete(CachePath),
                    dets:open_file(CacheName, DetsOpts)
            end,
            TaskRecords = compile_and_load_local_tasks(Files, CacheName),
            dets:close(CacheName),
            TaskRecords
    end.

compile_and_load_local_tasks(Files, Cache) ->
    lists:flatmap(fun (File) ->
                          case dets:lookup(Cache, tpk_file:rebase_filename(File, tetrapak:dir(), "")) of
                              [TMod = #tmod{includes = Includes, md5 = DetsMd5}] ->
                                  case tpk_file:md5sum(File) of
                                      {ok, DetsMd5} ->
                                          case tetrapak_task_erlc:check_mtimes(tpk_file:mtime(File), Includes) of
                                              false ->
                                                  load_local_task(File, TMod);
                                              true ->
                                                  compile_and_load(File, Cache)
                                          end;
                                      _OtherMd5 ->
                                          compile_and_load(File, Cache)
                                  end;
                              [_StaleTmod] ->
                                  compile_and_load(File, Cache);
                              [] ->
                                  compile_and_load(File, Cache)
                          end
               end, Files).

compile_and_load(File, Cache) ->
    NewTMod = compile_local_task(File),
    dets:insert(Cache, NewTMod),
    load_local_task(File, NewTMod).

load_local_task(File, #tmod{module = ModuleName, tasks = Tasks, hooks = Hooks, code = Code}) ->
    code:purge(ModuleName),
    case code:load_binary(ModuleName, File, Code) of
        {module, ModuleName} ->
            lists:map(fun ({TN, TD}) ->
                              Key   = tetrapak_task:split_name(TN),
                              Task1 = #task{module = ModuleName, name = TN, description = TD, origin = local},
                              Task2 = apply_hook_options(proplists:get_all_values(TN, Hooks), Task1),
                              {Key, Task2}
                      end, Tasks);
        {error, Error} ->
            tetrapak:fail("failed to load local task ~s: ~p", [tpk_file:rebase_filename(File, tetrapak:dir(), ""), Error])
    end.

compile_local_task(File) ->
    FileDisplayPath = tpk_file:relative_path(File, tetrapak:dir()),
    {PreDefMacros, ModuleName} = predef_macros(File),
    IncludePath = [tetrapak:subdir("include")],
    case epp:parse_file(File, IncludePath, PreDefMacros) of
        {ok, Forms} ->
            TMod = process_forms(File, ModuleName, Forms),
            CompileIncludePath = [{i, D} || D <- IncludePath],
            CompileOptions = [return, debug_info, export_all | CompileIncludePath],
            io:format("Compiling ~s~n", [FileDisplayPath]),
            case compile:forms(TMod#tmod.code, CompileOptions) of
                {ok, ModuleName, BeamCode, Warnings} ->
                    {ok, FileMd5} = tpk_file:md5sum(File),
                    tetrapak_task_erlc:show_errors(tetrapak:dir(), "Warning: ", Warnings),
                    TMod#tmod{file = FileDisplayPath, md5 = FileMd5, code = BeamCode};
                {error, Errors, Warnings} ->
                    tetrapak_task_erlc:show_errors(tetrapak:dir(), "Error: ", Errors),
                    tetrapak_task_erlc:show_errors(tetrapak:dir(), "Warning: ", Warnings),
                    tetrapak:fail("task compilation failed")
            end;
        {error, Error} ->
            tetrapak:fail("failed to open local task file ~s: ~s", [FileDisplayPath, file:format_error(Error)])
    end.

predef_macros(File) ->
   {MegaSecs, Secs, MicroSecs} = erlang:now(),
   Replace   = fun ($.) -> $_;   (C) -> C end,
   CleanBase = lists:map(Replace, filename:rootname(filename:basename(File), ".erl")),
   ModuleStr = CleanBase ++ "__tpk__" ++
               integer_to_list(MegaSecs) ++ "__" ++
               integer_to_list(Secs) ++ "__" ++
               integer_to_list(MicroSecs),
   Module = list_to_atom(ModuleStr),
   PreDefMacros = [{'MODULE', Module, redefine},
                   {'MODULE_STRING', ModuleStr, redefine}],
   {PreDefMacros, Module}.

process_forms(File, ModuleName, Forms) ->
    {Found, TMod, Code} = lists:foldl(fun (Form, {FoundM, TMod, CodeAcc}) ->
                                              do_form(File, Form, FoundM, TMod, CodeAcc)
                                      end, {false, #tmod{module = ModuleName}, []}, Forms),
    case Found of
        true  -> TMod#tmod{code = lists:reverse(Code)};
        false -> TMod#tmod{code = [{attribute, 1, module, ModuleName} | lists:reverse(Code)]}
    end.

do_form(File, Attr = {attribute, _L, file, {IncludeFile, _}}, FoundM, TMod, Code) when File /= IncludeFile ->
    NewTMod = TMod#tmod{includes = [IncludeFile | TMod#tmod.includes]},
    {FoundM, NewTMod, [Attr | Code]};
do_form(_File, {attribute, L, module, _ModName}, _FoundM, TMod, Code) ->
    {true, TMod, [{attribute, L, module, TMod#tmod.module} | Code]};
do_form(_File, {attribute, _L, task, {TaskName, TaskDesc}}, FoundM, TMod, Code) when is_list(TaskName), is_list(TaskDesc) ->
    NewTMod = TMod#tmod{tasks = [{tetrapak_task:normalize_name(TaskName), TaskDesc} | TMod#tmod.tasks]},
    {FoundM, NewTMod, Code};
do_form(File, {attribute, Line, task, OtherTerm}, _FoundM, _TMod, _Code) ->
    tcom_fail(File, Line, "bad task attribute: ~p", [OtherTerm]);
do_form(File, {attribute, Line, hook, HookAttr}, FoundM, TMod, Code) ->
    NewTMod = TMod#tmod{hooks = do_hook_attribute(File, Line, HookAttr, TMod)},
    {FoundM, NewTMod, Code};
do_form(_File, OtherForm, FoundM, TMod, Code) ->
    {FoundM, TMod, [OtherForm | Code]}.

do_hook_attribute(File, Line, {TaskName, HookType, HookTarget}, TMod) when is_list(TaskName), is_atom(HookType), is_list(HookTarget) ->
    case proplists:get_value(tetrapak_task:normalize_name(TaskName), TMod#tmod.tasks) of
        undefined ->
            tcom_fail(File, Line, "hook specification for (yet) unknown local task: ~s", [TaskName]);
        _ ->
            check_hook_type(File, Line, HookType),
            (tetrapak_task:normalize_name(TaskName) == tetrapak_task:normalize_name(HookTarget))
                andalso tcom_fail(File, Line, "cannot hook a task to itself", []),
            [{TaskName, {HookType, [HookTarget]}} | TMod#tmod.hooks]
    end;
do_hook_attribute(File, Line, OtherTerm, _HookAcc) ->
    tcom_fail(File, Line, "bad hook specification: ~p", [OtherTerm]).

check_hook_type(_File, _Line, run_before) -> ok;
check_hook_type(_File, _Line, run_after) -> ok;
check_hook_type(File, Line, HT) ->
    tcom_fail(File, Line, "bad hook type (allowed: run_before, run_after): ~p", [HT]).

tcom_fail(File, Line, Fmt, Args) ->
    io:format("~s:~b: Error: " ++ Fmt ++ "\n", [tpk_file:relative_path(File, tetrapak:dir()), Line | Args]),
    tetrapak:fail("task compilation failed").

%% ------------------------------------------------------------
%% -- Config files
project_config_path(Filename) ->
    filename:join(tetrapak:subdir("tetrapak"), Filename).

read_config(File, Config) ->
    case read_ini_file(File, Config) of
        {error, {file, enoent}} ->
            Config;
        {ok, NewConfig} ->
            NewConfig;
        {error, Error} ->
            tpk_util:show_error_info(File, "Error: ", Error),
            tetrapak:fail()
    end.

add_cli_config(CliOptions, Config) ->
    lists:foldl(fun ({config, Key, Value}, Cfg = #config{values = ValueAcc}) ->
                        Cfg#config{values = lists:keystore(Key, 1, ValueAcc, {Key, Value})};
                    (_Other, ConfAcc) ->
                        ConfAcc
                end, Config, CliOptions).

read_ini_file(Filename, Config) ->
    case tetrapak_ini_parser:file(Filename) of
        {ok, Sections} -> {ok, do_sections(Sections, Config)};
        Error          -> Error
    end.

do_sections(SList, Config) ->
    lists:foldl(fun ({section, SName, Props}, OuterAcc) ->
                        lists:foldl(fun ({Key, Value}, Cfg = #config{values = ValueAcc}) ->
                                            Pair = {TheKey, _} = {ckey(SName, Key), Value},
                                            Cfg#config{values = lists:keystore(TheKey, 1, ValueAcc, Pair)}
                                    end, OuterAcc, Props);
                    ({object, OKey, OValues}, Cfg = #config{objects = ObjAcc}) ->
                        Cfg#config{objects = lists:keystore(OKey, 1, ObjAcc, {OKey, OValues})}
                end, Config, SList).

ckey("", Key)      -> Key;
ckey(Section, Key) -> Section ++ "." ++ Key.

%% ------------------------------------------------------------
%% -- option parsing on top of init:get_arguments/0
getopt(Name, Shortname, Size) ->
    case init:get_argument(Shortname) of
        error ->
            [];
        {ok, ValueLists} ->
            [list_to_tuple([Name | check_option(Shortname, Size, VL)]) || VL <- ValueLists]
    end.

check_option(_Option, Size, Args) when length(Args) =:= Size ->
    Args;
check_option(Option, Size, Args) ->
    tetrapak:fail("option -~s expects ~b arguments, given ~b", [Option, Size, length(Args)]).
