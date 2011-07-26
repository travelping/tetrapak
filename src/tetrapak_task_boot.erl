%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_boot).
-behaviour(tetrapak_task).
-export([initial_tmap/0, run/2]).

-include("tetrapak.hrl").

initial_tmap() ->
    [{["clean", "taskcache"],  #task{name = "clean:taskcache", module = ?MODULE, description = "Delete the local task cache"}},
     {["tetrapak", "boot"],    #task{name = "tetrapak:boot", module = ?MODULE,   description = "The root of all evil"}},
     {["tetrapak", "info"],    #task{name = "tetrapak:info", module = ?MODULE,   description = "Show version and tasks"}}].

run("tetrapak:boot", _) ->
    Props   = load_appdata(),
    Version = proplists:get_value(vsn, Props, "UNKNOWN"),
    Env     = proplists:get_value(env, Props),

    BaseConfig     = lists:foldl(fun ({Key, Value}, Acc) ->
                                         gb_trees:enter(ckey("", Key), Value, Acc)
                                 end, gb_trees:empty(), proplists:get_value(config, Env)),
    ProjectConfig1 = read_config(project_config_path("config.ini"), BaseConfig),
    ProjectConfig2 = read_config(project_config_path("local.ini"), ProjectConfig1),

    {ok, CliOptions} = application:get_env(tetrapak, cli_options),
    TheConfig = lists:foldl(fun ({config, Key, Value}, ConfAcc) ->
                                    gb_trees:enter(ckey("", Key), Value, ConfAcc);
                                (_Other, ConfAcc) ->
                                    ConfAcc
                            end, ProjectConfig2, CliOptions),

    %% scan tasks
    tetrapak_context:register_tasks(tetrapak_task:context(), builtin_tasks(proplists:get_value(tasks, Env))),
    tetrapak_context:register_tasks(tetrapak_task:context(), scan_local_tasks(tetrapak:subdir("tetrapak"))),

    RetVars = gb_trees:enter("version", Version, TheConfig),
    {done, RetVars};

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
    lists:foldl(fun ({TaskName, Module, Desc}, Acc) ->
                        NewTask   = #task{name = tetrapak_task:normalize_name(TaskName),
                                          module = Module,
                                          description = Desc,
                                          origin = builtin},
                        AddModule = fun (_) -> NewTask end,
                        pl_update(tetrapak_task:split_name(TaskName), AddModule, NewTask, Acc)
                end, [], Tasks).

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
-record(tmod, {file, md5, module, tasks = [], includes = [], code}).

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
                              [] ->
                                  compile_and_load(File, Cache)
                          end
               end, Files).

compile_and_load(File, Cache) ->
    NewTMod = compile_local_task(File),
    dets:insert(Cache, NewTMod),
    load_local_task(File, NewTMod).

load_local_task(File, #tmod{module = ModuleName, tasks = Tasks, code = Code}) ->
    code:purge(ModuleName),
    case code:load_binary(ModuleName, File, Code) of
        {module, ModuleName} ->
            [{tetrapak_task:split_name(TN), #task{module = ModuleName, name = TN, description = TD, origin = local}} || {TN, TD} <- Tasks];
        {error, Error} ->
            tetrapak:fail("failed to load local task ~s: ~p", [tpk_file:rebase_filename(File, tetrapak:dir(), ""), Error])
    end.

compile_local_task(File) ->
    FileDisplayPath = tpk_file:rebase_filename(File, tetrapak:dir(), ""),
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
            tetrapak:fail("failed to open local task source ~s: ~s", [FileDisplayPath, file:format_error(Error)])
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
    {Found, TMod, Code} = lists:foldr(fun (Form, {FoundM, TMod, CodeAcc}) ->
                                              do_form(File, Form, FoundM, TMod, CodeAcc)
                                      end, {false, #tmod{module = ModuleName}, []}, Forms),
   case Found of
       true  -> TMod#tmod{code = Code};
       false -> TMod#tmod{code = [{attribute, 1, module, ModuleName} | Code]}
   end.

do_form(File, Attr = {attribute, _L, file, {IncludeFile, _}}, FoundM, TMod, Code) when File /= IncludeFile ->
    NewTMod = TMod#tmod{includes = [IncludeFile | TMod#tmod.includes]},
    {FoundM, NewTMod, [Attr | Code]};
do_form(_File, {attribute, L, module, _ModName}, _FoundM, TMod, Code) ->
    {true, TMod, [{attribute, L, module, TMod#tmod.module} | Code]};
do_form(_File, {attribute, _L, task, {TaskName, TaskDesc}}, FoundM, TMod, Code) ->
    NewTMod = TMod#tmod{tasks = [{tetrapak_task:normalize_name(TaskName), TaskDesc} | TMod#tmod.tasks]},
    {FoundM, NewTMod, Code};
do_form(_File, OtherForm, FoundM, TMod, Code) ->
    {FoundM, TMod, [OtherForm | Code]}.

%% ------------------------------------------------------------
%% -- Config files
project_config_path(Filename) ->
    filename:join(tetrapak:subdir("tetrapak"), Filename).

read_config(File, Tree) ->
    case read_ini_file(File, Tree) of
        {error, {file, enoent}} ->
            Tree;
        {ok, ConfigTree} ->
            ConfigTree;
        {error, Error} ->
            tpk_util:show_error_info(File, "Error: ", Error),
            tetrapak:fail()
    end.

read_ini_file(Filename, Tree) ->
    case tetrapak_ini_lexer:file(Filename) of
        {ok, Tokens, _Endl} ->
            case tetrapak_ini_parser:parse(Tokens) of
                {ok, Sections} -> {ok, do_sections(Sections, Tree)};
                Error          -> Error
            end;
        Error -> Error
    end.

do_sections(SList, Tree) ->
    lists:foldl(fun ({section, SName, Props}, OuterAcc) ->
                        lists:foldl(fun ({Key, Value}, InnerAcc) ->
                                            gb_trees:enter(ckey(SName, Key), Value, InnerAcc)
                                    end, OuterAcc, Props)
                end, Tree, SList).

ckey("", Key)      -> "config:" ++ Key;
ckey(Section, Key) -> "config:" ++ Section ++ "." ++ Key.
