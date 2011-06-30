%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_erlc).
-behaviour(tetrapak_task).
-export([check/1, run/2]).
-export([check_mtimes/2, show_errors/3]).

-include_lib("kernel/include/file.hrl").

-record(erl, {
    file,
    module,
    behaviours = [],
    attributes = [],
    includes = [],
    mtime,
    invalid = false
}).

%% ------------------------------------------------------------
%% -- Task API
check("build:erlang") ->
    tetrapak:require(["build:yecc"]),
    EbinDir             = tetrapak:subdir("ebin"),
    SrcDir              = tetrapak:subdir("src"),
    ExtraCompileOptions = tetrapak:config("build.erlc_options", []),
    CompileOptions      = [{outdir, EbinDir}, {i, tetrapak:subdir("include")}, return_errors, return_warnings, debug_info]
                          ++ ExtraCompileOptions,
    Sources             = lists:sort(fun compile_order/2, erlang_source_files(SrcDir)),
    FileList            =
        lists:flatmap(fun (File) ->
                          case needs_compile(CompileOptions, EbinDir, File) of
                              true  -> [{File, CompileOptions}];
                              false -> []
                          end
                      end, Sources),
    case FileList of
        [] -> {done,      [{modules, []}]};
        _  -> {needs_run, FileList}
    end;

check("build:yecc") ->
    SrcDir = tetrapak:subdir("src"),
    tpk_util:check_files_mtime(SrcDir, ".yrl", SrcDir, ".erl");

check("build:leex") ->
    SrcDir = tetrapak:subdir("src"),
    tpk_util:check_files_mtime(SrcDir, ".xrl", SrcDir, ".erl");

check("clean:yecc") ->
    SrcDir = tetrapak:subdir("src"),
    tpk_util:check_files_exist(SrcDir, ".yrl", SrcDir, ".erl");

check("clean:leex") ->
    SrcDir = tetrapak:subdir("src"),
    tpk_util:check_files_exist(SrcDir, ".xrl", SrcDir, ".erl").

run("build:erlang", ErlFiles) ->
    file:make_dir(tetrapak:subdir("ebin")),
    compile_foreach(fun ({File, CompileOptions}) ->
                            try_load(tetrapak:subdir("ebin"), File#erl.behaviours),
                            run_compiler(compile, file, [File#erl.file, CompileOptions])
                    end, ErlFiles),
    {done, [{modules, [F#erl.module || {F, _} <- ErlFiles]}]};

run("build:yecc", Files) ->
    compile_foreach(fun ({InputFile, OutputFile}) ->
                            run_compiler(yecc, file, [InputFile, [{parserfile, OutputFile}, {return, true}, {report, false}]])
                    end, Files);

run("build:leex", Files) ->
    compile_foreach(fun ({InputFile, OutputFile}) ->
                            run_compiler(leex, file, [InputFile, [{scannerfile, OutputFile}, {return, true}, {report, false}]])
                    end, Files);

run("clean:erlang", _) ->
    tpk_file:delete("\\.beam$", tetrapak:subdir("ebin"));

run("clean:yecc", Files) ->
    lists:foreach(fun ({_, ErlFile}) -> tpk_file:delete(ErlFile) end, Files);

run("clean:leex", Files) ->
    lists:foreach(fun ({_, ErlFile}) -> tpk_file:delete(ErlFile) end, Files).

%% ------------------------------------------------------------
%% -- Helpers
compile_foreach(Function, List) ->
    Res = lists:foldl(fun (Item, DoFail) ->
                              case Function(Item) of
                                  ok    -> DoFail;
                                  error -> true
                              end
                      end, false, List),
    if Res  -> tetrapak:fail("compilation failed");
       true -> ok
    end.

run_compiler(M, F, A = [File | _]) ->
    BaseDir = tetrapak:dir(),
    io:format("Compiling ~s~n", [tpk_file:rebase_filename(File, BaseDir, "")]),
    case apply(M, F, A) of
        {ok, _Module} -> ok;
        {ok, _Module, Warnings} ->
            show_errors(BaseDir, "Warning: ", Warnings),
            ok;
        error ->
            error;
        {error, Errors, Warnings} ->
            show_errors(BaseDir, "Error: ", Errors),
            show_errors(BaseDir, "Warning: ", Warnings),
            error
    end.

show_errors(BaseDir, Prefix, Errors) ->
    lists:foreach(fun ({FileName, FileErrors}) ->
                          case lists:prefix(BaseDir, FileName) of
                              true ->
                                  Path = tpk_file:rebase_filename(FileName, BaseDir, "");
                              false ->
                                  Path = FileName
                          end,
                          lists:foreach(fun (Error) -> tpk_util:show_error_info(Path, Prefix, Error) end, FileErrors)
                  end, Errors).

compile_order(File1, _File2) ->
    lists:member({behaviour_info, 1}, proplists:get_value(export, File1#erl.attributes, [])) or
    lists:member({parse_transform, 2}, proplists:get_value(export, File1#erl.attributes, [])).

try_load(EbinDir, ModList) ->
    lists:foreach(fun (Mod) ->
                          MAtom = list_to_atom(Mod),
                          case code:is_loaded(MAtom) of
                              false -> code:load_abs(filename:join(EbinDir, Mod ++ ".beam"));
                              _     -> ok
                          end
                  end, ModList).

needs_compile(NewCOptions, Ebin, #erl{module = Mod, attributes = Attrs, includes = Inc, mtime = ModMTime}) ->
    Beam = filename:join(Ebin, tpk_util:f("~s.beam", [Mod])),
    COptions = proplists:get_value(compile, Attrs, []) ++ NewCOptions,
    case filelib:is_regular(Beam) of
        false -> true;
        true  ->
            {ok, {_, [{compile_info, ComInfo}]}} = beam_lib:chunks(Beam, [compile_info]),
            BeamCOptions = proplists:get_value(options, ComInfo),
            BeamMTime    = tpk_file:mtime(Beam),
            ((BeamMTime =< ModMTime)) %% beam is older
            orelse lists:usort(BeamCOptions) /= lists:usort(COptions) %% compiler options changed
            orelse check_mtimes(BeamMTime, Inc) end.

check_mtimes(FileMTime, Files) ->
    lists:any(fun (F) ->
                      case file:read_file_info(F) of
                          {error, _} -> true;
                          {ok, Info} -> Info#file_info.mtime >= FileMTime
                      end
              end, Files).

erlang_source_files(Path) ->
    case filelib:is_dir(Path) of
        true ->
            tpk_file:walk(fun (File, Acc) ->
                                  case tpk_util:match("\\.erl$", File) of
                                      true  -> [scan_source(File) | Acc];
                                      false -> Acc
                                  end
                          end, [], Path);
        false ->
            tetrapak:fail("not a directory: ~s", [Path])
    end.

scan_source(Path) ->
    case epp_dodger:quick_parse_file(Path, []) of
        {ok, Forms} ->
            Rec = #erl{mtime = tpk_file:mtime(Path), file = Path, module = filename:basename(Path, ".erl")},
            lists:foldl(fun (F, Acc) -> do_form(Path, F, Acc) end, Rec, tl(Forms))
    end.

do_form(File, {attribute, _, file, {IncludeFile, _}}, R) when File /= IncludeFile ->
    R#erl{includes = [IncludeFile | R#erl.includes]};
do_form(_File, {attribute, _, module, Module}, R) when is_atom(Module) ->
    R#erl{module = atom_to_list(Module)};
do_form(_File, {attribute, _, module, Module}, R) when is_list(Module) ->
    R#erl{module = string:join([atom_to_list(A) || A <- Module], ".")};
do_form(_File, {attribute, _, module, {Module, _}}, R) when is_atom(Module) ->
    R#erl{module = atom_to_list(Module)};
do_form(_File, {attribute, _, module, {Module, _}}, R) when is_list(Module) ->
    R#erl{module = string:join([atom_to_list(A) || A <- Module], ".")};
do_form(_File, {attribute, _, behaviour, Behaviour}, R) ->
    R#erl{behaviours = [atom_to_list(Behaviour) | R#erl.behaviours]};
do_form(_File, {attribute, _, behavior, Behaviour}, R) ->
    R#erl{behaviours = [atom_to_list(Behaviour) | R#erl.behaviours]};
do_form(_File, {attribute, _, Attr, Value}, R) ->
    case proplists:get_value(Attr, R#erl.attributes) of
        undefined -> R#erl{attributes = [{Attr, avalue(Value)} | R#erl.attributes]};
        Existing  -> R#erl{attributes = lists:keyreplace(Attr, 1, R#erl.attributes, {Attr, avalue(Value) ++ Existing})}
    end;
do_form(_File, {error, _}, R) ->
    R#erl{invalid = true};
do_form(_File, _, R) ->
    R.

avalue(Val) when is_list(Val) -> Val;
avalue(Val)                   -> [Val].
