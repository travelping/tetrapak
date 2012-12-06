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

-module(tetrapak_task_erlc).
-behaviour(tetrapak_task).
-export([check/1, run/2]).
-export([check_mtimes/2, show_errors/3]).

-include("tetrapak.hrl").
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
    EbinDir             = tetrapak:path("ebin"),
    SrcDir              = tetrapak:path("src"),
    ExtraCompileOptions = tetrapak:config("build.erlc_options", []),
    CompileOptions      = [{outdir, EbinDir}, {i, tetrapak:path("include")}, return_errors, return_warnings, debug_info]
                          ++ ExtraCompileOptions,
    Sources             = sort_dependency_order(erlang_source_files(SrcDir)),
    FileList            =
        lists:flatmap(fun (File) ->
                          case needs_compile(CompileOptions, EbinDir, File) of
                              true  -> [{File, CompileOptions}];
                              false -> []
                          end
                      end, Sources),
    case FileList of
        [] -> {done, [{modules, []}]};
        _  -> {needs_run, FileList}
    end;

check("build:yecc") ->
    SrcDir = tetrapak:path("src"),
    tpk_util:check_files_mtime(SrcDir, ".yrl", SrcDir, ".erl");

check("build:leex") ->
    SrcDir = tetrapak:path("src"),
    tpk_util:check_files_mtime(SrcDir, ".xrl", SrcDir, ".erl");

check("clean:yecc") ->
    SrcDir = tetrapak:path("src"),
    tpk_util:check_files_exist(SrcDir, ".yrl", SrcDir, ".erl");

check("clean:leex") ->
    SrcDir = tetrapak:path("src"),
    tpk_util:check_files_exist(SrcDir, ".xrl", SrcDir, ".erl").

run("build:erlang", ErlFiles) ->
    file:make_dir(tetrapak:path("ebin")),
    compile_foreach(fun ({File, CompileOptions}) ->
                            run_compiler(compile, file, [File#erl.file, CompileOptions])
                    end, ErlFiles),
    {done, [{modules, [atom_to_list(F#erl.module) || {F, _} <- ErlFiles]}]};

run("build:yecc", Files) ->
    compile_foreach(fun ({InputFile, OutputFile}) ->
                            run_compiler(yecc, file, [InputFile, [{parserfile, OutputFile}, {return, true}, {report, false}]])
                    end, Files);

run("build:leex", Files) ->
    compile_foreach(fun ({InputFile, OutputFile}) ->
                            run_compiler(leex, file, [InputFile, [{scannerfile, OutputFile}, {return, true}, {report, false}]])
                    end, Files);

run("clean:erlang", _) ->
    tpk_file:delete("\\.beam$", tetrapak:path("ebin"));

run("clean:yecc", Files) ->
    lists:foreach(fun ({_, ErlFile}) -> tpk_file:delete(ErlFile) end, Files);

run("clean:leex", Files) ->
    lists:foreach(fun ({_, ErlFile}) -> tpk_file:delete(ErlFile) end, Files);

run("clean:test", _) ->
    tpk_file:delete("\\.beam$", tetrapak:path("test")).

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
    io:format("Compiling ~s~n", [tpk_file:relative_path(File, BaseDir)]),
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
                          lists:foreach(fun (Error) ->
                                                DisplayPath = tpk_file:relative_path(FileName, BaseDir),
                                                ?DEBUG("show error: ~p", [{BaseDir, FileName, DisplayPath}]) ,
                                                tpk_util:show_error_info(DisplayPath, Prefix, Error)
                                        end, FileErrors)
                  end, Errors).

sort_dependency_order(Files) ->
    G = digraph:new(),
    lists:foreach(fun (F) -> digraph:add_vertex(G, F#erl.module, F) end, Files),
    lists:foreach(fun (F) ->
                          lists:foreach(fun (B) ->
                                                digraph:add_edge(G, B, F#erl.module)
                                        end, F#erl.behaviours),
                          lists:foreach(fun ({parse_transform, PT}) ->
                                                digraph:add_edge(G, PT, F#erl.module);
                                            (_) ->
                                                ok
                                        end, proplists:get_value(compile, F#erl.attributes, []))
                  end, Files),
    case digraph_utils:topsort(G) of
        false ->
            Files;
        Mods ->
            [element(2, digraph:vertex(G, M)) || M <- Mods]
    end.

needs_compile(_NewCOptions, Ebin, #erl{module = Mod, attributes = _Attrs, includes = Inc, mtime = ModMTime}) ->
    Beam = filename:join(Ebin, tpk_util:f("~s~s", [Mod, code:objfile_extension()])),
    % COptions = remove_return_options(proplists:get_value(compile, Attrs, []) ++ NewCOptions),
    case filelib:is_regular(Beam) of
        false -> true;
        true  ->
            % {ok, {_, [{compile_info, ComInfo}]}} = beam_lib:chunks(Beam, [compile_info]),
            % BeamCOptions = remove_return_options(proplists:get_value(options, ComInfo)),
            BeamMTime    = tpk_file:mtime(Beam),
            ((BeamMTime =< ModMTime)) %% beam is older
            % orelse lists:usort(BeamCOptions) /= lists:usort(COptions) %% compiler options changed
            orelse check_mtimes(BeamMTime, Inc) end.

% remove_return_options(Opts) ->
%     [O || O <- Opts, O /= return_errors, O /= return_warnings].

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
    IncsFromOptions = [tetrapak:path(I) || {i, I} <- tetrapak:config("build.erlc_options")],
    IncPath = [tetrapak:path("src"), tetrapak:path("include") | IncsFromOptions],
    case epp:parse_file(Path, IncPath, []) of
        {ok, []} ->
            #erl{mtime = tpk_file:mtime(Path), file = Path, module = filename:basename(Path, ".erl")};
        {ok, Forms} ->
            Rec = #erl{mtime = tpk_file:mtime(Path), file = Path, module = filename:basename(Path, ".erl")},
            lists:foldl(fun (F, Acc) -> do_form(Path, F, Acc) end, Rec, tl(Forms))
    end.

do_form(File, {attribute, _, file, {IncludeFile, _}}, R) when File /= IncludeFile ->
    R#erl{includes = [IncludeFile | R#erl.includes]};
do_form(_File, {attribute, _, module, Module}, R) when is_atom(Module) ->
    R#erl{module = Module};
do_form(_File, {attribute, _, module, Module}, R) when is_list(Module) ->
    R#erl{module = list_to_atom(string:join([atom_to_list(A) || A <- Module], "."))};
do_form(_File, {attribute, _, module, {Module, _}}, R) when is_atom(Module) ->
    R#erl{module = Module};
do_form(_File, {attribute, _, module, {Module, _}}, R) when is_list(Module) ->
    R#erl{module = list_to_atom(string:join([atom_to_list(A) || A <- Module], "."))};
do_form(_File, {attribute, _, behaviour, Behaviour}, R) ->
    R#erl{behaviours = [Behaviour | R#erl.behaviours]};
do_form(_File, {attribute, _, Attr, Value}, R) ->
    case proplists:get_value(Attr, R#erl.attributes) of
        undefined -> R#erl{attributes = [{Attr, avalue(Value)} | R#erl.attributes]};
        Existing  -> R#erl{attributes = lists:keyreplace(Attr, 1, R#erl.attributes, {Attr, avalue(Value) ++ Existing})}
    end;
do_form(_File, {error, _}, R) ->
    R#erl{invalid = true};
do_form(_File, _Form, R) ->
    R.

avalue(Val) when is_list(Val) -> Val;
avalue(Val)                   -> [Val].
