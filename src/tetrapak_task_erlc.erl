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

-task({"build:erlang", "Build Erlang modules"}).
-task({"clean:erlang", "Delete compiled Erlang modules"}).

-record(erl, {
    file,
    module,
    attributes = [],
    exports = [],
    behaviours = [],
    includes = [],
    mtime,
    invalid = false
}).

%% ------------------------------------------------------------
%% -- Task API
check("build:erlang") ->
    EbinDir             = tetrapak:subdir("ebin"),
    SrcDir              = tetrapak:subdir("src"),
    Sources             = erlang_source_files(SrcDir),
    ExtraCompileOptions = tetrapak:get("config:build:erlang:options", []),
    CompileOptions      = [{outdir, EbinDir}, {i, "include"}, debug_info, return_errors] ++ ExtraCompileOptions,
    FileList            =
        lists:flatmap(fun (File) ->
                          case needs_compile(CompileOptions, EbinDir, File) of
                              true  -> [{File, CompileOptions}];
                              false -> []
                          end
                      end, Sources),
    case FileList of
        [] -> done;
        _  -> {needs_run, FileList}
    end.

run("build:erlang", ErlFiles) ->
    lists:foreach(fun ({File, CompileOptions}) ->
                          tpk_log:debug("compile ~s", [File#erl.file]),
                          case compile:file(File#erl.file, CompileOptions) of
                              {ok, _Module}             -> ok;
                              {error, Errors, Warnings} ->
                                  tetrapak:fail("compilation of ~s failed:~n~s~n~s",
                                                [File#erl.module, string:join(Errors, "\n"), string:join(Warnings, "\n")])
                          end
                  end, ErlFiles);

run("clean:erlang", _) ->
    tpk_file:delete("\\.beam$", tetrapak:subdir("ebin")).

%% ------------------------------------------------------------
%% -- Helpers
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
            orelse lists:any(fun (I) -> tpk_file:mtime(I) >= BeamMTime end, Inc) %% include file changed
    end.

erlang_source_files(Path) ->
    case filelib:is_dir(Path) of
        true ->
            tpk_file:walk(fun (File, Acc) ->
                              case tpk_util:match("\\.erl$", File) of
                                  true ->
                                      {ok, MRec} = scan_source(File),
                                      insert_file(MRec, Acc);
                                  false ->
                                      Acc
                              end
                          end, [], Path);
        false ->
            tetrapak:fail("not a directory: ~s", [Path])
    end.

insert_file(F, Acc) -> [F | Acc].

scan_source(Path) ->
    case epp:parse_file(Path, ["include"], []) of
        {ok, Forms} ->
            Rec = #erl{mtime = tpk_file:mtime(Path), file = Path, module = filename:basename(Path, ".erl")},
            {ok, lists:foldl(fun (F, Acc) -> do_form(Path, F, Acc) end, Rec, tl(Forms))};
        {error, Error} ->
            {error, Error}
    end.

do_form(File, {attribute, _, file, {IncludeFile, _}}, R) when File /= IncludeFile ->
    R#erl{includes = [IncludeFile | R#erl.includes]};
do_form(_File, {attribute, _, export, AddExports}, R) ->
    R#erl{exports = AddExports ++ R#erl.exports};
do_form(_File, {attribute, _, behaviour, Behaviour}, R) ->
    R#erl{behaviours = [Behaviour | R#erl.behaviours]};
do_form(_File, {attribute, _, behavior, Behaviour}, R) ->
    R#erl{behaviours = [Behaviour | R#erl.behaviours]};
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
