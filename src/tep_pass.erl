%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_pass).
-include("tetrapak.hrl").
-compile({no_auto_import, [get/1]}).

%% pass behaviour functions
-export([behaviour_info/1]).
-export([worker/2, context/0, fail/2, get/2, require_all/1]).
%% misc
-export([normalize_name/1, split_name/1, find_passes/0]).

-define(CTX, '$__tep_pass_context').

behaviour_info(exports) -> [{run, 2}].

worker(#pass{name = PassName, modules = [PassModule | _OtherModules]}, Context) ->
    tep_log:debug("worker: pass ~s starting", [PassName]),
    erlang:put(?CTX, Context),
    case try_check(PassModule, PassName) of
        {done, Variables} ->
            tep_context:signal_done(Context, PassName, Variables);
        {needs_run, PassData} ->
            case try_run(PassModule, PassName, PassData) of
                {done, Variables} ->
                    tep_context:signal_done(Context, PassName, Variables)
            end
    end.

try_check(PassModule, PassName) ->
    Function = tep_util:f("~s:check/1", [PassModule]),
    try
        case PassModule:check(PassName) of
            needs_run ->
                {needs_run, undefined};
            {needs_run, Data} ->
                {needs_run, Data};
            done ->
                {done, dict:new()};
            {done, Variables} ->
                {done, do_output_variables(Function, PassName, Variables)};
            true ->
                {needs_run, undefined};
            false ->
                {done, dict:new()};
            ok ->
                {done, dict:new()};
            OtherInvalid ->
                fail("~s returned an invalid value: ~p", [Function, OtherInvalid])
        end
    catch
        error:Exn ->
            case {Exn, erlang:get_stacktrace()} of
                {undef, [{PassModule, check, [PassName]} | _]} ->
                    %% check/1 is undefined, treat it as 'needs_run'
                    {needs_run, undefined};
                {function_clause, [{PassModule, check, [PassName]} | _]} ->
                    %% check/1 is defined, but not for this pass, treat it as 'needs_run'
                    {needs_run, undefined};
                _ ->
                    handle_error(PassName, Function, error, Exn)
            end;
        Class:Exn ->
            handle_error(PassName, Function, Class, Exn)
    end.

try_run(PassModule, PassName, PassData) ->
    Function = tep_util:f("~s:run/1", [PassModule]),
    try
        case PassModule:run(PassName, PassData) of
            done ->
                {done, dict:new()};
            {done, Variables} ->
                {done, do_output_variables(Function, PassName, Variables)};
            ok ->
                {done, dict:new()};
            OtherInvalid ->
                fail("~s returned an invalid value: ~p", [Function, OtherInvalid])
        end
    catch
        Class:Exn ->
            handle_error(PassName, Function, Class, Exn)
    end.

handle_error(PassName, Function, throw, {'$tep_pass_fail', Message}) ->
    tep_log:warn("failed: ~s (in ~s):~n  ~s", [PassName, Function, Message]),
    exit({pass_failed, PassName});
handle_error(PassName, Function, Class, Exn) ->
    tep_log:warn("crashed: ~s (in ~s):~n  ~p:~p~n  ~p~n",
                 [PassName, Function, Class, Exn, erlang:get_stacktrace()]),
    exit({pass_failed, PassName}).

do_output_variables(Fun, PassName, Vars) when is_list(Vars) ->
    lists:foldl(fun ({Key, Value}, Acc) ->
                        dict:store(PassName ++ ":" ++ str(Key), Value, Acc);
                    (Item, _Acc) ->
                        fail("~s returned an invalid proplist (item ~p)", [Fun, Item])
                end, dict:new(), Vars);
do_output_variables(_Fun, _PassName, {Size, nil}) when is_integer(Size) ->
    dict:new();
do_output_variables(_Fun, PassName, Tree = {Size, {_, _, _, _}}) when is_integer(Size) ->
    tep_util:fold_tree(fun ({Key, Value}, Acc) ->
                               dict:store(PassName ++ ":" ++ str(Key), Value, Acc)
                       end, dict:new(), Tree);
do_output_variables(Fun, _PassName, _Variables) ->
    fail("~s returned an invalid key-value structure (not a proplist() | gb_tree())", [Fun]).

fail(Fmt, Args) ->
    throw({'$tep_pass_fail', tep_util:f(Fmt, Args)}).

context() ->
    case erlang:get(?CTX) of
        Ctx when is_pid(Ctx) -> Ctx;
        _AnythingElse        -> error(not_inside_pass)
    end.

get(Key, FailUnknown) ->
    case require_all([Key], FailUnknown) of
        ok ->
            tep_context:get_cached(context(), Key);
        {error, {unknown_key, _}} ->
            {error, unknown_key}
    end.

require_all(Keys) ->
    require_all(Keys, false).
require_all(Keys, FailUnknown) when is_list(Keys) ->
    KList = lists:map(fun str/1, Keys),
    case tep_context:wait_for(context(), KList) of
        ok ->
            ok;
        {error, Error} ->
            case {FailUnknown, Error} of
                {_, {failed, Other}}        ->
                    fail("required pass '~s' failed", [Other]);
                {false, _} ->
                    {error, Error};
                {true, {unknown_key, Unknown}} ->
                    fail("require/1 of unknown key: ~p", [Unknown])
            end
    end.

%% ------------------------------------------------------------
%% -- Beam Scan
find_passes() ->
    find_passes([code:lib_dir(tetrapak, ebin)]).
find_passes(Directories) ->
    lists:foldl(fun (Dir, OuterModAcc) ->
                   tep_log:debug("checking for pass modules in ~s", [Dir]),
                   tep_file:walk(fun (File, ModAcc) ->
                                    case is_pass_module(File) of
                                        false              -> ModAcc;
                                        {Module, PassDefs} -> store_passdefs(Module, PassDefs, ModAcc)
                                    end
                                  end, OuterModAcc, Dir)
                end, [], Directories).

is_pass_module(Mfile) ->
    case filename:extension(Mfile) of
        ".beam" ->
            {ok, {ModuleName, Chunks}} = beam_lib:chunks(Mfile, [attributes]),
            Attributes = proplists:get_value(attributes, Chunks, []),
            IsPass = lists:member(?MODULE, proplists:get_value(behaviour, Attributes, [])) orelse
                     lists:member(?MODULE, proplists:get_value(behavior, Attributes, [])),
            if
                IsPass ->
                    case proplists:get_value(pass, Attributes) of
                        undefined -> false;
                        InfoList  -> {ModuleName, lists:flatten(InfoList)}
                    end;
                true ->
                    false
            end;
        _ ->
            false
    end.

store_passdefs(Module, List, PassMap) ->
    lists:foldl(fun ({PassName, Desc}, Acc) ->
                        NewPass   = #pass{name = normalize_name(PassName),
                                          modules = [Module],
                                          description = Desc},
                        AddModule = fun (#pass{modules = OldMods}) -> NewPass#pass{modules = [Module | OldMods]} end,
                        pl_update(split_name(PassName), AddModule, NewPass, Acc)
               end, PassMap, List).

pl_update(Key, AddItem, NewItem, Proplist) ->
    case proplists:get_value(Key, Proplist) of
        undefined -> [{Key, NewItem} | Proplist];
        Value     -> [AddItem(Value) | proplists:delete(Key, Proplist)]
    end.

normalize_name(Key) ->
    string:to_lower(string:strip(str(Key))).
split_name(Key) ->
    SplitName = re:split(normalize_name(Key), ":", [{return, list}]),
    lists:filter(fun ([]) -> false;
                     (_)  -> true
                 end, SplitName).

str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
str(Lis)                     -> Lis.
