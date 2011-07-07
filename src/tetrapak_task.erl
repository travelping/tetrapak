%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task).
-include("tetrapak.hrl").
-compile({no_auto_import, [get/1]}).

%% task behaviour functions
-export([behaviour_info/1]).
-export([worker/3, context/0, directory/0, fail/0, fail/2, get/1, require_all/1]).
-export([output_collector/3, print_output_header/2]).
%% misc
-export([normalize_name/1, split_name/1]).

-define(CTX, '$__tetrapak_task_context').
-define(DIRECTORY, '$__tetrapak_task_directory').

behaviour_info(callbacks) -> [{run, 2}];
behaviour_info(_) -> undefined.

context() ->
    case erlang:get(?CTX) of
        Ctx when is_pid(Ctx) -> Ctx;
        _AnythingElse        -> error(not_inside_task)
    end.

directory() ->
    case erlang:get(?DIRECTORY) of
        Ctx when is_list(Ctx) -> Ctx;
        _AnythingElse         -> error(not_inside_task)
    end.

worker(#task{name = TaskName, module = TaskModule}, Context, Directory) ->
    ?DEBUG("worker: task ~s starting", [TaskName]),

    OutputCollector = spawn_link(?MODULE, output_collector, [Context, TaskName, self()]),
    group_leader(OutputCollector, self()),

    erlang:put(?CTX, Context),
    erlang:put(?DIRECTORY, Directory),
    case try_check(TaskModule, TaskName) of
        {done, Variables} ->
            ?DEBUG("worker: check/1 -> done"),
            tetrapak_context:task_done(Context, TaskName, Variables),
            exit({?TASK_DONE, TaskName});
        {needs_run, TaskData} ->
            case try_run(TaskModule, TaskName, TaskData) of
                {done, Variables} ->
                    ?DEBUG("worker: run/2 -> done"),
                    tetrapak_context:task_done(Context, TaskName, Variables),
                    exit({?TASK_DONE, TaskName})
            end
    end.

try_check(TaskModule, TaskName) ->
    Function = tpk_util:f("~s:check/1", [TaskModule]),
    try
        case TaskModule:check(TaskName) of
            needs_run ->
                {needs_run, undefined};
            {needs_run, Data} ->
                {needs_run, Data};
            done ->
                {done, dict:new()};
            {done, Variables} ->
                {done, do_output_variables(Function, TaskName, Variables)};
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
                {undef, [{TaskModule, check, [TaskName]} | _]} ->
                    %% check/1 is undefined, treat it as 'needs_run'
                    {needs_run, undefined};
                {function_clause, [{TaskModule, check, [TaskName]} | _]} ->
                    %% check/1 is defined, but not for this task, treat it as 'needs_run'
                    {needs_run, undefined};
                _ ->
                    handle_error(TaskName, Function, error, Exn)
            end;
        Class:Exn ->
            handle_error(TaskName, Function, Class, Exn)
    end.

try_run(TaskModule, TaskName, TaskData) ->
    Function = tpk_util:f("~s:run/1", [TaskModule]),
    try
        case TaskModule:run(TaskName, TaskData) of
            done ->
                {done, dict:new()};
            {done, Variables} ->
                {done, do_output_variables(Function, TaskName, Variables)};
            ok ->
                {done, dict:new()};
            OtherInvalid ->
                fail("~s returned an invalid value: ~p", [Function, OtherInvalid])
        end
    catch
        Class:Exn ->
            handle_error(TaskName, Function, Class, Exn)
    end.

handle_error(TaskName, _Function, throw, {?TASK_FAIL, Message}) ->
    case Message of
        undefined -> ok;
        _ ->
            io:put_chars(["Error: ", Message, $\n])
    end,
    exit({?TASK_FAIL, TaskName});
handle_error(TaskName, Function, Class, Exn) ->
    io:format("crashed in ~s:~n~p:~p~n~p~n", [Function, Class, Exn, erlang:get_stacktrace()]),
    exit({?TASK_FAIL, TaskName}).

do_output_variables(Fun, TaskName, Vars) when is_list(Vars) ->
    lists:foldl(fun ({Key, Value}, Acc) ->
                        dict:store(TaskName ++ ":" ++ str(Key), Value, Acc);
                    (Item, _Acc) ->
                        fail("~s returned an invalid proplist (item ~p)", [Fun, Item])
                end, dict:new(), Vars);
do_output_variables(_Fun, _TaskName, {Size, nil}) when is_integer(Size) ->
    dict:new();
do_output_variables(_Fun, TaskName, Tree = {Size, {_, _, _, _}}) when is_integer(Size) ->
    tpk_util:fold_tree(fun ({Key, Value}, Acc) ->
                               dict:store(TaskName ++ ":" ++ str(Key), Value, Acc)
                       end, dict:new(), Tree);
do_output_variables(Fun, _TaskName, _Variables) ->
    fail("~s returned an invalid key-value structure (not a proplist() | gb_tree())", [Fun]).

fail() ->
    throw({?TASK_FAIL, undefined}).
fail(Fmt, Args) ->
    throw({?TASK_FAIL, tpk_util:f(Fmt, Args)}).

get(Key) ->
    case require_all([Key]) of
        ok ->
            tetrapak_context:get_cached(context(), Key);
        {error, {unknown_key, _}} ->
            {error, unknown_key}
    end.

require_all(Keys) when is_list(Keys) ->
    KList = lists:map(fun str/1, Keys),
    case tetrapak_context:wait_for(context(), KList) of
        ok ->
            ok;
        {error, {failed, Other}} ->
            fail("required task '~s' failed", [Other]);
        {error, Error} ->
            {error, Error}
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
str(Lis) when is_list(Lis)   -> Lis.

%% ------------------------------------------------------------
%% -- Output handler
-define(LineWidth, 30).

output_collector(Context, TaskName, TaskProcess) ->
    ?DEBUG("output_collector for ~s", [TaskName]),
    process_flag(trap_exit, true),
    tetrapak_context:register_io_worker(Context),
    receive
        Req = {io_request, _, _, _} ->
            tetrapak_context:task_wants_output(Context),
            Buffer = handle_io(Req, <<>>),
            output_collector_loop(Context, TaskName, TaskProcess, Buffer);
        {'EXIT', TaskProcess, _Reason} ->
            exit(normal)
    end.

output_collector_loop(Context, TaskName, TaskProcess, Buffer) ->
    receive
        Req = {io_request, _, _, _} ->
            NewBuffer = handle_io(Req, Buffer),
            output_collector_loop(Context, TaskName, TaskProcess, NewBuffer);
        {reply, Context, output_ok} ->
            print_output_header(group_leader(), TaskName),
            io:put_chars(Buffer),
            output_collector_loop(Context, TaskName, TaskProcess, console);
        {'EXIT', TaskProcess, _Reason} ->
            case Buffer of
                console ->
                    tetrapak_context:task_output_done(Context);
                <<>> ->
                    tetrapak_context:task_output_done(Context);
                _ ->
                    wait_output_ok(Context, TaskName, Buffer)
            end
    end.

wait_output_ok(Context, TaskName, Buffer) ->
    receive
        {reply, Context, output_ok} ->
            print_output_header(group_leader(), TaskName),
            io:put_chars(Buffer),
            tetrapak_context:task_output_done(Context);
        _Other ->
            ?DEBUG("wait_output_ok other ~p", [_Other])
    end.

handle_io(Req, console) ->
    group_leader() ! Req,
    console;
handle_io({io_request, From, ReplyAs, Request}, Buffer) ->
    {Reply, Chars} = tetrapak_io:ioreq_output(Request),
    NewBuffer = do_output(Buffer, Chars),
    From ! {io_reply, ReplyAs, Reply},
    NewBuffer.

do_output(Buffer, Chars) ->
    <<Buffer/binary, (iolist_to_binary(Chars))/binary>>.

print_output_header(IODev, TaskName) ->
    io:put_chars(IODev, ["== ", TaskName, " ", lists:duplicate(max(0, ?LineWidth - length(TaskName)), $=), $\n]).
