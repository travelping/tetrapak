%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tpk_util).
-export([f/1, f/2, match/2, unix_time/0, unix_time/1, to_hex/1, fold_tree/3]).
-export([check_files/2, check_files/5, check_files_mtime/1, check_files_mtime/4,
         check_files_exist/4, varsubst/2, varsubst_file/2]).
-export([cmd/3, outputcmd/3, parse_cmdline/3]).
-export([format_error/1, show_error_info/2, show_error_info/3]).
-export([debug_log_to_stderr/2]).

-include_lib("kernel/include/file.hrl").
-include("tetrapak.hrl").

f(Str) -> f(Str,[]).
f(Str, Args) -> lists:flatten(io_lib:format(Str, Args)).

show_error_info(File, Error) ->
    show_error_info(File, "", Error).
show_error_info(File, Prefix, {Mod, Einfo}) ->
    io:format("~s: ~s~s~n", [File, Prefix, Mod:format_error(Einfo)]);
show_error_info(File, Prefix, {Line, Mod, Einfo}) ->
    if
        is_integer(Line) ->
            io:format("~s:~b: ~s~s~n", [File, Line, Prefix, Mod:format_error(Einfo)]);
        true ->
            io:format("~s: ~s~s~n", [File, Prefix, Mod:format_error(Einfo)])
    end.

match(Fun, String) when is_function(Fun) ->
    Fun(String);
match(Re, String) when is_list(Re) or is_binary(Re) ->
    case re:run(String, Re, [{capture, none}]) of
        match -> true;
        nomatch -> false
    end;
match(undefined, _String) ->
    false.

unix_time() ->
    unix_time(calendar:universal_time()).
unix_time(DateTime = {{_y, _mon, _d}, {_h, _min, _s}}) ->
    Epoch = 62167219200,
    Secs  = calendar:datetime_to_gregorian_seconds(DateTime),
    Secs - Epoch.

to_hex(B) when is_binary(B) ->
    << <<(nibble2hex(N))>> || <<N:4>> <= B >>.

nibble2hex(X) when X < 10 -> X + $0;
nibble2hex(X)             -> X - 10 + $a.

check_files(Dir1, Suffix1, Dir2, Suffix2, CheckFunction) ->
    Files =
      tpk_file:walk(fun (Path, Acc) ->
                            case lists:suffix(Suffix1, Path) of
                                true ->
                                    InOtherDir = filename:join(Dir2, tpk_file:rebase_filename(Path, Dir1, Dir2)),
                                    WithoutSuffix = string:substr(InOtherDir, 1, length(InOtherDir) - length(Suffix1)),
                                    OtherPath = WithoutSuffix ++ Suffix2,
                                    case filelib:is_regular(OtherPath) of
                                        true ->
                                            case CheckFunction(Path, OtherPath) of
                                                true  -> [{Path, OtherPath} | Acc];
                                                false -> Acc
                                            end;
                                        false ->
                                            [{Path, OtherPath} | Acc]
                                    end;
                                false -> Acc
                            end
                    end, [], Dir1),
    case Files of
        [] -> done;
        _  -> {needs_run, Files}
    end.

check_files(Files, Fun) ->
    FilterFun = fun ({P1, P2}) -> Fun(P1, P2) end,
    case lists:filter(FilterFun, Files) of
        []       -> done;
        Filtered -> {needs_run, Filtered}
    end.

check_files_mtime(Dir1, Suffix1, Dir2, Suffix2) ->
    check_files(Dir1, Suffix1, Dir2, Suffix2, fun check_mtime/2).

check_files_mtime(Files) ->
    check_files(Files, fun check_mtime/2).

check_files_exist(Dir1, Suffix1, Dir2, Suffix2) ->
    check_files(Dir1, Suffix1, Dir2, Suffix2, fun (_, _) -> true end).

check_mtime(File1, File2) ->
    case file:read_file_info(File1) of
        {error, _} -> false;
        {ok, #file_info{mtime = MTime1}} ->
            case file:read_file_info(File2) of
                {error, _} -> true;
                {ok, #file_info{mtime = MTime2}} ->
                    MTime1 >= MTime2
            end
    end.

varsubst(Text, Variables) ->
    BinText = iolist_to_binary(Text),
    case re:run(BinText, "@@([^@ ]+)@@", [global, {capture, all, index}, unicode]) of
        nomatch -> BinText;
        {match, Matches} ->
            vs_replace(Matches, 0, BinText, <<>>, Variables)
    end.

varsubst_file(Infile, Variables) ->
    {ok, Content} = file:read_file(Infile),
    varsubst(Content, Variables).

vs_replace([], _Offset, TextRest, Result, _Vars) -> <<Result/bytes, TextRest/bytes>>;
vs_replace([[{Start, Len}, {VStart, VLen}] | RM], Offset, Text, Result, Vars) ->
    VDLen = VStart - Start, BStart = Start - Offset,
    <<Before:BStart/bytes, _:VDLen/bytes, Var:VLen/bytes, _:VDLen/bytes, After/bytes>> = Text,
    NewResult = case proplists:get_value(binary_to_list(Var), Vars) of
                    undefined ->
                        tetrapak:fail("varsubst: undefined variable ~s", [Var]);
                    Value ->
                        PP = list_to_binary(f("~s", [Value])),
                        <<Result/bytes, Before/bytes, PP/bytes>>
                end,
    vs_replace(RM, Offset + BStart + Len, After, NewResult, Vars).

fold_tree(Fun, Acc, Tree) ->
    Iter = gb_trees:iterator(Tree),
    fold_tree1(Fun, Acc, Iter).
fold_tree1(Fun, Acc, Iterator) ->
    case gb_trees:next(Iterator) of
        none              -> Acc;
        {Key, Val, Iter2} -> fold_tree1(Fun, Fun({Key, Val}, Acc), Iter2)
    end.

cmd(Dir, Command, Args) ->
    run_cmd(Dir, Command, Args, fun (Port) -> capture_output(Command, Port, <<>>) end).
outputcmd(Dir, Command, Args) ->
    run_cmd(Dir, Command, Args, fun (Port) -> relay_output(Command, Port) end).

run_cmd(Dir, Command, Args, OutputHandler) ->
    case os:find_executable(Command) of
        false ->
            return_einfo({cmd_spawn, Command, not_found});
        Executable ->
            PortOptions = [{cd, Dir}, {args, Args}, stream, binary, use_stdio, stderr_to_stdout, in, exit_status, hide],
            case catch erlang:open_port({spawn_executable, Executable}, PortOptions) of
                {'EXIT', Reason} ->
                    return_einfo({cmd_spawn, Command, Reason});
                Port when is_port(Port) ->
                    OutputHandler(Port)
            end
    end.

relay_output(Command, Port) ->
    receive
        {'EXIT', Port, Reason} ->
            port_close(Port),
            return_einfo({cmd_port_exit, Command, Reason});
        {Port, {data, Data}} ->
            io:put_chars(Data),
            relay_output(Command, Port);
        {Port, {exit_status, ExitStatus}} ->
            {ok, ExitStatus};
        OtherMsg ->
            ?DEBUG("relay_output other msg: ~p", [OtherMsg])
    end.

capture_output(Command, Port, Buffer) ->
    receive
        {'EXIT', Port, Reason} ->
            port_close(Port),
            return_einfo({cmd_port_exit, Command, Reason});
        {Port, {data, Data}} ->
            capture_output(Command, Port, <<Buffer/binary, Data/binary>>);
        {Port, {exit_status, ExitStatus}} ->
            {ok, ExitStatus, Buffer};
        OtherMsg ->
            ?DEBUG("capture_output other msg: ~p", [OtherMsg])
    end.

return_einfo(Error) ->
    {error, {undefined, ?MODULE, Error}}.

format_error({cmd_spawn, Cmd, not_found}) ->
    "command not found: " ++ Cmd;
format_error({cmd_spawn, Cmd, Error}) ->
    Cmd ++ ": " ++ file:format_error(Error);
format_error({cmd_port_exit, Cmd, Error}) ->
    Cmd ++ ": " ++ file:format_error(Error).

%% ------------------------------------------------------------
%% -- getopt-style option parsing
parse_cmdline(Args, OptionDesc, ArgDesc) ->
    Defaults = [{Key, Value} || {_Type, Key, _Flags, Value} <- OptionDesc],
    case parse_options(Args, OptionDesc, ArgDesc, false, Defaults) of
        {Result, []} ->
            {ok, Result};
        {_Result, Missing} ->
            MissingNames = lists:map(fun ({arg, Name}) -> f("~s", [Name]) end, Missing),
            io:format("error: missing arguments: ~s~n", [string:join(MissingNames, ", ")]),
            {error, {missing, MissingNames}}
    end.

parse_options([], _OptDesc, ArgDesc, _NextIsArg, Result) ->
    {Result, ArgDesc};
parse_options([Arg | Rest], OptDesc, ArgDesc, NextIsArg, Result) ->
    case Arg of
        "--" ->
            parse_options(Rest, OptDesc, ArgDesc, true, Result);
        "-"  ++ _Name when not NextIsArg ->
            {{Key, Val}, TheRest} = parse_option(Arg, Rest, OptDesc),
            NewResult = lists:keyreplace(1, Key, Result, {Key, Val}),
            parse_options(TheRest, OptDesc, ArgDesc, NextIsArg, NewResult);
        _ ->
            case ArgDesc of
                [{arg, Name} | ArgDescRest] ->
                    NewResult = lists:keyreplace(1, Name, Result, {Name, Arg}),
                    parse_options(Rest, OptDesc, ArgDescRest, NextIsArg, NewResult);
                [] ->
                    io:format("error: superfluous argument: ~p~n", [Arg]),
                    throw({error, {superfluous_argument, Arg}})
            end
    end.

parse_option(Option, Rest, OptDesc) ->
    case find_option(Option, OptDesc) of
        {option_arg, Name, _Flags, _Default} ->
            case Rest of
                [Value | TheRest] ->
                    {{Name, Value}, TheRest};
                [] ->
                    io:format("error: option ~s requires an argument~n", [Option]),
                    throw({error, {missing_option_arg, Name}})
            end;
        {flag, Name, _Flags, _Default} ->
            {{Name, true}, Rest};
        undefined ->
            io:format("error: unknown option ~s~n", [Option]),
            throw({error, {unknown_option, Option}})
    end.

find_option(_Option, []) -> undefined;
find_option(Option, [Opt = {_, _Name, Flags, _Default} | Rest]) ->
    case lists:member(Option, Flags) of
        false -> find_option(Option, Rest);
        true -> Opt
    end.

%% @hidden
debug_log_to_stderr(Fmt, Args) ->
    case application:get_env(tetrapak, debug) of
        {ok, true} ->
            io:fwrite(standard_error, "---- ~s -- " ++ Fmt ++ "\n",
                      [io_lib:write(self()) | Args]);
        _ ->
            ok
    end.
