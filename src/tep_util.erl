%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_util).
-export([f/1, f/2, match/2, set_prop/3, find_module/1]).
-export([run/2, run/3]).
-export([varsubst/2]).
-export([parse_cmdline/3]).

f(Str) -> f(Str,[]).
f(Str, Args) -> lists:flatten(io_lib:format(Str, Args)).

match(Re, String) ->
  case re:run(String, Re, [{capture, none}]) of
    match -> true;
    nomatch -> false
  end.

find_module(Name) ->
  case catch Name:module_info() of
    {'EXIT', _} -> {error, bad_module};
    _ -> {ok, Name}
  end.

set_prop(SetKey, SetValue, Alist) ->
  F = fun ({Key, Value}, {Found, Res}) ->
      if (Key == SetKey) -> {true,  [{Key, SetValue} | Res]};
         true            -> {Found, [{Key, Value} | Res]}
      end
  end,
  {Found, NewAlist} = lists:foldl(F, {false, []}, Alist),
  if Found -> NewAlist;
     true  -> [{SetKey, SetValue} | NewAlist]
  end.

run(Prog, Args) ->
  {ok, Cwd} = file:get_cwd(),
  run(Prog, Args, Cwd).
run(Prog, Args, Dir) ->
  tep_log:debug("running command ~s ~s", [Prog, string:join(Args, " ")]),
  case os:find_executable(Prog) of
    false -> {error, no_such_program};
    Cmd ->
      Port = erlang:open_port({spawn_executable, Cmd},
                              [{cd, Dir}, in, exit_status, stderr_to_stdout,
                               {args, Args}, {line, 400}]),
      display_output(Port)
  end.

display_output(Port) ->
  receive
    {Port, {data, {Eol, Line}}} ->
      tep_log:output(case Eol of
          eol -> "~s~n";
          noeol -> "~s"
        end, [Line]),
      display_output(Port);
    {Port, {exit_status, 0}} ->
      {exit, ok};
    {Port, {exit_status, _}} ->
      {exit, error};
    {Port, closed} ->
      {exit, error}
  end.

varsubst(Text, Variables) ->
  BinText = iolist_to_binary(Text),
  case re:run(BinText, "@@(\\S+)@@", [global, {capture, all, index}, unicode]) of
    nomatch -> BinText;
    {match, Matches} ->
      vs_replace(Matches, 0, BinText, <<>>, Variables)
  end.

vs_replace([], _Offset, TextRest, Result, _Vars) -> <<Result/bytes, TextRest/bytes>>;
vs_replace([[{Start, Len}, {VStart, VLen}] | RM], Offset, Text, Result, Vars) ->
  VDLen = VStart - Start, BStart = Start - Offset,
  <<Before:BStart/bytes, _:VDLen/bytes, Var:VLen/bytes, _:VDLen/bytes, After/bytes>> = Text,
  NewResult = case proplists:get_value(binary_to_list(Var), Vars) of
    undefined ->
      tep_log:warn("varsubst: undefined variable ~s", [Var]),
      <<Result/bytes, Before/bytes>>;
    Value ->
      PP = list_to_binary(f("~s", [Value])),
      <<Result/bytes, Before/bytes, PP/bytes>>
  end,
  vs_replace(RM, Offset + BStart + Len, After, NewResult, Vars).


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
      parse_options(TheRest, OptDesc, ArgDesc, NextIsArg, set_prop(Key, Val, Result));
    _ ->
      case ArgDesc of
        [{arg, Name} | ArgDescRest] ->
          parse_options(Rest, OptDesc, ArgDescRest, NextIsArg, set_prop(Name, Arg, Result));
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
