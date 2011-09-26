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

-module(tetrapak).
-export([version/0, run/2, cli_main/1]).
-export([get/1, get/2, require/1, require_all/1, dir/0, subdir/1, fail/0, fail/1, fail/2,
         config/1, config/2, config_path/1, config_path/2]).
-export([cmd/2, cmd/3, outputcmd/2, outputcmd/3]).
-compile({no_auto_import, [get/1]}).

-include("tetrapak.hrl").

%% ------------------------------------------------------------
%% -- Ext API
run(Directory, TaskCmds) ->
    {ok, Context} = tetrapak_context:new(Directory),
    case tetrapak_context:run_sequentially(Context, ["tetrapak:boot" | TaskCmds]) of
        ok                          -> ok;
        {error, {unknown_key, Key}} -> {unknown, Key};
        {error, _}                  -> error
    end.

cli_main([_ | CliArgs]) ->
    {ok, Cwd} = file:get_cwd(),

    %% ensure the app file is loaded
    application:load(tetrapak),

    %% start our error logger
    tetrapak_error_logger:swapin(),

    case os:getenv("DEBUG") of
        Value when (Value == "1") or (Value == "true") ->
            application:set_env(tetrapak, debug, true);
        _ ->
            ok
    end,

    %% add the current project's ebin to the front
    EbinDir = filename:join(Cwd, "ebin"),
    case filelib:is_dir(EbinDir) of
        true ->
            code:add_patha(EbinDir);
        false ->
            ok
    end,

    %% process command-line options
    RunTasks = case CliArgs ++ init:get_plain_arguments() of
                   [] -> ["tetrapak:info"];
                   As -> As
               end,

    case tetrapak:run(Cwd, RunTasks) of
        {unknown, Key} ->
            io:format(standard_error, "Error: no such task: ~s~n", [Key]),
            halt(1);
        error ->
            halt(2);
        ok ->
            halt(0)
    end.

%% ------------------------------------------------------------
%% -- Task API
version() ->
    get("tetrapak:boot:version").

dir() ->
    tetrapak_task:directory().

subdir(Dir) ->
    filename:join(tetrapak_task:directory(), Dir).

require(Key) ->
    tetrapak_task:require_all([Key]).

require_all(Keys) ->
    tetrapak_task:require_all(Keys).

get(Key) ->
    case tetrapak_task:get(Key) of
        {ok, Value}          -> Value;
        {error, unknown_key} -> fail("get() of unknown key: ~s", [Key])
    end.

get(Key, Default) ->
    case tetrapak_task:get(Key) of
        {ok, Value} -> Value;
        {error, unknown_key} -> Default
    end.

fail() ->
    tetrapak_task:fail().
fail(Reason) ->
    tetrapak_task:fail(Reason, []).
fail(Fmt, Args) ->
    tetrapak_task:fail(Fmt, Args).

config(Key) ->
    case tetrapak_task:get_config(Key) of
        {ok, Value}          -> Value;
        {error, unknown_key} -> fail("unknown configuration parameter: ~s", [Key])
    end.

config(Key, Default)      ->
    case tetrapak_task:get_config(Key) of
        {ok, Value}          -> Value;
        {error, unknown_key} -> Default
    end.

config_path(Key)          -> subdir(config(Key)).
config_path(Key, Default) -> subdir(config(Key, Default)).

%% run and capture output to binary
cmd(Cmd, Args) ->
    cmd(dir(), Cmd, Args).
cmd(Dir, Cmd, Args) ->
    case tpk_util:cmd(Dir, Cmd, Args) of
        {ok, 0, Output}           -> Output;
        {ok, _Other, _Output}     -> fail("exit status non-zero: ~s ~s", [Cmd, string:join(Args, " ")]);
        {error, {_L, Mod, Error}} -> fail("error running command ~s: ~s", [Cmd, Mod:format_error(Error)])
    end.

%% run and display output
outputcmd(Cmd, Args) ->
    outputcmd(dir(), Cmd, Args).
outputcmd(Dir, Cmd, Args) ->
    case tpk_util:outputcmd(Dir, Cmd, Args) of
        {ok, 0}                   -> ok;
        {ok, _Other}              -> fail("exit status non-zero: ~s ~s", [Cmd, string:join(Args, " ")]);
        {error, {_L, Mod, Error}} -> fail("error running command ~s: ~s", [Cmd, Mod:format_error(Error)])
    end.
