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
-export([get/1, get/2, require/1, require_all/1, dir/0, path/1, subdir/1, fail/0, fail/1, fail/2,
         config/1, config/2, config_path/1, config_path/2, extract_app_name/1, extract_app_name/2]).
-export([cmd/2, cmd/3, outputcmd/2, outputcmd/3]).
-compile({no_auto_import, [get/1]}).

-include("tetrapak.hrl").

%% @private
run(Directory, TaskCmds) ->
    tetrapak_iosched:ensure_started(),
    Context = tetrapak_context:new(Directory),
    RunTasks = ["tetrapak:debug_setup", "tetrapak:boot" | TaskCmds],
    tetrapak_context:add_directory(Context, Directory),
    case tetrapak_context:run_sequentially(Context, Directory, RunTasks) of
        ok                          -> ok;
        {error, {unknown_key, Key}} -> {unknown, Key};
        {error, _}                  -> error;
        {context_exit, Reason}      ->
            io:format("!!! context died: ~p~n", [Reason]),
            error
    end.

%% @private
cli_main([_ | CliArgs]) ->
    {ok, Cwd} = file:get_cwd(),

    %% ensure the app file is loaded
    application:load(tetrapak),

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

%% @doc Get the app name from a app specified in configuration
-spec extract_app_name(string()) -> atom().
extract_app_name(App) ->
    extract_app_name(App, "").

%% @doc Get the app name from a app specified in configuration with adding of specifical prefix
-spec extract_app_name(string(), string()) -> atom().
extract_app_name(App, Prefix) ->
    [AppString | _] = string:tokens(App, " "),
    list_to_atom(Prefix ++ AppString).

%% @doc Get the version of tetrapak currently running.
-spec version() -> string().
version() ->
    get("tetrapak:boot:version").

%% @doc Get the current task's working directory.
-spec dir() -> file:filename().
dir() ->
    tetrapak_task:directory().

%% @doc Make a filename absolute against the current task's working directory.
-spec path(string()) -> file:filename().
path(Filename) ->
    filename:join(tetrapak:dir(), Filename).

%% @private
subdir(FN) ->
    path(FN).

%% @equiv require_all([Key])
-spec require(string()) -> ok.
require(Key) ->
    tetrapak_task:require_all([Key]).

%% @doc Wait for completion of several tasks before continuing execution.
-spec require_all([string(), ...]) -> ok.
require_all(Keys) ->
    tetrapak_task:require_all(Keys).

%% @doc Get a tasks return value.
%%    This function requires the task that is expected to
%%    contain the given return value key and then returns
%%    it's value. <br/><br/>
%%    Example use:
%%    ```
%%    tetrapak:get("config:vcs:branch")//
%%    '''
-spec get(string()) -> term().
get(Key) ->
    case tetrapak_task:get(Key) of
        {ok, Value}          -> Value;
        {error, unknown_key} -> fail("get() of unknown key: ~s", [Key])
    end.

%% @doc Get a tasks return value.
%%    Works like {@link get/1}, except that ``Default'' is.
%%    returned if ``Key'' refers to an unknown task or return value key.
%%    <br/>
%%    Use this function with care because it somewhat obscures dependency problems.
-spec get(string(), term()) -> term().
get(Key, Default) ->
    case tetrapak_task:get(Key) of
        {ok, Value} -> Value;
        {error, unknown_key} -> Default
    end.

%% @doc Exit the current task.
%%    See {@link fail/2}
-spec fail() -> no_return().
fail() ->
    tetrapak_task:fail().

%% @doc Exit the current task.
%%    See {@link fail/2}
-spec fail(string()) -> no_return().
fail(Reason) ->
    tetrapak_task:fail(Reason, []).

%% @doc Exit the current task.
%%    This function displays a message,
%%    expanding tilde escape sequences like {@link io:format/2},
%%    then exits the current task. The failure will propagate to
%%    all other tasks which are currently running. If tetrapak
%%    was executed from the command line, the Erlang VM will be halted
%%    with a non-zero status code.
-spec fail(string(), list(term())) -> no_return().
fail(Fmt, Args) ->
    tetrapak_task:fail(Fmt, Args).

%% @doc Read a value from the configuration.
%%    This function will fail the current task if an unknown configuration parameter is specified.
-spec config(string()) -> term().
config(Key) ->
    case tetrapak_task:get_config(Key) of
        {ok, Value} -> Value;
        false       -> fail("unknown configuration parameter: ~s", [Key])
    end.

%% @doc Read a value from the configuration.
%%    This function returns Default if no value could be found for Key.
-spec config(string(), term()) -> term().
config(Key, Default)      ->
    case tetrapak_task:get_config(Key) of
        {ok, Value} -> Value;
        false       -> Default
    end.

%% @doc Read an absolute filename from the configuration.
%%    If the filename is given relative in the config file
%%    it will be resolved against the current task's working directory.
%%    This function fails the current task if an unknown configuration parameter is specified.
-spec config_path(string()) -> file:filename().
config_path(Key) -> path(config(Key)).

%% @doc Read an absolute filename from the configuration.
%%    Behaves like {@link config_path/1}, except ``Default'' is returned
%%    if the ``Key'' is unknown.
-spec config_path(string(), string()) -> file:filename().
config_path(Key, Default) -> path(config(Key, Default)).

%% @doc Run a shell command and capture it's output.
%%    The current task fails if the exit status of the command is non-zero.
%%    Use {@link tpk_util:cmd/2} if you want to handle other status codes.
-spec cmd(string(), [string()]) -> binary().
cmd(Cmd, Args) ->
    cmd(dir(), Cmd, Args).
cmd(Dir, Cmd, Args) ->
    case tpk_util:cmd(Dir, Cmd, Args) of
        {ok, 0, Output} ->
            Output;
        {ok, _Other, _Output} ->
            fail("exit status non-zero: ~s ~s", [Cmd, string:join(Args, " ")]);
        {error, {_L, Mod, Error}} ->
            fail("error running command ~s: ~s", [Cmd, Mod:format_error(Error)])
    end.

%% @doc Run a shell command and display it's output.
%%    The current task fails if the exit status of the command is non-zero.
%%    Use {@link tpk_util:outputcmd/2} if you want to handle other status codes.
-spec outputcmd(string(), [string()]) -> ok.
outputcmd(Cmd, Args) ->
    outputcmd(dir(), Cmd, Args).
outputcmd(Dir, Cmd, Args) ->
    case tpk_util:outputcmd(Dir, Cmd, Args) of
        {ok, 0} ->
            ok;
        {ok, _Other} ->
            fail("exit status non-zero: ~s ~s", [Cmd, string:join(Args, " ")]);
        {error, {_L, Mod, Error}} ->
            fail("error running command ~s: ~s", [Cmd, Mod:format_error(Error)])
    end.
