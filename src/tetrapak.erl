%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak).
-export([version/0, run/2, cli_main/0]).
-export([get/1, get/2, require/1, require_all/1, dir/0, subdir/1, fail/0, fail/1, fail/2,
         config/1, config/2, config_path/1, config_path/2, cmd/2]).
-compile({no_auto_import, [get/1]}).

-include("tetrapak.hrl").

%% ------------------------------------------------------------
%% -- Ext API
run(Directory, TaskCmds) ->
    Context = tetrapak_context:new(Directory),
    case tetrapak_context:run_sequentially(Context, ["tetrapak:boot" | TaskCmds]) of
        ok                          -> ok;
        {error, {unknown_key, Key}} -> {unknown, Key};
        {error, _}                  -> error
    end.

cli_main() ->
    {ok, Cwd} = file:get_cwd(),

    EbinDir = filename:join(Cwd, "ebin"),
    case filelib:is_dir(EbinDir) of
        true ->
            code:add_patha(EbinDir);
        false ->
            ok
    end,

    case init:get_plain_arguments() of
        [] ->
            run(Cwd, ["tetrapak:info"]),
            halt(1);
        Cmds ->
            case tetrapak:run(Cwd, Cmds) of
                {unknown, Key} ->
                    io:format(standard_error, "Error: no such command: ~s~n", [Key]),
                    halt(1);
                error ->
                    halt(2);
                ok ->
                    halt(0)
            end
    end.

%% ------------------------------------------------------------
%% -- Task API
version() ->
    get("tetrapak:info:version").

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

config(Key)               -> get("config:ini:" ++ Key).
config(Key, Default)      -> get("config:ini:" ++ Key, Default).
config_path(Key)          -> subdir(config(Key)).
config_path(Key, Default) -> subdir(config(Key, Default)).

cmd(Cmd, Args) ->
    case tpk_util:cmd(dir(), Cmd, Args) of
        {ok, 0, Output} ->
            Output;
        {ok, _Other, _Output} ->
            fail("exit status non-zero: ~s ~s", [Cmd, string:join(Args, " ")]);
        {error, {_L, Mod, Error}} ->
            fail("error running command ~s: ", [Cmd, Mod:format_error(Error)])
    end.
