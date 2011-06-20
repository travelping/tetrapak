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
         config/1, config/2, config_path/1, config_path/2]).
-compile({no_auto_import, [get/1]}).

-include("tetrapak.hrl").

% increase_version(#tep_project{vsn = Version, app_file = AppFile}) ->
%    VComps = re:split(Version, "\\.", [{return, list}]),
%    NewLast = integer_to_list(list_to_integer(lists:last(VComps)) + 1),
%    NewVL = lists:append(lists:sublist(VComps, length(VComps) - 1), [NewLast]),
%    NewV = string:join(NewVL, "."),
%    tep_log:info("increasing version in project app file from ~s to ~s", [Version, NewV]),
%    {ok, AppContents} = file:read_file(AppFile),
%    NewContents = re:replace(AppContents,
%                             "(\\{\\s*vsn\\s*,\\s*)(\"[^\"]+\")(\\s*\\})",
%                             "\\1\"" ++ NewV ++ "\"\\3",
%                             [{return, binary}]),
%    file:write_file(AppFile, NewContents).

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
