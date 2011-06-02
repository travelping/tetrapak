%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak).
-export([all_commands/0, run/2]).
-export([get/1, get/2, require/1, require_all/1, dir/0, subdir/1, fail/1, fail/2]).
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
all_commands() ->
    [{Task#task.name, Task#task.description} || {_, Task} <- lists:keysort(1, tetrapak_task:find_tasks())].

run(Directory, TaskCmds) ->
    Context = tetrapak_context:new(Directory),
    case tetrapak_context:wait_for(Context, TaskCmds) of
        ok ->
            ok;
        {error, {unknown_key, Key}} ->
            {unknown, Key};
        {error, {failed, _Task}} ->
            error; % unlikely
        {error, shutdown} ->
            error
    end.

%% ------------------------------------------------------------
%% -- Task API
dir() ->
    tetrapak_context:get_directory(tetrapak_task:context()).

subdir(Dir) ->
    filename:join(dir(), Dir).

require(Key) ->
    tetrapak_task:require_all([Key]).

require_all(Keys) ->
    tetrapak_task:require_all(Keys).

get(Key) ->
    {ok, Value} = tetrapak_task:get(Key, true),
    Value.

get(Key, Default) ->
    case tetrapak_task:get(Key, false) of
        {ok, Value} -> Value;
        {error, unknown_key} -> Default
    end.

fail(Reason) ->
    tetrapak_task:fail(Reason, []).
fail(Fmt, Args) ->
    tetrapak_task:fail(Fmt, Args).
