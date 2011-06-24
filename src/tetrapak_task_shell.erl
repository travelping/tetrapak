%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tetrapak_task_shell).
-behaviour(tetrapak_task).
-export([run/2]).

run("shell", _) ->
    code:ensure_loaded(tpk),
    case tetrapak_io:can_start_shell() of
        true ->
            tetrapak_io:start_shell(),
            timer:sleep(infinity);
        false ->
            tetrapak:fail("Cannot start shell")
    end;

run("tetrapak:reload", _) ->
    EbinDir = tetrapak:subdir("ebin"),
    Modules = lists:map(fun (F) -> {list_to_atom(filename:basename(F, ".beam")), filename:join(EbinDir, F)} end, filelib:wildcard("*.beam", EbinDir)),
    lists:foreach(fun ({Mod, Path}) ->
                          case code:is_loaded(Mod) of
                              {file, _} ->
                                  DiskMtime = tpk_file:mtime(Path),
                                  (DiskMtime > loaded_mtime(Mod)) andalso load(Mod);
                              false ->
                                  load(Mod)
                          end
                  end, Modules).

loaded_mtime(Mod) ->
    {Y,Mon,D,H,Min,S} = proplists:get_value(time, Mod:module_info(compile)),
    calendar:universal_time_to_local_time({{Y,Mon,D},{H,Min,S}}).

load(Mod) ->
    io:format("load ~s~n", [Mod]),
    code:purge(Mod),
    code:load_file(Mod).
