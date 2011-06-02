%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_log).
-export([output/1,output/2,info/1,info/2,debug/1,debug/2,warn/1,warn/2]).

output(Fmt)       -> output(Fmt, []).
output(Fmt, Args) -> lprintf("", Fmt, Args).

info(Fmt)         -> info(Fmt, []).
info(Fmt, Args)   -> lprintf("%%", Fmt, Args).

warn(Fmt)         -> warn(Fmt, []).
warn(Fmt, Args)   -> lprintf("!!", Fmt, Args).

debug(Fmt) -> debug(Fmt, []).
debug(Fmt, Args) ->
    case os:getenv("DEBUG") of
        false -> ok;
        Value when (Value /= "") and (Value /= "false") and (Value /= "0") ->
            lprintf("--", Fmt, Args);
        _ -> ok
    end.

lprintf(Pre, Fmt, Args) ->
    Pid = case os:getenv("DEBUG") of
              false ->
                  "";
              Value when (Value /= "") and (Value /= "false") and (Value /= "0") ->
                  io_lib:format("~11s ", [io_lib:write(self())])
          end,
    io:put_chars(standard_error, [Pid, Pre, $ , io_lib:fwrite(Fmt, Args), $\n]).
