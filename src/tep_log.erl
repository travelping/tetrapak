%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tep_log).
-export([output/1,output/2,info/1,info/2,warn/1,warn/2]).

output(Fmt) -> output(Fmt, []).
output(Fmt, Args) -> io:format(Fmt, Args).

info(Fmt) -> info(Fmt, []).
info(Fmt, Args) -> io:format("== " ++ Fmt ++ "~n", Args).

warn(Fmt) -> warn(Fmt, []).
warn(Fmt, Args) -> io:format("!! " ++ Fmt ++ "~n", Args).
