%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(tpk).
-export([r/1, h/0, l/0, b/0, bl/0, c/0]).

r(Cmd) -> run([Cmd]).
h()    -> run(["tetrapak:info", "tetrapak:tpk-help"]).
c()    -> r("check").
l()    -> r("tetrapak:reload").
b()    -> r("build").
bl()   -> run(["build", "tetrapak:reload"]).

run(Tasks) ->
    {ok, Cwd} = file:get_cwd(),
    tetrapak:run(Cwd, Tasks).
