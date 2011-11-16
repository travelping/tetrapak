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

%% @private
-module(tetrapak_iosched).
-export([ensure_started/0, want_output/1]).
-export([init/0]).

ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            proc_lib:spawn(?MODULE, init, []);
        Pid ->
            Pid
    end.

want_output(Pid) ->
    ?MODULE ! {wants_output, Pid}.

%% ------------------------------------------------------------
%% -- server loop
init() ->
    process_flag(trap_exit, true),
    register(?MODULE, self()),
    loop(queue:new()).

loop(Queue) ->
    receive
        {wants_output, Pid} ->
            link(Pid),
            loop(push_ioqueue(Queue, Pid));
        {'EXIT', Pid, _Reason} ->
            case queue:out(Queue) of
                {{value, Pid}, QueueRest} ->
                    case queue:peek(QueueRest) of
                        {value, NextPid} ->
                            NextPid ! {?MODULE, output_ok};
                        empty ->
                            ok
                    end,
                    loop(QueueRest);
                _ ->
                    loop(queue:filter(fun (X) -> X /= Pid end, Queue))
            end
    end.

push_ioqueue(Queue, Pid) ->
    case queue:is_empty(Queue) of
        true  -> Pid ! {?MODULE, output_ok};
        false -> ok
    end,
    queue:in(Pid, Queue).