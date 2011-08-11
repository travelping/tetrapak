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

-module(tetrapak_io).
-export([start/0, can_start_shell/0, start_shell/0, start_shell/1]).
-export([ioreq_output/1]).

-include("tetrapak.hrl").
-define(SERVER, tetrapak_io).

start() ->
    proc_lib:spawn(fun () -> server({fd,1,1}) end).

start_shell() ->
    start_shell({shell, start, [false]}).
start_shell(Shell = {_M,_F,_A}) ->
    whereis(?SERVER) ! {start_shell, Shell},
    ok.

can_start_shell() ->
    case whereis(?SERVER) of
        undefined        -> false;
        P when is_pid(P) -> true
    end.

server(PortName) ->
    Group = group:start(self(), {}),
    register(user, Group),
    Port = open_port(PortName, [out]),
    register(?SERVER, self()),
    server_loop(Port, Group).

server_loop(Port, Group) ->
    receive
        {io_request, From, ReplyAs, Request} when is_pid(From) ->
            {Reply, Chars} = ioreq_output(Request),
            port_command(Port, Chars),
            From ! {io_reply, ReplyAs, Reply},
            server_loop(Port, Group);
        {Group, get_unicode_state} ->
            Group ! {self(), get_unicode_state, true},
            server_loop(Port, Group);
        {Group, set_unicode_state} ->
            Group ! {self(), set_unicode_state, true},
            server_loop(Port, Group);
        {Group, Req} ->
            {_Reply, Chars} = ioreq_output(Req),
            port_command(Port, Chars),
            server_loop(Port, Group);
        {start_shell, Shell} ->
            erlang:unregister(?SERVER),
            user_drv:server('tty_sl -c -e', Shell);
        _Other ->
            ?DEBUG("io other: ~p", [_Other]),
            server_loop(Port, Group)
    end.

ioreq_output({put_chars, Encoding, Chars}) ->
    {ok, unicode:characters_to_binary(Chars, Encoding)};
ioreq_output({put_chars, Encoding, M, F, A}) ->
    case output_function(M, F, A) of
        {ok, Output} ->
            {ok, unicode:characters_to_binary(Output, Encoding)};
        Error ->
            {Error, []}
    end;
ioreq_output({put_chars, Chars}) ->
    {ok, Chars};
ioreq_output({put_chars, M, F, A}) ->
    output_function(M, F, A);
ioreq_output({requests, Requests}) ->
    io_requests(Requests, []);
ioreq_output(_OtherRequest) ->
    {{error, enotsup}, []}.

output_function(M, F, A) ->
    try apply(M, F, A) of
        Output when is_list(Output); is_binary(Output) ->
            {ok, Output};
        _ ->
            {{error, F}, []}
    catch
        _:_ ->
            {{error, F}, []}
    end.

%% Process a list of output requests as long as the previous status is 'ok'.
io_requests([R|Rs], Acc) ->
    case ioreq_output(R) of
        {ok, Chars} ->
            io_requests(Rs, [Chars | Acc]);
        {ErrorReply, _Chars} ->
            {ErrorReply, lists:reverse(Acc)}
    end;
io_requests([], Acc) ->
    {ok, lists:reverse(Acc)}.
