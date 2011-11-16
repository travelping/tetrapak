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
-module(tetrapak_io).
-export([start/0, can_start_shell/0, start_shell/0, start_shell/1]).
-export([ioreq_output/1]).

-include("tetrapak.hrl").

start() ->
    spawn(fun () -> server({fd,1,1}) end).

can_start_shell() ->
    case process_info(whereis(user), dictionary) of
        {dictionary, Dict} ->
            proplists:get_value(?MODULE, Dict, false);
        _ ->
            false
    end.

start_shell() ->
    start_shell({shell, start, [false]}).
start_shell(Shell = {_M,_F,_A}) ->
    whereis(user) ! {io_request, self(), user, {start_shell, Shell}},
    receive
        {io_reply, user, ok_shell_started} ->
            ok;
        {io_reply, user, _OtherReply} ->
            {error, notsup}
    after
        500 ->
            {error, timeout}
    end.

server(PortName) ->
    register(user, self()),
    put(?MODULE, true),
    group_leader(self(), self()),
    server_loop(open_port(PortName, [out])).

server_loop(Port) ->
    receive
        {io_request, From, ReplyAs, {start_shell, Shell}} ->
            ?DEBUG("io: starting shell as requested"),
            put(?MODULE, false),
            unregister(user),
            user_drv:start('tty_sl -c -e', Shell),
            timer:sleep(20),
            From ! {io_reply, ReplyAs, ok_shell_started},
            NewUser = whereis(user),
            forwarder_loop(NewUser);
        {io_request, From, ReplyAs, Request} when is_pid(From) ->
            {Reply, Chars} = ioreq_output(Request),
            port_command(Port, Chars),
            From ! {io_reply, ReplyAs, Reply},
            server_loop(Port);
        _Other ->
            ?DEBUG("io other: ~p", [_Other]),
            server_loop(Port)
    end.

forwarder_loop(User) ->
    receive
        Message ->
            User ! Message,
            forwarder_loop(User)
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
