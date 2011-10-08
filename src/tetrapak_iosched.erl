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