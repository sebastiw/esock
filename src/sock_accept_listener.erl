-module(sock_accept_listener).

-export([give_control/1
        ]).

-export([init/1
        ]).

give_control(LSock) ->
    Me = self(),
    ListenerP = proc_lib:spawn_link(?MODULE, init, [[Me, LSock]]),
    %% ok = socket:setopt(LSock, {otp, controlling_process}, ListenerP),
    ListenerP ! start,
    ListenerP.

init([Parent, LSock]) ->
    process_flag(trap_exit, true),
    receive
        start ->
            accept_loop(Parent, LSock)
    end.

accept_loop(Parent, LSock) ->
    case socket:accept(LSock) of
        {error, _Err} = Error ->
            Error;
        {ok, Sock} ->
            %% ok = socket:setopt(Sock, {otp, controlling_process}, Parent),
            Parent ! {accepted, Sock},
            accept_loop(Parent, LSock)
    end.
