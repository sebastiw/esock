-module(sock_busy_connecter).

-export([give_control/2
        ]).

-export([init/1
        ]).

give_control(Sock, RAddr) ->
    Opts = #{timeout => 1000,
             parent => self(),
             socket => Sock,
             remote_addr => RAddr
            },
    ConnectorP = proc_lib:spawn_link(?MODULE, init, [Opts]),
    %% ok = socket:setopt(Sock, {otp, controlling_process}, ConnectorP),
    ConnectorP ! start,
    ConnectorP.

init(Opts) ->
    process_flag(trap_exit, true),
    receive
        start ->
            connect_loop(Opts)
    end.

connect_loop(Opts) ->
    #{timeout := Timeout,
      parent := Parent,
      socket := Sock,
      remote_addr := RAddr
     } = Opts,
    timer:sleep(Timeout),
    case socket:connect(Sock, RAddr) of
        {error, econnrefused} ->
            connect_loop(Opts);
        {error, _Err} = Error ->
            io:format("XXX_SWO ~p:~p:~B (~p)~n", [?MODULE, ?FUNCTION_NAME, ?LINE, Error]),
            Error;
        ok ->
            io:format("XXX_SWO ~p:~p:~B~n", [?MODULE, ?FUNCTION_NAME, ?LINE]),
            %% ok = socket:setopt(Sock, {otp, controlling_process}, Parent),
            Parent ! {connected, Sock}
    end.
