-module(sock_assoc).
-behaviour(gen_server).

%% API
-export([start_link/4,
         get_paths/1
        ]).

%% Callbacks
-export([init/1,
         handle_continue/2,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2
        ]).

-include("sock.hrl").

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_link(Sock, LocalAddrs, LocalPort, LocalOpts) ->
    gen_server:start_link(?MODULE, [Sock, LocalAddrs, LocalPort, LocalOpts], []).

get_paths(Assoc) ->
    %% TBD
    gen_server:call(Assoc, get_paths).

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init([Sock, RemoteAddrs, RemotePort, AssocOpts]) ->
    State = #{socket => Sock,
              remote_addrs => RemoteAddrs,
              remote_port => RemotePort,
              options => AssocOpts,
              paths => []},
    {ok, State, {continue, connect}}.

handle_continue(connect, State) ->
    Sock = maps:get(socket, State),
    RAddrs = maps:get(remote_addrs, State),
    RPort = maps:get(remote_port, State),
    Opts = maps:get(options, State),
    connect_async(Sock, RAddrs, RPort, Opts),
    {noreply, State}.

handle_info({comm_up, S, SockAddr}, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, {comm_up, S}]),
    {ok, Path} = sock_path:create_path(SockAddr),
    Paths = maps:get(paths, State),
    {noreply, State#{paths => [Path|Paths]}};
handle_info(What, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, What]),
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call(get_paths, _From, State) ->
    PathPids = maps:get(paths, State),
    RPort = maps:get(remote_port, State),
    PAddrs = [sock_path:get_path(P) || P <- PathPids],
    Paths = {self(), PAddrs, RPort},
    {reply, {ok, Paths}, State};
handle_call(_What, _From, State) ->
    {reply, undefined, State}.

terminate(_What, _State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

connect_async(Sock, Addrs, Port, _Opts) ->
    Parent = self(),
    {ok, Domain} = sock_utils:get_domain(Addrs, _Opts),
    SockAddrs = [sock_utils:socket_address(Domain, A, Port) || A <- Addrs],
    spawn_link(fun () -> client_loop(Sock, SockAddrs, Parent) end).

-ifdef(USE_SOCKET).
client_loop(Sock, [SockAddr|SockAddrs], Parent) ->
    case socket:connect(Sock, SockAddr, 500) of
        ok ->
            Parent ! {comm_up, Sock, SockAddr};
        {error, _} = Error ->
            Parent ! Error,
            client_loop(Sock, SockAddrs ++ [SockAddr], Parent)
    end.
-else.
client_loop(Sock, [SockAddr|SockAddrs], Parent) ->
    %% connectx_init does not work that well?
    case gen_sctp:connect(Sock, SockAddr, [{sctp_autoclose, 500}]) of
        {ok, Ass} ->
            Parent ! {comm_up, Ass, SockAddr};
        {error, _} = Error ->
            Parent ! Error,
            client_loop(Sock, SockAddrs ++ [SockAddr], Parent)
    end.
-endif.
