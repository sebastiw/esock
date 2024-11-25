-module(sock_assoc).
-behaviour(gen_server).

%% API
-export([start_link/4
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

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init([Sock, RemoteAddrs, RemotePort, AssocOpts]) ->
    State = #{socket => Sock,
              remote_addrs => RemoteAddrs,
              remote_port => RemotePort,
              options => AssocOpts},
    {ok, State, {continue, connect}}.

handle_continue(connect, State) ->
    Sock = maps:get(socket, State),
    Addrs = maps:get(remote_addrs, State),
    Port = maps:get(remote_port, State),
    Opts = maps:get(options, State),
    connect_async(Sock, Addrs, Port, Opts),
    {noreply, State}.

handle_info(What, State) ->
    io:format("sock_assoc: handle_info: ~p~n", [What]),
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call(_What, _From, State) ->
    {reply, undefined, State}.

terminate(_What, _State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

connect_async(Sock, Addrs, Port, _Opts) when ?USE_SOCKET ->
    [socket:connect(Sock, A, Port) || A <- Addrs];
connect_async(Sock, Addrs, Port, _Opts) ->
    Parent = self(),
    {ok, Domain} = sock_utils:get_domain(Addrs, _Opts),
    SockAddrs = [sock_utils:socket_address(Domain, A, Port) || A <- Addrs],
    spawn_link(fun () -> client_loop(Sock, SockAddrs, Parent) end).

client_loop(Sock, [SockAddr|SockAddrs], Parent) ->
    %% connectx_init does not work that well?
    case gen_sctp:connect(Sock, SockAddr, [{sctp_autoclose, 100}]) of
        {ok, Ass} ->
            Parent ! {comm_up, Ass};
        {error, _} = Error ->
            Parent ! Error,
            client_loop(Sock, SockAddrs ++ [SockAddr], Parent)
    end.

