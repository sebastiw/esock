-module(esock_assoc).
-behaviour(gen_server).

%% API
-export([start_link/6,
         connect/1,
         send/2,
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

-include("esock.hrl").

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_link(Backend, Sock, RemoteAddrs, RemotePort, RemoteOpts, CallbackPid) ->
    gen_server:start_link(?MODULE, [Backend, Sock, RemoteAddrs, RemotePort, RemoteOpts, CallbackPid], []).

connect(Assoc) ->
    gen_server:call(Assoc, connect).

send(Assoc, Data) ->
    gen_server:cast(Assoc, {send, Data}).

get_paths(Assoc) ->
    %% TBD
    gen_server:call(Assoc, get_paths).

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init([Backend, Sock, RemoteAddrs, RemotePort, AssocOpts, CallbackPid]) ->
    State = #{backend => Backend,
              socket => Sock,
              remote_addrs => RemoteAddrs,
              remote_port => RemotePort,
              options => AssocOpts,
              paths => [],
              callback_pid => CallbackPid},
    {ok, State}.

handle_continue(connect, State) ->
    Backend = maps:get(backend, State),
    Sock = maps:get(socket, State),
    RAddrs = maps:get(remote_addrs, State),
    RPort = maps:get(remote_port, State),
    Opts = maps:get(options, State),
    connect_async(Backend, Sock, RAddrs, RPort, Opts),
    {noreply, State}.

handle_info({comm_up, S, SockAddr}, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, {comm_up, S, SockAddr}]),
    Backend = maps:get(backend, State),
    CallbackPid = maps:get(callback_pid, State),
    spawn_link(fun () -> client_recv(Backend, S, CallbackPid) end),
    {ok, Path} = esock_path:create_path(SockAddr, CallbackPid),
    Paths = maps:get(paths, State),
    {noreply, State#{paths => [Path|Paths]}};
handle_info(What, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, What]),
    {noreply, State}.

handle_cast({send, Data}, State) ->
    Backend = maps:get(backend, State),
    Sock = maps:get(socket, State),
    send(Backend, Sock, Data),
    {noreply, State};
handle_cast(_What, State) ->
    {noreply, State}.

handle_call(connect, _From, State) ->
    {reply, ok, State, {continue, connect}};
handle_call(get_paths, _From, State) ->
    PathPids = maps:get(paths, State),
    RPort = maps:get(remote_port, State),
    PAddrs = [esock_path:get_path(P) || P <- PathPids],
    Paths = {self(), PAddrs, RPort},
    {reply, {ok, Paths}, State};
handle_call(_What, _From, State) ->
    {reply, undefined, State}.

terminate(_What, _State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

connect_async(Backend, Sock, Addrs, Port, _Opts) ->
    Parent = self(),
    {ok, Domain} = esock_utils:get_domain(Addrs, _Opts),
    SockAddrs = [esock_utils:socket_address(Domain, A, Port) || A <- Addrs],
    spawn_link(fun () -> client_loop(Backend, Sock, SockAddrs, Parent) end).

client_loop(socket = Backend, Sock, [SockAddr|SockAddrs], Parent) ->
    case socket:connect(Sock, SockAddr, 500) of
        ok ->
            Parent ! {comm_up, Sock, SockAddr};
        {error, _} = Error ->
            Parent ! Error,
            client_loop(Backend, Sock, SockAddrs ++ [SockAddr], Parent)
    end;
client_loop(gen_sctp = Backend, Sock, [SockAddr|SockAddrs], Parent) ->
    %% connectx_init does not work that well?
    case gen_sctp:connect(Sock, SockAddr, [{sctp_autoclose, 500}]) of
        {ok, Ass} ->
            Parent ! {comm_up, Ass, SockAddr};
        {error, _} = Error ->
            Parent ! Error,
            client_loop(Backend, Sock, SockAddrs ++ [SockAddr], Parent)
    end.

client_recv(socket = Backend, Sock, CallbackPid) ->
    case socket:recvmsg(Sock) of
        {ok, Msg} ->
            Addr = maps:get(addr, Msg, undefined),
            PeerIP = maps:get(addr, Addr, undefined),
            PeerPort = maps:get(port, Addr, undefined),
            %% TODO: Deal with Msg flags?
            #{iov := [IOVec],
              ctrl := AncData,
              flags := _Flags} = Msg,
            CallbackPid ! {data, PeerIP, PeerPort, IOVec, AncData},
            client_recv(Backend, Sock, CallbackPid);
        {error, _} = Err ->
            CallbackPid ! Err
    end;
client_recv(gen_sctp = Backend, Sock, CallbackPid) ->
    case gen_sctp:recv(Sock, infinity) of
        {ok, {PeerIP, PeerPort, AncData, Msg}} ->
            CallbackPid ! {data, PeerIP, PeerPort, Msg, AncData};
        {error, _} = Err ->
            CallbackPid ! Err
    end,
    client_recv(Backend, Sock, CallbackPid).

send(socket = _Backend, Sock, Data) ->
    ok = socket:send(Sock, Data);
send(gen_sctp = _Backend, _Sock, _Data) ->
    %% TODO
    %% ok = gen_sctp:send(Sock, #sndrcvinfo{}, Data).
    ok.
