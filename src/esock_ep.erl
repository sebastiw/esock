-module(esock_ep).
-behaviour(gen_server).

%% API
-export([start_link/4,
         create_assoc/5,
         get_assocs/1,
         find_assoc/3
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

start_link(LocalAddrs, LocalPort, LocalOpts, CallbackPid) ->
    Protocol = proplists:get_value(protocol, LocalOpts, sctp),
    Name = {Protocol, LocalAddrs, LocalPort},
    gen_server:start_link({via, esock_reg, Name}, ?MODULE, [LocalAddrs, LocalPort, LocalOpts, CallbackPid], []).

create_assoc(Ep, RemoteAddr, RemotePort, AssocOpts, CallbackPid) ->
    gen_server:call(Ep, {create_assoc, RemoteAddr, RemotePort, AssocOpts, CallbackPid}).

get_assocs(Ep) ->
    %% TBD
    gen_server:call(Ep, get_assocs).

find_assoc(Ep, RemoteAddr, RemotePort) ->
    %% TBD
    gen_server:call(Ep, {find_assoc, RemoteAddr, RemotePort}).

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init([LocalAddrs, LocalPort, LocalOpts, CallbackPid]) ->
    Protocol = proplists:get_value(protocol, LocalOpts, sctp),
    {ok, Sock} = open_and_bind(LocalAddrs, LocalPort, LocalOpts, Protocol),
    State = #{socket => Sock,
              options => LocalOpts,
              assocs => [],
              callback_pid => CallbackPid
             },
    {ok, State, {continue, maybe_listen}}.

handle_continue(maybe_listen, State) ->
    Options = maps:get(options, State, []),
    %% Accept callback, wether to accept an incoming connection
    %% fun(RemoteIP, RemotePort, AncData, CurrentCount)
    AC = case proplists:get_value(accept, Options) of
             N when is_integer(N) ->
                 fun(_I, _P, _A, C) -> C < N end;
             F when is_function(F, 2) ->
                 fun(I, P, _A, _C) -> F(I, P) end;
             F when is_function(F, 3) ->
                 fun(I, P, _A, C) -> F(I, P, C) end;
             F when is_function(F, 4) ->
                 F;
             undefined ->
                 undefined
         end,
    case AC of
        undefined ->
            {noreply, State};
        _ ->
            Sock = maps:get(socket, State),
            case listen(Sock) of
                ok ->
                    CallbackPid = maps:get(callback_pid, State),
                    spawn_link(fun () -> server_recv(Sock, CallbackPid, AC, 0) end),
                    {noreply, State#{options => Options ++ [{accept, AC}]}}
            end
    end.

handle_info({recv, PeerIP, PeerPort, _Msg, AncData}, State) ->
    io:format("~p:recv:~p ~p~n", [?MODULE, ?LINE, {PeerIP, PeerPort, AncData}]),
    {noreply, State};
handle_info(What, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, What]),
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call({create_assoc, RemoteAddrs, RemotePort, AssocOpts, CallbackPid}, _From, State) ->
    Sock = maps:get(socket, State),
    LocalOpts = maps:get(options, State, []),
    Opts = LocalOpts ++ AssocOpts,
    {ok, Pid} = esock_assoc:start_link(Sock, RemoteAddrs, RemotePort, Opts, CallbackPid),
    Assocs = maps:get(assocs, State),
    {reply, {ok, Pid}, State#{assocs => [Pid|Assocs]}};
handle_call(get_assocs, _From, State) ->
    Assocs = maps:get(assocs, State),
    {reply, {ok, Assocs}, State};
handle_call(_What, _From, State) ->
    {reply, undefined, State}.

terminate(_What, _State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

-spec open_and_bind([inet:ip_address()], inet:port_number(), map(), socket:protocol()) ->
          {ok, socket:socket() | gen_sctp:sctp_socket()} | {error, atom()}.
-ifdef(USE_SOCKET).
open_and_bind(LocalAddrs, LocalPort, LocalOpts, Protocol) ->
    {ok, Domain} = esock_utils:get_domain(LocalAddrs, LocalOpts),
    {ok, Sock} = socket:open(Domain, seqpacket, Protocol),
    %% OTP 27 socket-api does not seem to support multiple bound local addresses
    [LocalAddr|_] = LocalAddrs,
    Addr = esock_utils:socket_address(Domain, LocalAddr, LocalPort),
    case socket:bind(Sock, Addr) of
        ok ->
            {ok, Sock};
        {error, Reason} ->
            {error, Reason}
    end.
-else.
open_and_bind(LocalAddrs, LocalPort, LocalOpts, sctp) ->
    {ok, Domain} = esock_utils:get_domain(LocalAddrs, LocalOpts),
    Addrs = [{ifaddr, esock_utils:socket_address(Domain, L, LocalPort)} || L <- LocalAddrs],
    Opts = [{type, seqpacket},
            %% {port, LocalPort} %% Should not be needed with sockaddr?
            Domain
           | Addrs
           ],
    case gen_sctp:open(Opts) of
        {ok, Sock} ->
            {ok, Sock};
        {error, Reason} ->
            {error, Reason}
    end.
-endif.

-spec listen(socket:socket() | gen_sctp:sctp_socket()) -> ok | {error, term()}.
-ifdef(USE_SOCKET).
listen(Sock) ->
    socket:listen(Sock).
-else.
listen(Sock) ->
    gen_sctp:listen(Sock, true).
-endif.

-spec server_recv(socket:socket() | gen_sctp:sctp_socket(), pid(), accept_callback(), integer()) -> no_return().
-ifdef(USE_SOCKET).
server_recv(Sock, CallbackPid, AC, NumPeers) ->
    case socket:recvmsg(Sock) of
        {ok, Msg} ->
            Addr = maps:get(addr, Msg, undefined),
            PeerIP = maps:get(addr, Addr, undefined),
            PeerPort = maps:get(port, Addr, undefined),
            %% TODO: Deal with Msg flags?
            #{iov := [IOVec],
              ctrl := AncData,
              flags := _Flags} = Msg,
            case AC(PeerIP, PeerPort, AncData, NumPeers) of
                true ->
                    %% TODO: peeloff and handle separately
                    CallbackPid ! {recv, PeerIP, PeerPort, IOVec, AncData},
                    server_recv(Sock, CallbackPid, AC, NumPeers + 1);
                false ->
                    %% TODO: peeloff and close, for now just ignore
                    io:format("~p:server_recv:~p Rejecting connection from ~p:~p~n", [?MODULE, ?LINE, PeerIP, PeerPort]),
                    server_recv(Sock, CallbackPid, AC, NumPeers)
            end;
        {error, _} = Err ->
            CallbackPid ! {recv, Err}
    end.
-else.
server_recv(Sock, CallbackPid, AC, I) ->
    case gen_sctp:recv(Sock, infinity) of
        {ok, Msg} ->
            CallbackPid ! {recv, Msg};
        {error, _} = Err ->
            CallbackPid ! {recv, Err}
    end,
    server_recv(Sock, CallbackPid, AC, I).
-endif.
