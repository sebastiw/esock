-module(esock_ep).
-behaviour(gen_server).

%% API
-export([start_link/3,
         register_owner/3,
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

start_link(LocalAddrs, LocalPort, LocalOpts) ->
    Protocol = proplists:get_value(protocol, LocalOpts, sctp),
    Name = {Protocol, LocalAddrs, LocalPort},
    gen_server:start_link({via, esock_reg, Name}, ?MODULE, [LocalAddrs, LocalPort, LocalOpts], []).

register_owner(Ep, OwnerPid, AcceptCallback) ->
    gen_server:call(Ep, {register_owner, OwnerPid, AcceptCallback}).

create_assoc(Ep, RemoteAddr, RemotePort, RemoteOpts, CallbackPid) ->
    gen_server:call(Ep, {create_assoc, RemoteAddr, RemotePort, RemoteOpts, CallbackPid}).

get_assocs(Ep) ->
    %% TBD
    gen_server:call(Ep, get_assocs).

find_assoc(Ep, RemoteAddr, RemotePort) ->
    %% TBD
    gen_server:call(Ep, {find_assoc, RemoteAddr, RemotePort}).

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init([LocalAddrs, LocalPort, LocalOpts]) ->
    Backend = proplists:get_value(backend, LocalOpts, socket),
    Protocol = proplists:get_value(protocol, LocalOpts, sctp),
    {ok, Sock} = open_and_bind(Backend, LocalAddrs, LocalPort, LocalOpts, Protocol),
    State = #{backend => Backend,
              socket => Sock,
              options => LocalOpts,
              assocs => []
             },
    {ok, State, {continue, maybe_listen}}.

handle_continue(maybe_listen, State) ->
    Backend = maps:get(backend, State),
    Sock = maps:get(socket, State),
    ok = listen(Backend, Sock),
    {ok, Pid} = esock_ep_listener:start_link(Backend, Sock, self()),
    {noreply, State#{child => Pid}}.

handle_info({assoc, RemoteAddrs, RemotePort, RemoteOpts, CallbackPid}, State) ->
    Sock = maps:get(socket, State),
    LocalOpts = maps:get(options, State, []),
    Opts = LocalOpts ++ RemoteOpts,
    Backend = maps:get(backend, State),
    {ok, Pid} = esock_assoc:start_link(Backend, Sock, RemoteAddrs, RemotePort, Opts, CallbackPid),
    Assocs = maps:get(assocs, State),
    {noreply, State#{assocs => [Pid|Assocs]}};
handle_info({recv, PeerIP, PeerPort, _Msg, AncData}, State) ->
    io:format("~p:recv:~p ~p~n", [?MODULE, ?LINE, {PeerIP, PeerPort, AncData}]),
    {noreply, State};
handle_info(What, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, What]),
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call({register_owner, OwnerPid, AcceptCallback}, _From, State) ->
    Child = maps:get(child, State),
    ok = esock_ep_listener:register_owner(Child, OwnerPid, AcceptCallback),
    {reply, ok, State};
handle_call({create_assoc, RemoteAddrs, RemotePort, RemoteOpts, CallbackPid}, _From, State) ->
    Sock = maps:get(socket, State),
    LocalOpts = maps:get(options, State, []),
    Opts = LocalOpts ++ RemoteOpts,
    Backend = maps:get(backend, State),
    {ok, Pid} = esock_assoc:start_link(Backend, Sock, RemoteAddrs, RemotePort, Opts, CallbackPid),
    ok = esock_assoc:connect(Pid),
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

-spec open_and_bind(esock:backend(), [inet:ip_address()], inet:port_number(), map(), socket:protocol()) ->
          {ok, socket:socket() | gen_sctp:sctp_socket()} | {error, atom()}.
open_and_bind(socket, LocalAddrs, LocalPort, LocalOpts, Protocol) ->
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
    end;
open_and_bind(gen_sctp, LocalAddrs, LocalPort, LocalOpts, sctp) ->
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

-spec listen(esock:backend(), socket:socket() | gen_sctp:sctp_socket()) -> ok | {error, term()}.
listen(socket, Sock) ->
    socket:listen(Sock);
listen(gen_sctp, Sock) ->
    gen_sctp:listen(Sock, true).

