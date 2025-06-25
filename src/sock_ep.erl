-module(sock_ep).
-behaviour(gen_server).

%% API
-export([start_link/3,
         create_assoc/4,
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

-include("sock.hrl").

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_link(LocalAddrs, LocalPort, LocalOpts) ->
    Protocol = proplists:get_value(protocol, LocalOpts, sctp),
    Name = {Protocol, LocalAddrs, LocalPort},
    gen_server:start_link({via, sock_reg, Name}, ?MODULE, [LocalAddrs, LocalPort, LocalOpts], []).

create_assoc(Ep, RemoteAddr, RemotePort, AssocOpts) ->
    gen_server:call(Ep, {create_assoc, RemoteAddr, RemotePort, AssocOpts}).

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
    Protocol = proplists:get_value(protocol, LocalOpts, sctp),
    {ok, Sock} = open_and_bind(LocalAddrs, LocalPort, LocalOpts, Protocol),
    State = #{socket => Sock,
              options => LocalOpts,
              assocs => []
             },
    {ok, State, {continue, maybe_listen}}.

handle_continue(maybe_listen, State) ->
    Options = maps:get(options, State, []),
    AC = case proplists:get_value(accept, Options) of
             N when is_integer(N) ->
                 fun(_A, _P, C) -> C < N end;
             F when is_function(F, 2) ->
                 fun(A, P, _C) -> F(A, P) end;
             F when is_function(F, 3) ->
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
                    Parent = self(),
                    spawn_link(fun () -> server_recv(Sock, Parent) end),
                    {noreply, State#{options => Options ++ [{accept, AC}]}}
            end
    end.

handle_info({recv, {PeerIP, PeerPort, [], AncData}}, State) ->
    io:format("~p:recv:~p ~p~n", [?MODULE, ?LINE, {PeerIP, PeerPort, AncData}]),
    {noreply, State};
handle_info(What, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, What]),
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call({create_assoc, RemoteAddrs, RemotePort, AssocOpts}, _From, State) ->
    Sock = maps:get(socket, State),
    LocalOpts = maps:get(options, State, []),
    Opts = LocalOpts ++ AssocOpts,
    {ok, Pid} = sock_assoc:start_link(Sock, RemoteAddrs, RemotePort, Opts),
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
    {ok, Domain} = sock_utils:get_domain(LocalAddrs, LocalOpts),
    {ok, Sock} = socket:open(Domain, seqpacket, Protocol),
    %% OTP 27 socket-api does not seem to support multiple bound local addresses
    [LocalAddr|_] = LocalAddrs,
    Addr = sock_utils:socket_address(Domain, LocalAddr, LocalPort),
    case socket:bind(Sock, Addr) of
        ok ->
            {ok, Sock};
        {error, Reason} ->
            {error, Reason}
    end.
-else.
open_and_bind(LocalAddrs, LocalPort, LocalOpts, sctp) ->
    {ok, Domain} = sock_utils:get_domain(LocalAddrs, LocalOpts),
    Addrs = [{ifaddr, sock_utils:socket_address(Domain, L, LocalPort)} || L <- LocalAddrs],
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

-spec server_recv(socket:socket() | gen_sctp:sctp_socket(), pid()) -> no_return().
-ifdef(USE_SOCKET).
server_recv(_Sock, _Parent) ->
    ok.
-else.
server_recv(Sock, Parent) ->
    case gen_sctp:recv(Sock, infinity) of
        {ok, Msg} ->
            Parent ! {recv, Msg};
        {error, _} = Err ->
            Parent ! {recv, Err}
    end,
    server_recv(Sock, Parent).
-endif.
