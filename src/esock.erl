-module(esock).

-moduledoc """
API for the sock application.
""".

%% API
-export([create_ep/0,
         create_ep/1,
         create_ep/2,
         create_ep/3,
         register_owner/1,
         register_owner/2,
         register_owner/3,
         create_assoc/4,
         send/2,
         get_eps/0,
         get_ep/2,
         get_assocs/1,
         get_paths/1,
         find_assoc/4
        ]).

-export_type([port_no/0,
              address/0,
              backend/0,
              assoc/0,
              path/0,
              ep/0,
              accept_callback/0,
              protocol/0,
              local_opt/0,
              assoc_opt/0
             ]).

-type port_no() :: inet:port_number().
-type address() :: inet:ip_address().

-opaque assoc() :: pid().
-opaque path() :: {assoc(), address()}.
-opaque ep() :: pid().

-type accept_callback() ::
        fun((address(), port_no(), CurrentAssocs :: non_neg_integer()) -> boolean()) |
        fun((address(), port_no()) -> boolean()).

-type backend() :: gen_sctp | socket.
-type protocol() :: sctp | tcp.
-type local_opt() ::
        {backend, backend()} |
        {protocol, protocol()} |
        gen_sctp:option() |
        gen_tcp:option().

-type assoc_opt() ::
        local_opt().

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-doc """
Create an endpoint (EP) that can be used to listen to incoming
connections or to create associations to remote addresses.

Without call to register_owner the EP will not accept any incoming connections.
""".
-spec create_ep() ->
          {ok, ep()} | {error, inet:posix()}.
-spec create_ep(LocalOpts :: [local_opt()]) ->
          {ok, ep()} | {error, inet:posix()}.
-spec create_ep(LocalPort :: port_no(), LocalOpts :: [local_opt()]) ->
          {ok, ep()} | {error, inet:posix()}.
-spec create_ep(LocalAddrs :: [address()], LocalPort :: port_no(), [local_opt()]) ->
          {ok, ep()} | {error, inet:posix()}.

create_ep() ->
    create_ep([]).

create_ep(LocalOpts) ->
    create_ep(0, LocalOpts).

create_ep(LocalPort, LocalOpts) ->
    create_ep([loopback], LocalPort, LocalOpts).

create_ep(LocalAddrs, LocalPort, LocalOpts) ->
    esock_sup:start_child(LocalAddrs, LocalPort, LocalOpts).

-doc """
Register a process as owner of the endpoint (EP). The owner process
will receive messages about incoming connections and data received on
associations created by the EP.
""".
-spec register_owner(ep()) ->
          ok | {error, not_found}.
-spec register_owner(ep(), pid()) ->
          ok | {error, not_found}.
-spec register_owner(ep(), pid(), accept_callback()) ->
          ok | {error, not_found}.

register_owner(Ep) ->
    register_owner(Ep, self()).

register_owner(Ep, OwnerPid) ->
    register_owner(Ep, OwnerPid, fun(_, _, _) -> true end).

register_owner(Ep, OwnerPid, AcceptCallback) ->
    esock_ep:register_owner(Ep, OwnerPid, AcceptCallback).

-doc """
Create a connection to a remote peer.
""".
-spec create_assoc(ep(), RemoteAddrs :: [address()], RemotePort :: port_no(), [assoc_opt()]) ->
          {ok, assoc()} |
          {error, inet:posix() | not_found}.

create_assoc(Ep, RemoteAddrs, RemotePort, RemoteOpts) ->
    create_assoc(Ep, RemoteAddrs, RemotePort, RemoteOpts, self()).

create_assoc(Ep, RemoteAddrs, RemotePort, RemoteOpts, CallbackPid) ->
    esock_ep:create_assoc(Ep, RemoteAddrs, RemotePort, RemoteOpts, CallbackPid).

-spec send(assoc(), binary()) -> ok.
send(Assoc, Data) ->
    esock_assoc:send(Assoc, Data).

%%% Experimental API

-doc """
Retrieve all endpoints.
""".
-spec get_eps() ->
          [ep()].
get_eps() ->
    esock_sup:get_eps().

-doc """
Retrieve a specific endpoint bound to some address.
""".
-spec get_ep(LocalAddr :: address(), LocalPort :: port_no()) ->
          {ok, ep()} |
          {error, not_found}.
get_ep(LocalAddr, LocalPort) ->
    esock_sup:get_ep(LocalAddr, LocalPort).

-doc """
Retrive all associatons for a specific endpoint.
""".
-spec get_assocs(ep()) ->
          {ok, [assoc()]} |
          {error, not_found}.
get_assocs(Ep) ->
    esock_ep:get_assocs(Ep).

-doc """
Retrieve all paths of a specific association.
""".
-spec get_paths(assoc()) ->
          {ok, [path()]} |
          {error, not_found}.
get_paths(Assoc) ->
    esock_assoc:get_paths(Assoc).

-doc """
Find an association based on local and remote addresses.
""".
-spec find_assoc(LocalAddr :: address(), LocalPort :: port_no(), RemoteAddr :: address(), RemotePort :: port_no()) ->
          {ok, assoc()} |
          {error, not_found}.
find_assoc(LocalAddr, LocalPort, RemoteAddr, RemotePort) ->
    esock_sup:find_assoc(LocalAddr, LocalPort, RemoteAddr, RemotePort).
