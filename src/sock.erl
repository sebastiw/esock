-module(sock).

-moduledoc """
API for the sock application.
""".

%% API
-export([create_ep/0,
         create_ep/1,
         create_ep/2,
         create_ep/3,
         create_assoc/4,
         get_eps/0,
         get_ep/2,
         get_assocs/1,
         get_paths/1,
         find_assoc/4
        ]).

-export_type([port_no/0,
              address/0,
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

-type protocol() :: sctp | tcp.
-type local_opt() ::
        {accept, non_neg_integer() | accept_callback()} |
        {protocol, protocol()} |
        gen_sctp:option() |
        gen_tcp:option().

-type assoc_opt() ::
        local_opt().

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-spec create_ep() ->
          {ok, ep()} | {error, inet:posix()}.
-spec create_ep(LocalOpts :: [local_opt()]) ->
          {ok, ep()} | {error, inet:posix()}.
-spec create_ep(LocalPort :: port_no(), LocalOpts :: [local_opt()]) ->
          {ok, ep()} | {error, inet:posix()}.
-spec create_ep(LocalAddrs :: [address()], LocalPort :: port_no(), [local_opt()]) ->
          {ok, ep()} |
          {error, inet:posix()}.

create_ep() ->
    create_ep([]).

create_ep(LocalOpts) ->
    create_ep(0, LocalOpts).

create_ep(LocalPort, LocalOpts) ->
    create_ep([loopback], LocalPort, LocalOpts).

create_ep(LocalAddrs, LocalPort, LocalOpts) ->
    create_ep(LocalAddrs, LocalPort, LocalOpts, self()).

create_ep(LocalAddrs, LocalPort, LocalOpts, CallbackPid) ->
    sock_sup:start_child(LocalAddrs, LocalPort, LocalOpts, CallbackPid).

-spec create_assoc(ep(), RemoteAddrs :: [address()], RemotePort :: port_no(), [assoc_opt()]) ->
          {ok, assoc()} |
          {error, inet:posix() | not_found}.
create_assoc(Ep, RemoteAddrs, RemotePort, RemoteOpts) ->
    sock_ep:create_assoc(Ep, RemoteAddrs, RemotePort, RemoteOpts).

-spec get_eps() ->
          [ep()].
get_eps() ->
    sock_sup:get_eps().

-spec get_ep(LocalAddr :: address(), LocalPort :: port_no()) ->
          {ok, ep()} |
          {error, not_found}.
get_ep(LocalAddr, LocalPort) ->
    sock_sup:get_ep(LocalAddr, LocalPort).

-spec get_assocs(ep()) ->
          {ok, [assoc()]} |
          {error, not_found}.
get_assocs(Ep) ->
    sock_ep:get_assocs(Ep).

-spec get_paths(assoc()) ->
          {ok, [path()]} |
          {error, not_found}.
get_paths(Assoc) ->
    sock_assoc:get_paths(Assoc).

-spec find_assoc(LocalAddr :: address(), LocalPort :: port_no(), RemoteAddr :: address(), RemotePort :: port_no()) ->
          {ok, assoc()} |
          {error, not_found}.
find_assoc(LocalAddr, LocalPort, RemoteAddr, RemotePort) ->
    sock_sup:find_assoc(LocalAddr, LocalPort, RemoteAddr, RemotePort).
