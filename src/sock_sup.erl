-module(sock_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1,
         which_children/0
        ]).

%% Supervisor
-export([init/1
        ]).

-export_type([options/0
             ]).

-type options() :: #{mode := mode(),
                     local_addr := socket:sockaddr(),
                     local_info := socket_info(),
                     remote_addr := socket:sockaddr()
                    }.

-type mode() :: server | client.
-type socket_info() :: #{%% subset of socket:socket_info()
                         domain := inet | inet6,
                         protocol := sctp | tcp,
                         type := stream
                        }.

-define(DEFAULT_ADDR, #{family => inet, addr => {0,0,0,0}, port => 0}).
-define(DEFAULT_INFO, #{domain => inet, protocol => sctp}).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(options()) -> {ok, pid()}.
start_child(Opts0) ->
    Opts = normalize_config(Opts0),
    Addr = maps:get(local_addr, Opts),
    Info = maps:get(local_info, Opts),
    case filter_sockets(Addr, Info) of
        [P|_] ->
            {ok, P};
        [] ->
            supervisor:start_child(?MODULE, child_spec(Opts))
    end.

-spec which_children() -> list(supervisor:child_spec()).
which_children() ->
    supervisor:which_children(?MODULE).

%% ---------------------------------------------------------------------------
%% Supervisor
%% ---------------------------------------------------------------------------

init(_) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs = [
               ],
  {ok, {SupFlags, ChildSpecs}}.

%% ---------------------------------------------------------------------------
%% Helper functions
%% ---------------------------------------------------------------------------

child_spec(Opts) ->
    Addr = maps:get(local_addr, Opts),
    #{id => Addr,
      start => {sock, start_link, [Opts]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [sock_sup]}.

normalize_config(Opts0) ->
    LAddr0 = maps:get(local_addr, Opts0, #{}),
    LInfo0 = maps:get(local_info, Opts0, #{}),
    Family = find_family(Opts0),
    LAddr = case Family of
                inet ->
                    normalize_ipv4(LAddr0);
                inet6 ->
                    normalize_ipv6(LAddr0)
            end,

    LInfo = normalize_info(LInfo0, LAddr),
    RAddr = case Opts0 of
                #{mode := client, remote_addr := RAddr0} ->
                    RAddr0;
                #{mode := server} ->
                    maps:get(remote_addr, Opts0, #{});
                _ ->
                    throw(#{error => "No remote address found",
                            solution => "add remote_addr to options()"})
            end,
    Opts0#{local_addr => LAddr,
           local_info => LInfo,
           remote_addr => RAddr
          }.

find_family(#{domain := D}) ->
    D;
find_family(#{local_addr := #{family := F}}) ->
    F;
find_family(#{remote_addr := #{family := F}}) ->
    F;
find_family(_) ->
    inet.

-spec normalize_ipv4(map()) -> socket:sockaddr_in().
normalize_ipv4(Addr0) ->
    #{family => inet,
      port => maps:get(port, Addr0, 0),
      addr => maps:get(addr, Addr0, loopback)
     }.

-spec normalize_ipv6(map()) -> socket:sockaddr_in6().
normalize_ipv6(Addr0) ->
    #{family => inet6,
      port => maps:get(port, Addr0, 0),
      addr => maps:get(addr, Addr0, loopback),
      flowinfo => maps:get(flowinfo, Addr0, 0),
      scope_id => maps:get(scope_id, Addr0, 0)
     }.

-spec normalize_info(map(), socket:sockaddr_in() | socket:sockaddr_in6()) -> socket_info().
normalize_info(Info0, Addr) ->
    Domain = maps:get(domain, Info0, maps:get(family, Addr)),
    Protocol = maps:get(protocol, Info0, sctp),
    Type = stream = maps:get(type, Info0, stream),
    #{domain => Domain,
      protocol => Protocol,
      type => Type
     }.

filter_sockets(Addr, Info) ->
    lists:filter(fun ({S, {ok, Addr0}, Info0}) ->
                         is_subset_of(Addr, Addr0) andalso is_subset_of(Info, Info0)
                             andalso {true, {S, Addr0, Info0}}
                 end, get_sockets()).

-spec get_sockets() -> list({socket:socket(), socket:sockaddr_recv(), socket:socket_info()}).
get_sockets() ->
    Sockets = socket:which_sockets(),
    [{S, socket:sockname(S), socket:info(S)} || S <- Sockets].

is_subset_of(A, B) ->
    A =:= maps:with(maps:keys(A), B).
