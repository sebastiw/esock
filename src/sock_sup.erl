-module(sock_sup).
-behaviour(supervisor).

-moduledoc """
Application wide supervisor of endpoint processes.
""".

%% API
-export([start_link/0,
         start_child/3,
         get_eps/0,
         get_ep/2,
         find_assoc/4
        ]).

%% Callbacks
-export([init/1
        ]).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(LocalAddrs, LocalPort, LocalOpts) ->
    ChildOpts = [LocalAddrs, LocalPort, LocalOpts],
    supervisor:start_child(?MODULE, ep_spec(ChildOpts)).

get_eps() ->
    [Pid || {_, Pid, _, _} <- supervisor:which_children(?MODULE)].

get_ep(LocalAddr, LocalPort) ->
    Cs = [Pid
          || {{LAs, LP}, Pid, _, _} <- supervisor:which_children(?MODULE),
             LocalPort =:= LP,
             lists:member(LocalAddr, LAs)],
    case Cs of
        [] -> {error, not_found};
        [C] -> {ok, C}
    end.

find_assoc(_LocalAddr, _LocalPort, _RemoteAddr, _RemotePort) ->
    %% TBD
   {error, not_found}.

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init(_) ->
    sock_reg:start_link(),
    Flags = #{strategy => one_for_one,
              intensity => 5,
              period => 10,
              auto_shutdown => never},
    {ok, {Flags, []}}.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

ep_spec([LocalAddrs, LocalPort, LocalOpts] = Args) ->
    Proto = maps:get(protocol, LocalOpts, sctp),
    #{id => {Proto, LocalAddrs, LocalPort},
      start => {sock_ep, start_link, Args},
      restart => transient,
      significant => false,
      shutdown => 5,
      type => worker,
      modules => [sock_ep]}.
