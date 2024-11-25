-module(sock_path).

%% API
-export([start_link/3
        ]).

%% Callbacks
-export([init/1,
         terminate/2
        ]).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_link(LocalAddrs, LocalPort, LocalOpts) ->
    Parent = self(),
    spawn_link(?MODULE, init, [LocalAddrs, LocalPort, LocalOpts]).

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init(_) ->
    ok.

terminate(_Reason, _State) ->
    ok.
