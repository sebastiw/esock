-module(sock).

%% API
-export([start_server/1,
         start_client/1,
         start_link/1
        ]).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_server(Opts) ->
    sock_sup:start_child(Opts#{mode => server}).

start_client(Opts) ->
    sock_sup:start_child(Opts#{mode => client}).

start_link(#{mode := server} = Opts) ->
    sock_listener:start_link(Opts);
start_link(#{mode := client} = Opts) ->
    sock_connecter:start_link(Opts).

