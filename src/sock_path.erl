-module(sock_path).
-behaviour(gen_server).

%% API
-export([create_path/1,
         start_link/1,
         get_path/1
        ]).

%% Callbacks
-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2
        ]).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

create_path(SockAddr) ->
    Parent = self(),
    start_link(SockAddr#{parent => Parent}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

get_path(Pid) ->
    gen_server:call(Pid, get_path).

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init([State]) ->
    {ok, State}.

handle_info(What, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, What]),
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call(get_path, _From, State) ->
    Addr = maps:get(addr, State, undefined),
    {reply, Addr, State};
handle_call(_What, _From, State) ->
    {reply, undefined, State}.

terminate(_Reason, _State) ->
    ok.
