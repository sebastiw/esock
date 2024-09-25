-module(sock_connecter).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1,
         stop/1,
         connect/2
        ]).

%% Gen Server callbacks
-export([init/1,
         handle_continue/2,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2
        ]).

-type subnet() :: nonempty_binary(). %% e.g. <<"0.0.0.0/0">> or <<"192.168.0.25/32">>

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-spec start_link(term()) -> {ok, pid()}.
start_link(Opts) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Opts, []),
    RAddr = maps:get(remote_addr, Opts),
    connect(Pid, RAddr),
    {ok, Pid}.

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec connect(pid(), list({subnet(), inet:port_number()})) -> ok;
             (pid(), list(subnet())) -> ok.
connect(Pid, MaskPortPairs) ->
    gen_server:call(Pid, {connect, MaskPortPairs}).

%% ---------------------------------------------------------------------------
%% Gen Server Callbacks
%% ---------------------------------------------------------------------------

init(Opts) ->
    process_flag(trap_exit, true),
    NewState = sock_utils:new_socket(Opts),
    {ok, NewState}.

handle_continue(connect, State) ->
    {noreply, State}.

handle_info({'EXIT', ConnecterP, normal}, #{pid := ConnecterP} = State) ->
    %% NewState = sock_utils:new_socket(State),
    %% {noreply, NewState, {continue, connect}};
    {noreply, State};
handle_info({'DOWN', MonRef, socket, Socket, closed}, #{monitor := MonRef, socket := Socket} = State) ->
    NewState = sock_utils:new_socket(State),
    {noreply, NewState, {continue, connect}};
handle_info({connected, Sock}, State) ->
    case socket:peername(Sock) of
        {ok, PeerName} ->
            case check_waiting(State, PeerName) of
                [] ->
                    ?LOG_INFO("~s:~B Invalid connection attempt by ~p~n", [?MODULE, ?LINE, PeerName]),
                    socket:close(Sock);
                [{_, _, Ref, Pid}|Rest] ->
                    %% ok = socket:setopt(Sock, {otp, controlling_process}, Pid),
                    Pid ! {connected, Ref, Sock},
                    notify_rest(Rest, PeerName)
            end;
        {error, Err} ->
            ?LOG_INFO("~s:~B Connected socket ~p error ~p~n", [?MODULE, ?LINE, Sock, Err])
    end,
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call({connect, RAddr}, {From, _}, State) ->
    Ref = make_ref(),
    CSock = maps:get(socket, State),
    _ConnecterP = sock_busy_connecter:give_control(CSock, RAddr),
    NewState = add_waiting(State, From, Ref, RAddr),
    {reply, {ok, Ref}, NewState, {continue, connect}};
handle_call(close, {From, _}, State) ->
    NewState = rem_waiting(State, From),
    {reply, ok, NewState};
handle_call(_What, _From, State) ->
    {reply, undefined, State}.

terminate(_How, State) ->
    Socket = maps:get(socket, State),
    socket:shutdown(Socket, read_write),
    socket:close(Socket),
    State.

%% ---------------------------------------------------------------------------
%% Help functions
%% ---------------------------------------------------------------------------

add_waiting(State, Pid, Ref, RAddr) ->
    Ws = maps:get(waiting, State, []),
    Mask = inet:ntoa(maps:get(addr, RAddr)),
    Port = maps:get(port, RAddr),
    MaskPortPairs = [{list_to_binary(Mask ++ "/32"), Port, Ref, Pid}],
    State#{waiting => Ws ++ MaskPortPairs}.

rem_waiting(State, Pid) ->
    Ws = maps:get(waiting, State, []),
    NewWs = lists:filter(fun ({_, _, _, P}) -> P =/= Pid end, Ws),
    State#{waiting => NewWs}.

check_waiting(State, Addr) ->
    MaskPidPairs = maps:get(waiting, State, []),
    sock_utils:get_matching_pids(MaskPidPairs, Addr).

notify_rest([], _) ->
    ok;
notify_rest([{_, _, Ref, R}|Rest], PeerName) ->
    R ! {taken, Ref, PeerName},
    notify_rest(Rest, PeerName).
