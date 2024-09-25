-module(sock_listener).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1,
         stop/0,
         accept/1
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
    gen_server:start_link(?MODULE, Opts, []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec accept(list({subnet(), inet:port_number()})) -> ok;
            (list(subnet())) -> ok.
accept(MaskPortPairs) ->
    gen_server:call(?MODULE, {accept, MaskPortPairs}).

%% ---------------------------------------------------------------------------
%% Gen Server Callbacks
%% ---------------------------------------------------------------------------

init(Opts) ->
    process_flag(trap_exit, true),
    NewState = sock_utils:new_socket(Opts),
    {ok, NewState, {continue, listen}}.

handle_continue(listen, State) ->
    LSock = maps:get(socket, State),
    ok = socket:listen(LSock),
    ListenerP = sock_accept_listener:give_control(LSock),
    {noreply, State#{pid => ListenerP}}.

handle_info({'DOWN', MonRef, socket, Socket, closed}, #{monitor := MonRef, socket := Socket} = State) ->
    NewState = sock_utils:new_socket(State),
    {noreply, NewState, {continue, listen}};
handle_info({accepted, Sock}, State) ->
    case socket:peername(Sock) of
        {ok, PeerName} ->
            case check_waiting(State, PeerName) of
                [] ->
                    ?LOG_INFO("~s:~B Invalid connection attempt by ~p~n", [?MODULE, ?LINE, PeerName]),
                    socket:close(Sock);
                [{_, _, Ref, Pid}|Rest] ->
                    ok = socket:setopt(Sock, {otp, controlling_process}, Pid),
                    Pid ! {accepted, Ref, Sock},
                    notify_rest(Rest, PeerName)
            end;
        {error, Err} ->
            ?LOG_INFO("~s:~B Accepted socket ~p error ~p~n", [?MODULE, ?LINE, Sock, Err])
    end,
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call({accept, MaskPortPairs}, {From, _}, State) ->
    Ref = make_ref(),
    NewState = add_waiting(State, From, Ref, MaskPortPairs),
    {reply, {ok, Ref}, NewState};
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

add_waiting(State, Pid, Ref, Masks) ->
    Ws = maps:get(waiting, State, []),
    MaskPortPairs = lists:map(fun ({Mask, Port}) -> {Mask, Port, Ref, Pid};
                                  (Mask) -> {Mask, 0, Ref, Pid}
                              end, Masks),
    State#{waiting => Ws ++ MaskPortPairs}.

rem_waiting(State, Pid) ->
    Ws = maps:get(waiting, State, []),
    NewWs = lists:filter(fun ({_, _, P}) -> P =/= Pid end, Ws),
    State#{waiting => NewWs}.

check_waiting(State, Addr) ->
    MaskPidPairs = maps:get(waiting, State, []),
    sock_utils:get_matching_pids(MaskPidPairs, Addr).

notify_rest([], _) ->
    ok;
notify_rest([{_, _, Ref, R}|Rest], PeerName) ->
    R ! {taken, Ref, PeerName},
    notify_rest(Rest, PeerName).
