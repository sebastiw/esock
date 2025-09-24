-module(esock_ep_listener).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/3,
         register_owner/3
        ]).

%% Callbacks
-export([init/1,
         handle_continue/2,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2
        ]).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_link(Backend, Sock, Parent) ->
    gen_server:start_link(?MODULE, [Backend, Sock, Parent], []).

register_owner(Pid, OwnerPid, AcceptCallback) when is_function(AcceptCallback, 3) ->
    gen_server:cast(Pid, {register_owner, OwnerPid, AcceptCallback}).

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init([Backend, Sock, Parent]) ->
    {ok, #{backend => Backend,
           parent => Parent,
           socket => Sock,
           callbacks => []
          }}.

handle_continue(recv, State) ->
    Backend = maps:get(backend, State),
    Parent = maps:get(parent, State),
    Sock = maps:get(socket, State),
    CBs = maps:get(callbacks, State),
    recv_msg(Backend, Parent, Sock, CBs, []),
    {noreply, State}.

handle_info({'$socket', Socket, select, _Ref}, #{socket := Socket} = State) ->
    {noreply, State, {continue, recv}};
handle_info(What, State) ->
    io:format("~p:~p:~p ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, What]),
    {noreply, State}.

handle_cast({register_owner, OwnerPid, AcceptCallback}, State) ->
    CBs = maps:get(callbacks, State),
    {noreply, State#{callbacks => [{OwnerPid, AcceptCallback}|CBs]}, {continue, recv}};
handle_cast(_What, State) ->
    {noreply, State}.

handle_call(_What, _From, State) ->
    {reply, undefined, State}.

terminate(_What, _State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

recv_msg(gen_sctp = Backend, Parent, Sock, AcceptCallbacks, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, 100),
    case gen_sctp:recv(Sock, Timeout) of
        {ok, Msg} ->
            Res = check_accept_callbacks(AcceptCallbacks, Msg),
            peeloff(Backend, Sock, Res),
            recv_msg(Backend, Parent, Sock, AcceptCallbacks, Opts);
        {error, Reason} ->
            io:format("~p:~p:~p recv error: ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, Reason]),
            Parent ! {error, self(), Reason}
    end;
recv_msg(socket = Backend, Parent, Sock, AcceptCallbacks, Opts) ->
    case socket:recvmsg(Sock, [], nowait) of
        {ok, Msg} ->
            #{iov := [IOVec],
              ctrl := AncData,
              flags := _Flags} = Msg,
            Addr = maps:get(addr, Msg, #{}),
            PeerIP = maps:get(addr, Addr, undefined),
            PeerPort = maps:get(port, Addr, undefined),
            Res = check_accept_callbacks(AcceptCallbacks, {PeerIP, PeerPort, AncData, IOVec}),
            maybe_notify_parent(Parent, PeerIP, PeerPort, Opts, Res),
            peeloff(Backend, Sock, Res),
            recv_msg(Backend, Parent, Sock, AcceptCallbacks, Opts);
        {select, SelectInfo} ->
            ?LOG_INFO(SelectInfo),
            ok;
        {completion, CompletionInfo} ->
            ?LOG_INFO(CompletionInfo),
            ok;
        {error, Reason} ->
            Parent ! {error, self(), Reason}
    end.

check_accept_callbacks([], _Msg) ->
    drop;
check_accept_callbacks([{Listener, CB}|T], {PeerIP, PeerPort, AncData, Msg}) ->
    case CB(PeerIP, PeerPort, AncData) of
        true ->
            Listener ! {data, PeerIP, PeerPort, Msg, AncData},
            {sent, Listener};
        false ->
            check_accept_callbacks(T, {PeerIP, PeerPort, AncData, Msg})
    end.

maybe_notify_parent(Parent, RemoteAddrs, RemotePort, Opts, {sent, Listener}) ->
    Parent ! {assoc, RemoteAddrs, RemotePort, Opts, Listener};
maybe_notify_parent(_, _, _, _, drop) ->
    ok.

peeloff(_Backend, _Sock, drop) ->
    %% TODO: peeloff and close
    ok;
peeloff(_Backend, _Sock, _) ->
    %% TODO: peeloff
    ok.
