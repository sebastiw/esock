-module(sock_local_handler).
-behaviour(gen_server).

-moduledoc """
Local socket handler.
""".

%% API
-export([start_link/1,
         add_connection/2
        ]).

%% GenServer callbacks
-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2
        ]).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

add_connection(Pid, Opts) ->
    gen_server:call(Pid, {add, Opts}).

%% ---------------------------------------------------------------------------
%% GenServer Callbacks
%% ---------------------------------------------------------------------------

init(Opts) ->
    {ok, Sock} = socket:open(inet, stream, sctp),
    BindLAddrs = [{L, socket:bind(Sock, L)} || L <- maps:get(local_addrs, Opts)],
    {_, BindRes} = lists:unzip(BindLAddrs),
    case lists:uniq(BindRes) of
        [ok] ->
            {ok, []};
        _E ->
            {error, not_bounded}
    end.

handle_info(_What, State) ->
    {noreply, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call({add, Opts}, _From, State) ->
    RAddrs = maps:get(remote_addrs, Opts, []),
    case length(RAddrs) of
        0 ->
            {reply, {error, no_remote_addrs}, State};
        _ ->
            Matches = [{RAddr, case lists:keyfind(RAddr, 1, State) of
                                   {_, P} -> P;
                                   false -> false
                               end} || RAddr <- RAddrs],
            case lists:all(fun ({_, T}) -> false =:= T end, Matches) of
                true ->
                    %% non is in state, proceed
                    %% - start new local-proc and add them addresses to state
                    {ok, Pid} = sock_remote_handler:start_link(Opts),
                    NewState = [{R, Pid} || R <- RAddrs] ++ State,
                    {reply, {ok, {new, Pid}}, NewState};
                false ->
                    {NonBound, Bound} = lists:partition(fun ({_, T}) -> false =:= T end, Matches),
                    {_, Pids} = lists:unzip(Bound),
                    case {lists:uniq(Pids), length(NonBound)} of
                        {[Pid], 0} ->
                            %% All addresses bound to one and same
                            %% process, proceed
                            {reply, {ok, {old, Pid}}, State};
                        {[Pid], _} ->
                            %% All bounded addresses bound to one and same
                            %% process, some unbounded addresses, proceed.
                            %% Part of multihoming; probably not ok in OTP27.
                            {reply, {ok, {new, old, Pid}}, State};
                        _ ->
                            {reply, {error, multiple_processes}, State}
                    end
            end
    end;
handle_call(_What, _From, State) ->
    {reply, undefined, State}.

terminate(_How, State) ->
    State.
