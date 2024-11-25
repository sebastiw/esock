-module(sock_reg).
-behaviour(gen_server).

%% API
-export([start_link/0,
         register_name/2,
         unregister_name/1,
         whereis_name/1,
         send/2
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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_name(Name :: term(), Pid :: pid()) -> yes | no.
register_name(Name, Pid) ->
    gen_server:call(?MODULE, {register_name, Name, Pid}).

-spec unregister_name(Name :: term()) -> yes | no.
unregister_name(Name) ->
    gen_server:call(?MODULE, {unregister_name, Name}).

-spec whereis_name(Name :: term()) -> pid() | undefined.
whereis_name(Name) ->
    gen_server:call(?MODULE, {whereis_name, Name}).

-spec send(Name :: term(), Msg :: term()) -> pid().
send(Name, Msg) ->
    gen_server:call(?MODULE, {send, Name, Msg}).

%% ---------------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------------

init(_) ->
    ets:new(sock_reg_ports, [named_table, bag]),
    ets:new(sock_reg_name, [named_table]),
    {ok, #{}}.

handle_info(_What, State) ->
    {noreply, State}.

handle_cast(_What, State) ->
        {noreply, State}.

handle_call({register_name, Name, Pid}, _From, State) ->
    {Protocol, IPs, Port} = Name,
    Reply = case ets:lookup(sock_reg_ports, {Protocol, Port}) of
                [] ->
                    insert_new(Name, Pid);
                [{_, IPs2}] ->
                    case already_bound(IPs, IPs2) of
                        true ->
                            no;
                        false ->
                            insert_new(Name, Pid)
                    end
            end,
    {reply, Reply, State};
handle_call({unregister_name, Name}, _From, State) ->
    {Protocol, IPs, Port} = Name,
    Reply = case ets:member(sock_reg_name, Name) of
                true ->
                    PortIPs = lists:zip([{Protocol, Port}], lists:usort(IPs), {pad, {{Protocol, Port}, loopback}}),
                    [ets:delete_object(sock_reg_ports, P) || P <- PortIPs],
                    ets:delete(sock_reg_name, Name),
                    yes;
                false ->
                    no
            end,
    {reply, Reply, State};
handle_call({whereis_name, Name}, _From, State) ->
    Reply = case ets:lookup(sock_reg_name, Name) of
                [] ->
                    undefined;
                [{_, Pid}] ->
                    Pid
            end,
    {reply, Reply, State};
handle_call({send, Name, Msg}, _From, State) ->
    Reply = case whereis_name(Name) of
                undefined ->
                    undefined;
                Pid ->
                    Pid ! Msg
            end,
    {reply, Reply, State}.

terminate(_How, _State) ->
    ets:delete(sock_reg_ports),
    ets:delete(sock_reg_name),
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

insert_new(Name, Pid) ->
    {Protocol, IPs, Port} = Name,
    PortIPs = lists:zip([{Protocol, Port}], lists:usort(IPs), {pad, {{Protocol, Port}, loopback}}),
    case ets:insert_new(sock_reg_name, {Name, Pid}) of
        true ->
            ets:insert_new(sock_reg_ports, PortIPs),
            yes;
        false ->
            no
    end.

already_bound([{_,_,_,_}|_] = IPs, IPs2) ->
    lists:member({0,0,0,0}, IPs2) orelse
        lists:all(fun (IP) -> lists:member(IP, IPs2) end, IPs);
already_bound([{_,_,_,_,_,_,_,_}|_] = IPs, IPs2) ->
    lists:member({0,0,0,0,0,0,0,0}, IPs2) orelse
        lists:all(fun (IP) -> lists:member(IP, IPs2) end, IPs);
already_bound(IPs, IPs2) ->
    lists:all(fun (IP) -> lists:member(IP, IPs2) end, IPs).
