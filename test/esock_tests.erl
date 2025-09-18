-module(esock_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(esock).

teardown(_) ->
    application:stop(esock).

ep_already_started_same_ip_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (_) ->
             FreePort1 = get_free_port(),
             {ok, EP1} = esock:create_ep(FreePort1, []),
             Err = esock:create_ep(FreePort1, []),
             Ass = esock:get_assocs(EP1),
             [?_assert(is_pid(EP1)),
              ?_assertMatch({error, {already_started, EP1}}, Err),
              ?_assertMatch({ok, []}, Ass)]
     end}.

listen_accept_1_test_() ->
    {"Create one EP and listen to incoming connections, accept 1 and reject 1.",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun (_) ->
              %% Socket under test
              FreePort1 = get_free_port(),
              {ok, _LEP1} = esock:create_ep([loopback], FreePort1, [{accept, 1}]),

              %% Create some connecting clients
              {ok, CSock1} = gen_sctp:open(),
              {ok, CSock2} = gen_sctp:open(),

              {_, {_, _, [], #sctp_assoc_change{state = comm_up} = CAssoc1}} = gen_sctp:connect(CSock1, #{family => inet, addr => loopback, port => FreePort1}, []),
              {_, {_, _, [], #sctp_assoc_change{state = comm_up} = CAssoc2}} = gen_sctp:connect(CSock2, #{family => inet, addr => loopback, port => FreePort1}, []),

              ok = gen_sctp:send(CSock1, #sctp_sndrcvinfo{assoc_id = CAssoc1#sctp_assoc_change.assoc_id}, <<"Hello">>),
              ok = gen_sctp:send(CSock2, #sctp_sndrcvinfo{assoc_id = CAssoc2#sctp_assoc_change.assoc_id}, <<"Hello">>),

              CollectMsg = collect_msgs(),
              [?_assertMatch([{ok, _}], CollectMsg)]
      end}}.

connect_1_test_() ->
    {"Create an assoc and connect to remote address.",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun (_) ->
              %% redbug:start(["sock_ep->return", "gen_sctp:connect->return"], #{print_file => "user.log"}),
              %% timer:sleep(100),
              %% redbug:stop(),

              %% Create some listening servers
              FreePort1 = get_free_port(),
              FreePort2 = get_free_port(),
              {ok, LSock1} = gen_sctp:open([{type, seqpacket}, inet, {ifaddr, #{family => inet, addr => loopback, port => FreePort1}}]),
              {ok, LSock2} = gen_sctp:open([{type, seqpacket}, inet, {ifaddr, #{family => inet, addr => loopback, port => FreePort2}}]),

              ok = gen_sctp:listen(LSock1, true),
              ok = gen_sctp:listen(LSock2, true),


              %% Socket under test
              FreePort3 = get_free_port(),
              {ok, CEP1} = esock:create_ep([loopback], FreePort3, []),

              {ok, _CAssoc1} = esock:create_assoc(CEP1, [loopback], FreePort1, []),

              {ok, {_, _, [], #sctp_assoc_change{state = comm_up} = LAssoc1}} = gen_sctp:recv(LSock1),

              ok = gen_sctp:send(LSock1, #sctp_sndrcvinfo{assoc_id = LAssoc1#sctp_assoc_change.assoc_id}, <<"Hello">>),
              {error, epipe} = gen_sctp:send(LSock2, #sctp_sndrcvinfo{assoc_id = LAssoc1#sctp_assoc_change.assoc_id}, <<"Hello">>),

              CollectMsg = collect_msgs(),
              [?_assertMatch([{ok, _}], CollectMsg)]
      end}}.

get_free_port() ->
    {ok, Sock} = socket:open(inet, seqpacket, sctp),
    ok = socket:bind(Sock, #{family => inet, addr => loopback, port => 0}),
    {ok, #{port := Port}} = socket:sockname(Sock),
    ok = socket:close(Sock),
    Port.

collect_msgs() ->
    collect_msgs([]).

collect_msgs(Acc) ->
    receive
        {recv, _IP, _Port, _Msg, _Anc} = Recv ->
            collect_msgs([{ok, Recv} | Acc]);
        Else ->
            collect_msgs([{error, Else} | Acc])
    after 100 ->
            Acc
    end.
