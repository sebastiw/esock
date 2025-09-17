-module(sock_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(sock).

teardown(_) ->
    application:stop(sock).

ep_already_started_same_ip_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (_) ->
             FreePort1 = get_free_port(),
             {ok, EP1} = sock:create_ep(FreePort1, []),
             Err = sock:create_ep(FreePort1, []),
             Ass = sock:get_assocs(EP1),
             [?_assert(is_pid(EP1)),
              ?_assertMatch({error, {already_started, EP1}}, Err),
              ?_assertMatch({ok, []}, Ass)]
     end}.

listen_accept_1_test() ->
    {"Create one EP and listen to incoming connections, accept 1 and reject 1.",
     {setup,
      fun setup/0,
      fun teardown/1,
      fun (_) ->
              %% Socket under test
              FreePort1 = get_free_port(),
              {ok, _LEP1} = sock:create_ep([loopback], FreePort1, [{accept, 1}]),

              %% Create some connecting clients
              {ok, CSock1} = gen_sctp:open(),
              {ok, CSock2} = gen_sctp:open(),

              {_, {_, _, [], #sctp_assoc_change{state = comm_up} = CAssoc1}} = gen_sctp:connect(CSock1, #{family => inet, addr => loopback, port => FreePort1}, []),
              {_, {_, _, [], #sctp_assoc_change{state = comm_up} = CAssoc2}} = gen_sctp:connect(CSock2, #{family => inet, addr => loopback, port => FreePort1}, []),

              ok = gen_sctp:send(CSock1, #sctp_sndrcvinfo{assoc_id = CAssoc1#sctp_assoc_change.assoc_id}, <<"Hello">>),
              ok = gen_sctp:send(CSock2, #sctp_sndrcvinfo{assoc_id = CAssoc2#sctp_assoc_change.assoc_id}, <<"Hello">>),

              receive
                  {recv, _IP, _Port, <<"Hello">>, _Anc} ->
                      receive
                          _ ->
                              exit(1)
                      after 100 ->
                              ok
                      end;
                  O ->
                      io:format("ERROR ~p~n", [O]),
                      exit(3)
              after 100 ->
                      exit(2)
              end
      end}}.

get_free_port() ->
    {ok, Sock} = socket:open(inet, seqpacket, sctp),
    ok = socket:bind(Sock, #{family => inet, addr => loopback, port => 0}),
    {ok, #{port := Port}} = socket:sockname(Sock),
    ok = socket:close(Sock),
    Port.
