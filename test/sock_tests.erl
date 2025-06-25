-module(sock_tests).

-include_lib("eunit/include/eunit.hrl").

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

connect_accept_1_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (_) ->
         redbug:start(["sock_assoc:connect_async->return", "socket:connect->return", "gen_sctp:connect->return"], #{print_file => "user.log"}),
         timer:sleep(100),
         FreePort1 = get_free_port(),
         FreePort2 = get_free_port(),
         FreePort3 = get_free_port(),
         {ok, _EP1} = sock:create_ep([loopback], FreePort1, [{accept, 1}]),
         {ok, EP2} = sock:create_ep([loopback], FreePort2, []),
         {ok, EP3} = sock:create_ep([loopback], FreePort3, []),
         A = sock:create_assoc(EP2, [loopback], FreePort1, []),
         Err = sock:create_assoc(EP3, [loopback], FreePort1, []),
         redbug:stop(),
         [?_assertMatch({ok, _Assoc1}, A),
          ?_assertMatch({error, einval}, Err)]
     end}.

get_free_port() ->
    {ok, Sock} = socket:open(inet, seqpacket, sctp),
    ok = socket:bind(Sock, #{family => inet, addr => loopback, port => 0}),
    {ok, #{port := Port}} = socket:sockname(Sock),
    ok = socket:close(Sock),
    Port.
