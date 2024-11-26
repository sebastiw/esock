-module(sock_tests).

-include_lib("eunit/include/eunit.hrl").

connect_accept_1_test() ->
    application:ensure_all_started(sock),
    FreePort1 = get_free_port(),
    FreePort2 = get_free_port(),
    {ok, EP1} = sock:create_ep(FreePort1, [{accept, 1}]),
    {ok, EP2} = sock:create_ep(FreePort2, []),
    ok.

get_free_port() ->
    {ok, Sock} = socket:open(inet, seqpacket, sctp),
    ok = socket:bind(Sock, #{family => inet, addr => loopback, port => 0}),
    {ok, #{port := Port}} = socket:sockname(Sock),
    ok = socket:close(Sock),
    Port.
