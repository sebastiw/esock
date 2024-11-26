-module(sock_utils).

-export([get_domain/2,
         socket_address/3,
         ip_in_subnet/2
        ]).

-include_lib("eunit/include/eunit.hrl").

get_domain([{_, _, _, _} | _], _Opts) ->
    {ok, inet};
get_domain([{_, _, _, _, _, _, _, _} | _], _Opts) ->
    {ok, inet6};
get_domain([localhost | _], _Opts) ->
    {ok, local}.

socket_address(Domain, LocalAddr, LocalPort) ->
    #{family => domain_to_family(Domain), addr => LocalAddr, port => LocalPort}.

domain_to_family(local) ->
    loopback;
domain_to_family(Domain) ->
    Domain.

-spec ip_in_subnet(binary(), binary()) -> boolean().
ip_in_subnet(IP, Subnet) ->
  [RP, BL] = string:split(Subnet, "/"),
  BitLength = binary_to_integer(BL),
  RPParts = string:split(RP, ".", all),
  IPParts = string:split(IP, ".", all),
  <<RPRaw:32>> = <<<<(binary_to_integer(A))>> || A <- RPParts>>,
  <<IPRaw:32>> = <<<<(binary_to_integer(A))>> || A <- IPParts>>,
  Mask = ((1 bsl 32) - 1) bxor ((1 bsl (32-BitLength)) - 1),
  (RPRaw band Mask) == (IPRaw band Mask).

ip_in_subnet_test_() ->
    [?_assertEqual(true, ip_in_subnet(<<"192.168.0.68">>, <<"0.0.0.0/0">>)),
     ?_assertEqual(false, ip_in_subnet(<<"192.168.0.68">>, <<"255.255.255.255/32">>)),
     ?_assertEqual(true, ip_in_subnet(<<"192.168.0.68">>, <<"192.168.0.68/32">>)),
     ?_assertEqual(true, ip_in_subnet(<<"192.168.0.68">>, <<"192.168.0.68/30">>)),
     ?_assertEqual(true, ip_in_subnet(<<"192.168.0.71">>, <<"192.168.0.68/30">>)),
     ?_assertEqual(false, ip_in_subnet(<<"0.0.0.0">>, <<"255.255.255.254/32">>))
    ].
