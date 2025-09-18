-module(esock_utils).

-export([get_domain/2,
         socket_address/3,
         ip_in_subnet/2
        ]).

-include_lib("eunit/include/eunit.hrl").

get_domain([{_, _, _, _} | _], _Opts) ->
    {ok, inet};
get_domain([{_, _, _, _, _, _, _, _} | _], _Opts) ->
    {ok, inet6};
get_domain([loopback | _], _Opts) ->
    %% Actually this should be `{ok, local}`; but gen_sctp:open does
    %% not accept that, even though documentation says it should.
    {ok, inet};
get_domain([_ | R], Opts) ->
    get_domain(R, Opts).

socket_address(Domain, LocalAddr, LocalPort) ->
    #{family => domain_to_family(Domain), addr => LocalAddr, port => LocalPort}.

domain_to_family(local) ->
    inet;
domain_to_family(Domain) ->
    Domain.

-spec ip_in_subnet(binary(), binary()) -> boolean().
ip_in_subnet(IP, Subnet) ->
    [RP, BL] = string:split(Subnet, "/"),
    BitLength = binary_to_integer(BL),
    {ok, RPTup} = inet:parse_address(binary_to_list(RP)),
    {ok, IPTup} = inet:parse_address(binary_to_list(IP)),
    MS = tuple_size(RPTup)*2,
    BS = tuple_size(RPTup)*MS,
    <<RPRaw:BS>> = <<<<A:MS>> || A <- tuple_to_list(RPTup)>>,
    <<IPRaw:BS>> = <<<<A:MS>> || A <- tuple_to_list(IPTup)>>,
    Mask = ((1 bsl BS) - 1) bxor ((1 bsl (BS-BitLength)) - 1),
    (RPRaw band Mask) == (IPRaw band Mask).

ipv4_in_subnet_test_() ->
    [?_assertEqual(true, ip_in_subnet(<<"192.168.0.68">>, <<"0.0.0.0/0">>)),
     ?_assertEqual(false, ip_in_subnet(<<"192.168.0.68">>, <<"255.255.255.255/32">>)),
     ?_assertEqual(true, ip_in_subnet(<<"192.168.0.68">>, <<"192.168.0.68/32">>)),
     ?_assertEqual(true, ip_in_subnet(<<"192.168.0.68">>, <<"192.168.0.68/30">>)),
     ?_assertEqual(true, ip_in_subnet(<<"192.168.0.71">>, <<"192.168.0.68/30">>)),
     ?_assertEqual(false, ip_in_subnet(<<"0.0.0.0">>, <<"255.255.255.254/32">>)),
     ?_assertEqual(true, ip_in_subnet(<<"0.0.0.0">>, <<"255.255.255.255/0">>))
    ].

ipv6_in_subnet_test_() ->
    [?_assertEqual(true, ip_in_subnet(<<"FFFF:FF00::6868">>, <<"::/0">>)),
     ?_assertEqual(false, ip_in_subnet(<<"FFFF:FF00::6868">>, <<"FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF/128">>)),
     ?_assertEqual(true, ip_in_subnet(<<"FFFF:FF00::6868">>, <<"FFFF:FF00::6868/128">>)),
     ?_assertEqual(true, ip_in_subnet(<<"FFFF:FF00::6868">>, <<"FFFF:FF00::6868/96">>)),
     ?_assertEqual(true, ip_in_subnet(<<"FFFF:FF00::7171">>, <<"FFFF:FF00::6868/96">>)),
     ?_assertEqual(false, ip_in_subnet(<<"0000::">>, <<"FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFE/128">>)),
     ?_assertEqual(true, ip_in_subnet(<<"0000::">>, <<"FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF/0">>))
    ].
