-module(sock_utils).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([make_name/3,
         new_socket/1,
         get_matching_pids/2
        ]).

-define(IPPROTO_SCTP, 132).
-define(SCTP_DEFAULT_SEND_PARAM, 10).
-define(SCTP_PPID_DIAMETER, 46).

%% TODO: How to handle conflicts?
%% Any ALL/ANY combination of IP ({0,0,0,0}) and Port (0) will bind to a
%% all IP-interfaces and/or an arbitrary port, which will give this process a name
%% like `sock_connecter_0.0.0.0_0', but will have binded for instance at
%% 127.0.0.1, and 192.168.0.68, at port 49302.
%% If then another process comes along which want to use 192.168.0.68:49302, it
%% will crash at binding due to ip/port already taken, or that it cannot find
%% the process named `sock_connecter_192.168.0.68_49302'.
make_name(Base, IP, Port) ->
    Addr = inet:ntoa(IP),
    NameElems = [atom_to_list(Base), Addr, integer_to_list(Port)],
    list_to_atom(lists:concat(lists:join("_", NameElems))).

new_socket(State) ->
    LocalInfo = maps:get(local_info, State),
    Domain = maps:get(domain, LocalInfo),
    Protocol = maps:get(protocol, LocalInfo),
    {ok, Sock} = socket:open(Domain, stream, Protocol),
    MonRef = socket:monitor(Sock),
    bind_local_addr(Sock, State),
    set_socket_options(Protocol, Sock, State),
    State#{
           state => closed,
           socket => Sock,
           monitor => MonRef
          }.

bind_local_addr(Sock, Opts) ->
  LAddr = maps:get(local_addr, Opts),
  LIP = maps:get(addr, LAddr),
  LPort = maps:get(port, LAddr),
  bind_local_addr(Sock, LIP, LPort).

bind_local_addr(Sock, IP, LPort) ->
  %% TODO: bind to multiple ip-addresses?
  ok = socket:bind(Sock, #{family => inet, port => LPort, addr => IP}).

set_socket_options(sctp, Sock, State) ->
    set_sctp_sndrcvinfo(Sock, maps:get(sctp_sndrcvinfo, State, #{}));
set_socket_options(_, _Sock, _State) ->
    ok.

set_sctp_sndrcvinfo(Sock, SndRcvInfo) ->
  PPID = maps:get(ppid, SndRcvInfo, ?SCTP_PPID_DIAMETER),
  TTL = maps:get(ttl, SndRcvInfo, 0),
  Context = maps:get(context, SndRcvInfo, 0),

  %% See 5.3.2. SCTP Header Information Structure (SCTP_SNDRCV) - DEPRECATED
  %% Deprecated in RFC6458, but the alternative of using a cmsg
  %% SCTP_SNDINFO is not supported by the version of sctp
  %% (lksctp-tools) that Ubuntu uses.
  SndRcvInfoBin = <<16#0000:16,
                    16#0000:16,
                    16#0000:16,
                    16#0000:16, %% one of these 16-bits shouldn't exist?
                    PPID:32,
                    Context:32, %% used for abort/error reporting
                    TTL:32, %% socket-process should be able to write to socket
                            %% within TTL, otherwise discard msg.
                    16#0000_0000:32,
                    16#0000_0000:32,
                    16#0000_0000:32>>,
  ok = socket:setopt_native(Sock, {?IPPROTO_SCTP, ?SCTP_DEFAULT_SEND_PARAM}, SndRcvInfoBin).

get_matching_pids(MaskPortPidPairs, InetAddr) ->
    #{addr := Addr, port := Port} = InetAddr,
    case inet:ntoa(Addr) of
        {error, einval} ->
            ?LOG_INFO("~s:~B Unsupported address ~p~n", [?MODULE, ?LINE, Addr]),
            [];
        A ->
            IP = list_to_binary(A),
            [MPRP || {Mask, P, _, _} = MPRP <- MaskPortPidPairs,
                     ip_in_subnet(IP, Mask),
                     port_matches(P, Port)]
    end.

get_matching_pids_test_() ->
    [{"Empty state",
      ?_assertEqual([],
                    get_matching_pids([],
                                      #{family => inet, port => 3868, addr => {192, 168, 0, 68}}))},
     {"Matching IP",
      ?_assertEqual([{<<"192.168.0.68/32">>, 0, 1, 68}],
                    get_matching_pids([{<<"192.168.0.68/32">>, 0, 1, 68},
                                       {<<"192.168.0.20/32">>, 0, 2, 20}],
                                      #{family => inet, port => 3868, addr => {192, 168, 0, 68}}))},
     {"Matching Port",
      ?_assertEqual([{<<"192.168.0.68/32">>, 3868, 1, 68}],
                    get_matching_pids([{<<"192.168.0.68/32">>, 3868, 1, 68},
                                       {<<"192.168.0.68/32">>, 2020, 2, 20}],
                                      #{family => inet, port => 3868, addr => {192, 168, 0, 68}}))},
     {"Multiple matches",
      ?_assertEqual([{<<"192.168.0.68/32">>, 3868, 1, 68},
                     {<<"192.168.0.68/32">>, 0, 2, 20}],
                    get_matching_pids([{<<"192.168.0.68/32">>, 3868, 1, 68},
                                       {<<"192.168.0.68/32">>, 0, 2, 20}],
                                      #{family => inet, port => 3868, addr => {192, 168, 0, 68}}))},
     {"No matching ip",
      ?_assertEqual([],
                    get_matching_pids([{<<"192.168.0.68/32">>, 0, 1, 68},
                                       {<<"192.168.0.20/32">>, 0, 2, 20}],
                                      #{family => inet, port => 3868, addr => {192, 168, 0, 44}}))},
     {"No matching port",
      ?_assertEqual([],
                    get_matching_pids([{<<"192.168.0.68/32">>, 2020, 1, 20},
                                       {<<"192.168.0.68/32">>, 3868, 2, 68}],
                                      #{family => inet, port => 4444, addr => {192, 168, 0, 68}}))}
    ].

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

port_matches(0, _) ->
    true;
port_matches(P, P) ->
    true;
port_matches(_, _) ->
    false.
