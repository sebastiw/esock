-module(esock_options).

-export([set_socket_options/3
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(IPPROTO_SCTP, 132).
-define(SCTP_DEFAULT_SEND_PARAM, 10).
-define(SCTP_PPID_DIAMETER, 46).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-spec set_socket_options(esock:protocol(), socket:socket(), map()) -> ok.
set_socket_options(sctp, Sock, State) ->
    set_sctp_sndrcvinfo(Sock, maps:get(sctp_sndrcvinfo, State, #{}));
set_socket_options(_, _, _) ->
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

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
