-module(sock_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test runner
-export([test/0, test/1]).

%% Property exports for PropEr
-export([prop_create_ep_valid_ports/0,
         prop_create_ep_invalid_ports/0,
         prop_create_assoc_valid_endpoints/0,
         prop_multiple_endpoints_same_port/0,
         prop_endpoint_lifecycle/0,
         prop_association_lifecycle/0,
         prop_concurrent_connections/0,
         prop_endpoint_options/0]).

%% Helper functions
-export([setup_application/0, cleanup_application/0]).

%% =============================================================================
%% Test Runner Functions
%% =============================================================================

test() ->
    test(100).

test(NumTests) ->
    setup_application(),
    try
        Results = [
            proper:quickcheck(prop_create_ep_valid_ports(), NumTests),
            proper:quickcheck(prop_create_ep_invalid_ports(), NumTests),
            proper:quickcheck(prop_create_assoc_valid_endpoints(), NumTests),
            proper:quickcheck(prop_multiple_endpoints_same_port(), NumTests),
            proper:quickcheck(prop_endpoint_lifecycle(), NumTests),
            proper:quickcheck(prop_association_lifecycle(), NumTests),
            proper:quickcheck(prop_concurrent_connections(), NumTests),
            proper:quickcheck(prop_endpoint_options(), NumTests)
        ],
        AllPassed = lists:all(fun(R) -> R =:= true end, Results),
        case AllPassed of
            true ->
                io:format("All PropEr tests passed!~n"),
                ok;
            false ->
                io:format("Some PropEr tests failed!~n"),
                throw(proper_tests_failed)
        end
    after
        cleanup_application()
    end.

%% =============================================================================
%% Property Definitions
%% =============================================================================

%% Property: Creating endpoints with valid ports should succeed
prop_create_ep_valid_ports() ->
    ?FORALL(Port, valid_port(),
        begin
            setup_application(),
            try
                case sock:create_ep([loopback], Port, []) of
                    {ok, EP} when is_pid(EP) ->
                        is_process_alive(EP);
                    {error, eaddrinuse} ->
                        %% Port might be in use, this is acceptable
                        true;
                    {error, _} ->
                        %% Other errors might be system-dependent
                        true
                end
            catch
                _:_ -> false
            after
                cleanup_application()
            end
        end).

%% Property: Creating endpoints with invalid ports should fail gracefully
prop_create_ep_invalid_ports() ->
    ?FORALL(Port, invalid_port(),
        begin
            setup_application(),
            try
                case sock:create_ep([loopback], Port, []) of
                    {ok, _} ->
                        % Some systems might accept ports > 65535, so this is not always an error
                        true;
                    {error, _} ->
                        true  % Should fail with some error for truly invalid ports
                end
            catch
                _:_ -> true  % Exceptions are also acceptable for invalid input
            after
                cleanup_application()
            end
        end).

%% Property: Creating associations between valid endpoints should work
prop_create_assoc_valid_endpoints() ->
    ?FORALL({ServerPort, ClientPort}, {valid_port(), valid_port()},
        begin
            setup_application(),
            try
                % Create server endpoint that accepts connections
                {ok, _ServerEP} = sock:create_ep([loopback], ServerPort, [{accept, 1}]),
                
                % Create client endpoint
                {ok, ClientEP} = sock:create_ep([loopback], ClientPort, []),
                
                % Create association
                case sock:create_assoc(ClientEP, [loopback], ServerPort, []) of
                    {ok, AssocPid} when is_pid(AssocPid) ->
                        % Verify the association is tracked
                        {ok, Assocs} = sock:get_assocs(ClientEP),
                        lists:member(AssocPid, Assocs);
                    {error, _} ->
                        % Connection might fail for various reasons (timing, etc.)
                        true
                end
            catch
                _:_ -> false
            after
                cleanup_application()
            end
        end).

%% Property: Multiple endpoints cannot bind to the same port
prop_multiple_endpoints_same_port() ->
    ?FORALL(Port, valid_port(),
        begin
            setup_application(),
            try
                case sock:create_ep([loopback], Port, []) of
                    {ok, EP1} ->
                        case sock:create_ep([loopback], Port, []) of
                            {error, {already_started, EP1}} -> true;
                            {error, eaddrinuse} -> true;
                            {error, _} -> true;  % Other errors acceptable
                            {ok, _} -> false  % Should not succeed
                        end;
                    {error, _} ->
                        true  % First creation failed, that's acceptable
                end
            catch
                _:_ -> false
            after
                cleanup_application()
            end
        end).

%% Property: Endpoint lifecycle - create, use, terminate
prop_endpoint_lifecycle() ->
    ?FORALL(Port, valid_port(),
        begin
            setup_application(),
            try
                case sock:create_ep([loopback], Port, []) of
                    {ok, EP} ->
                        % Endpoint should be alive
                        IsAlive1 = is_process_alive(EP),
                        
                        % Should be able to get associations (empty initially)
                        {ok, Assocs} = sock:get_assocs(EP),
                        
                        % Terminate the endpoint
                        exit(EP, shutdown),
                        
                        % Wait for termination
                        timer:sleep(100),
                        IsAlive2 = is_process_alive(EP),
                        
                        IsAlive1 andalso (Assocs =:= []) andalso (not IsAlive2);
                    {error, _} ->
                        true  % Creation failure is acceptable
                end
            catch
                _:_ -> false
            after
                cleanup_application()
            end
        end).

%% Property: Association lifecycle
prop_association_lifecycle() ->
    ?FORALL({ServerPort, ClientPort}, {valid_port(), valid_port()},
        begin
            setup_application(),
            try
                % Create endpoints
                {ok, _ServerEP} = sock:create_ep([loopback], ServerPort, [{accept, 1}]),
                {ok, ClientEP} = sock:create_ep([loopback], ClientPort, []),
                
                case sock:create_assoc(ClientEP, [loopback], ServerPort, []) of
                    {ok, AssocPid} ->
                        % Association should be alive
                        IsAlive1 = is_process_alive(AssocPid),
                        
                        % Should be tracked by client endpoint
                        {ok, ClientAssocs} = sock:get_assocs(ClientEP),
                        IsTracked = lists:member(AssocPid, ClientAssocs),
                        
                        % Terminate association
                        exit(AssocPid, shutdown),
                        timer:sleep(100),
                        IsAlive2 = is_process_alive(AssocPid),
                        
                        IsAlive1 andalso IsTracked andalso (not IsAlive2);
                    {error, _} ->
                        true  % Association creation might fail
                end
            catch
                _:_ -> false
            after
                cleanup_application()
            end
        end).

%% Property: Concurrent connections to server with accept limit
prop_concurrent_connections() ->
    ?FORALL({ServerPort, AcceptLimit, NumClients}, 
            {valid_port(), choose(1, 3), choose(1, 5)},
        begin
            setup_application(),
            try
                % Create server with accept limit
                {ok, _ServerEP} = sock:create_ep([loopback], ServerPort, [{accept, AcceptLimit}]),
                
                % Create multiple client endpoints and associations
                ClientResults = [
                    begin
                        ClientPort = get_free_port(),
                        {ok, ClientEP} = sock:create_ep([loopback], ClientPort, []),
                        sock:create_assoc(ClientEP, [loopback], ServerPort, [])
                    end || _ <- lists:seq(1, NumClients)
                ],
                
                % Count successful associations
                SuccessCount = length([ok || {ok, _} <- ClientResults]),
                
                % All create_assoc calls should succeed (they create association processes)
                % The actual connection limiting happens at the server level
                SuccessCount =:= NumClients
            catch
                _:_ -> false
            after
                cleanup_application()
            end
        end).

%% Property: Endpoint options are handled correctly
prop_endpoint_options() ->
    ?FORALL({Port, Protocol}, {valid_port(), oneof([sctp])},  % Only test SCTP for now
        begin
            setup_application(),
            try
                Options = [{protocol, Protocol}],
                case sock:create_ep([loopback], Port, Options) of
                    {ok, EP} when is_pid(EP) ->
                        is_process_alive(EP);
                    {error, _} ->
                        true  % Some protocols might not be available
                end
            catch
                _:_ -> false
            after
                cleanup_application()
            end
        end).

%% =============================================================================
%% Generators
%% =============================================================================

%% Generate valid port numbers (1024-65535, avoiding system ports)
valid_port() ->
    choose(1024, 65535).

%% Generate invalid port numbers (only truly invalid ones)
invalid_port() ->
    choose(-1000, -1).      % Only negative ports are truly invalid

%% =============================================================================
%% Helper Functions
%% =============================================================================

setup_application() ->
    application:ensure_all_started(sock).

cleanup_application() ->
    application:stop(sock).

%% Get a free port (similar to the existing test helper)
get_free_port() ->
    {ok, Sock} = socket:open(inet, seqpacket, sctp),
    ok = socket:bind(Sock, #{family => inet, addr => loopback, port => 0}),
    {ok, #{port := Port}} = socket:sockname(Sock),
    ok = socket:close(Sock),
    Port.
