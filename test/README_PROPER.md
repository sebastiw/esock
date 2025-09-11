# PropEr Property-Based Testing for Sock Library

This document describes the PropEr (Property-based testing) test suite for the sock library.

## Overview

The sock library now includes comprehensive property-based tests using PropEr, which complement the existing EUnit tests. Property-based testing generates random test data and verifies that certain properties hold across a wide range of inputs, helping to discover edge cases that might be missed by traditional example-based tests.

## Test Files

- `test/sock_proper_tests.erl` - Main PropEr test module
- `test/sock_tests.erl` - Updated to include PropEr test integration

## Properties Tested

### 1. `prop_create_ep_valid_ports/0`
**Property**: Creating endpoints with valid port numbers should succeed.
- **Generator**: Valid ports (1024-65535)
- **Verification**: Endpoint creation returns `{ok, Pid}` and the process is alive
- **Edge cases**: Tests various port numbers in the valid range

### 2. `prop_create_ep_invalid_ports/0`
**Property**: Creating endpoints with invalid port numbers should fail gracefully.
- **Generator**: Invalid ports (negative numbers)
- **Verification**: System handles invalid input without crashing
- **Note**: Only tests truly invalid ports (negative) as some systems accept ports > 65535

### 3. `prop_create_assoc_valid_endpoints/0`
**Property**: Creating associations between valid endpoints should work.
- **Generator**: Pairs of valid port numbers for server and client
- **Verification**: Association creation succeeds and is tracked by the client endpoint
- **Coverage**: Tests the core functionality of establishing connections

### 4. `prop_multiple_endpoints_same_port/0`
**Property**: Multiple endpoints cannot bind to the same port.
- **Generator**: Single port number
- **Verification**: Second endpoint creation fails with appropriate error
- **Edge cases**: Tests port collision detection

### 5. `prop_endpoint_lifecycle/0`
**Property**: Endpoint lifecycle management works correctly.
- **Generator**: Valid port numbers
- **Verification**: Endpoints can be created, used, and terminated properly
- **Coverage**: Tests process lifecycle and cleanup

### 6. `prop_association_lifecycle/0`
**Property**: Association lifecycle management works correctly.
- **Generator**: Pairs of valid port numbers
- **Verification**: Associations can be created, tracked, and terminated
- **Coverage**: Tests association process management

### 7. `prop_concurrent_connections/0`
**Property**: Concurrent connections respect server accept limits.
- **Generator**: Server port, accept limit (1-3), and number of clients (1-5)
- **Verification**: All association creation calls succeed (actual limiting happens at server level)
- **Coverage**: Tests concurrent connection handling

### 8. `prop_endpoint_options/0`
**Property**: Endpoint options are handled correctly.
- **Generator**: Valid port and protocol (currently only SCTP)
- **Verification**: Endpoints can be created with various options
- **Coverage**: Tests configuration handling

## Running the Tests

### Via EUnit (Recommended)
```bash
rebar3 as test eunit
```
The PropEr tests are automatically integrated into the EUnit test suite.

### Direct PropEr Execution
```bash
rebar3 as test shell --apps sock
```
Then in the shell:
```erlang
% Run all properties with 50 tests each
sock_proper_tests:test(50).

% Run a specific property
proper:quickcheck(sock_proper_tests:prop_create_ep_valid_ports(), 100).
```

## Configuration

The test suite uses the following configuration:
- **Default test count**: 50 tests per property (configurable)
- **Timeout**: 60 seconds for the entire PropEr test suite
- **Application lifecycle**: Each property test starts/stops the sock application

## Test Data Generators

### `valid_port/0`
Generates port numbers in the range 1024-65535, avoiding system ports.

### `invalid_port/0`
Generates negative port numbers (-1000 to -1).

### Helper Functions

- `setup_application/0` - Ensures the sock application is started
- `cleanup_application/0` - Stops the sock application
- `get_free_port/0` - Finds an available port for testing

## Benefits of Property-Based Testing

1. **Edge Case Discovery**: Automatically finds edge cases that manual tests might miss
2. **Regression Prevention**: Large number of generated test cases help prevent regressions
3. **Specification Verification**: Properties serve as executable specifications
4. **Confidence Building**: Extensive testing across input space increases confidence
5. **Shrinking**: When failures occur, PropEr automatically finds minimal failing cases

## Integration with CI/CD

The PropEr tests are integrated into the standard test suite and will run automatically in CI/CD pipelines. The tests are designed to be:
- **Deterministic**: Same seed produces same results
- **Fast**: Efficient generators and reasonable test counts
- **Reliable**: Proper application lifecycle management

## Future Enhancements

Potential areas for expansion:
1. **TCP Protocol Support**: Add tests for TCP when implemented
2. **Data Transmission**: Test actual data sending/receiving when available
3. **Error Injection**: Test behavior under various failure conditions
4. **Performance Properties**: Add properties about performance characteristics
5. **Stateful Testing**: Use PropEr's stateful testing for complex scenarios

## Dependencies

- **PropEr**: Property-based testing framework
- **EUnit**: For test integration and assertions
- **Sock Application**: The library under test

The PropEr dependency is automatically managed by rebar3 in the test profile.
