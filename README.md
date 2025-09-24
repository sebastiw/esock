# Overview

Setting up and managing connections via sockets.


# API

See [esock.erl](github.com/sebastiw/esock/blob/main/src/esock.erl)

```mermaid
---
SCTP relationship
---
flowchart TD
    EP(("Endpoint<br/>{Protocol, LocalIPs, LocalPort}"))
    AS1(("Association<br/>{RemoteIPs1, RemotePort1}"))
    AS2(("Association<br/>{RemoteIPs2, RemotePort2}"))
    AS3(("Association<br/>{RemoteIPs3, RemotePort3}"))

    P1(("Path<br>RemoteIP1_1"))
    P2(("Path<br>RemoteIP1_2"))
    P3(("Path<br>RemoteIP1_3"))
    P4(("Path<br>RemoteIP2_1"))
    P5(("Path<br>RemoteIP3_1"))
    P6(("Path<br>RemoteIP3_2"))

    EP-->AS1
    EP-->AS2
    EP-->AS3

    AS1-->P1
    AS1-->P2
    AS1-->P3
    AS2-->P4
    AS3-->P5
    AS3-->P6
```

How it relates internally with esock:
```mermaid
sequenceDiagram
    participant sa as Server<br />Application;
    participant se as Server<br />Esock;
    participant sk as Server<br />Kernel;
    participant ck as Client<br />Kernel;
    participant ce as Client<br />Esock;
    participant ca as Client<br />Application;
    sa->>se: esock:create_ep();
    se->>sk: esock_ep:start_link();
    note over sk: socket();
    note over sk: bind();

    ca->>ce: esock:create_ep();
    ce->>ck: esock_ep:start_link();
    note over ck: socket();
    ca->>ce: esock:create_assoc();
    ce->>ck: esock_assoc:start_link();
    note over ck: connect();
    ck-x sk: SCTP INIT
    ck-x sk: SCTP INIT
    note over sk: listen();
    ce-x sk: SCTP INIT
    sa->>se: esock:register_owner();
    se->>sk: esock_ep:register_owner();
    note over sk: accept();
    ck->>sk: SCTP INIT
    sk->>ck: SCTP INIT ACK
    ck->>sk: SCTP COOKIE
    sk->>ck: SCTP COOKIE ACK
    note over sk,ck: SCTP connection established
    note over sk: sctp_peeloff();

    ca->>ck: esock:send();
    ck->>sk: SCTP DATA
    sk->>sa: Owner ! data();

    sa->>sk: esock:send();
    sk->>ck: SCTP DATA
    ck->>ca: Owner ! data();
```



# Example

```erlang
%% start application supervisor
application:ensure_all_started(esock).

%% create a sctp ep on loopback interface with a free port
{ok, EP} = esock:create_ep().

%% connect sctp ep to remote address 127.0.0.1:30400
esock:create_assoc(EP, [{127,0,0,1}], 30400, #{}).

%% create another sctp ep which listen
{ok, EP2} = esock:create_ep(30400).

%% Start accepting connections and receive messages to self().
ok = esock:register_owner(EP, self(), {127,0,0,1}, 30400).
```

# Receive

Each read payload from the socket will be message passed to the
process that called create_ep, see type `received_msg()`.

# SCTP

This library is made mainly for the SCTP user application in mind.

From IETF RFC2960:

* SCTP user application (SCTP user): The logical higher-layer
application entity which uses the services of SCTP, also called
the Upper-layer Protocol (ULP).

* SCTP association: A protocol relationship between SCTP endpoints,
composed of the two SCTP endpoints and protocol state information
including Verification Tags and the currently active set of
Transmission Sequence Numbers (TSNs), etc. An association can be
uniquely identified by the transport addresses used by the
endpoints in the association. Two SCTP endpoints MUST NOT have
more than one SCTP association between them at any given time.

* SCTP endpoint: The logical sender/receiver of SCTP packets. On a
multi-homed host, an SCTP endpoint is represented to its peers as
a combination of a set of eligible destination transport addresses
to which SCTP packets can be sent and a set of eligible source
transport addresses from which SCTP packets can be received. All
transport addresses used by an SCTP endpoint must use the same
port number, but can use multiple IP addresses. A transport
address used by an SCTP endpoint must not be used by another SCTP
endpoint. In other words, a transport address is unique to an
SCTP endpoint.

