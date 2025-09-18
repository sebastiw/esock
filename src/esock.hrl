-type port_no() :: inet:port_number().
-type address() :: inet:ip_address().

-type anc_data() :: socket:cmsg_recv() | #{level := socket:level() | integer(), type := integer(), data := binary()}.

-type accept_callback() :: fun((address(), port_no(), [anc_data()], CurrentAssocs :: integer()) -> boolean()) |
                           fun((address(), port_no(), [anc_data()]) -> boolean()).
