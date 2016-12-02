eredis_sentinel
=====

Configuration:

    {eredis_sentinel, [
      {sentinel_addrs, [["some-host", 26379], ["some-other-host", 26379]]}
    ]}

Examples:

    {ok, Conn} = eredis_sentinel:connect_eredis(test, 0),

    {ok, Conn} = eredis_sentinel:connect_eredis_sync(test, 0),

    {ok, [Host, Port]} = eredis_sentinel:get_master(test).
