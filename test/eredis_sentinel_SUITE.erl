-module(eredis_sentinel_SUITE).

-export([all/0]).
-export([
  connect_eredis_sentinel_ok/1,
  connect_eredis_sentinel_with_redis_params_ok/1,
  connect_eredis_sentinel_not_available/1,
  connect_eredis_sync_sentinel_ok/1,
  connect_eredis_sync_sentinel_with_redis_params_ok/1,
  connect_eredis_sync_sentinel_not_available/1,
  get_master_master_unknown/1,
  get_master_ok/1
]).

-define(WRONG_SENTINEL, #{sentinel_addrs => [["127.0.0.1", 9999]]}).
-define(GOOD_SENTINEL, #{sentinel_addrs => [["127.0.0.1", 26379]]}).

all() -> [
  connect_eredis_sentinel_ok,
  connect_eredis_sentinel_with_redis_params_ok,
  connect_eredis_sentinel_not_available,
  connect_eredis_sync_sentinel_ok,
  connect_eredis_sync_sentinel_with_redis_params_ok,
  connect_eredis_sync_sentinel_not_available,
  get_master_master_unknown,
  get_master_ok
].

connect_eredis_sentinel_not_available(_Config) ->
  {error, all_sentinels_down} = eredis_sentinel:connect_eredis(some_master, 0, ?WRONG_SENTINEL).

connect_eredis_sentinel_ok(_Config) ->
  {ok, Conn} = eredis_sentinel:connect_eredis(test, 0, ?GOOD_SENTINEL),
  {ok, <<"PONG">>} = eredis:q(Conn, ["PING"]).

connect_eredis_sentinel_with_redis_params_ok(_Config) ->
  {ok, Conn} = eredis_sentinel:connect_eredis(mymaster, [0], ?GOOD_SENTINEL),
  {ok, <<"PONG">>} = eredis:q(Conn, ["PING"]).

connect_eredis_sync_sentinel_ok(_Config) ->
  {ok, Conn} = eredis_sentinel:connect_eredis_sync(test, 0, ?GOOD_SENTINEL),
  {ok, <<"PONG">>} = eredis_sync:q(Conn, ["PING"]),
  eredis_sync:close(Conn).

connect_eredis_sync_sentinel_with_redis_params_ok(_Config) ->
  {ok, Conn} = eredis_sentinel:connect_eredis_sync(mymaster, [0], ?GOOD_SENTINEL),
  {ok, <<"PONG">>} = eredis_sync:q(Conn, ["PING"]).

connect_eredis_sync_sentinel_not_available(_Config) ->
  {error, all_sentinels_down} = eredis_sentinel:connect_eredis_sync(some_master, 0, ?WRONG_SENTINEL).

get_master_master_unknown(_Config) ->
  {error, master_unknown} = eredis_sentinel:get_master(no_such_master, ?GOOD_SENTINEL).

get_master_ok(_Config) ->
  {ok, ["127.0.0.1", 6379]} = eredis_sentinel:get_master(test, ?GOOD_SENTINEL).
