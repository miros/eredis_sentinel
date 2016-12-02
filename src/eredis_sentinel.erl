-module(eredis_sentinel).

%% API exports
-export([
  connect_eredis/2,
  connect_eredis/3,
  connect_eredis_sync/2,
  connect_eredis_sync/3,
  get_master/1,
  get_master/2
]).

-type redis_addr() :: list(). % ["127.0.0.1", 6379]

-type master_name() :: binary() | string() | atom().
-type db_num() :: non_neg_integer().

-type redis_conn() :: term().
-type error() :: all_sentinels_down | master_unknown | not_a_master | master_not_available | term().

-type options() :: #{sentinel_addrs => [redis_addr()], timeout => timeout(), redis_client => module()}.

%%====================================================================
%% API functions
%%====================================================================

-spec connect_eredis(master_name(), db_num()) -> {ok, redis_conn()} | {error, error()}.

connect_eredis(MasterName, Db) ->
  connect_eredis(MasterName, Db, #{}).

-spec connect_eredis(master_name(), db_num(), options()) -> {ok, redis_conn()} | {error, error()}.

connect_eredis(MasterName, Db, Options) ->
  connect(MasterName, Db, Options, {eredis, start_link}).

-spec connect_eredis_sync(master_name(), db_num()) -> {ok, redis_conn()} | {error, error()}.

connect_eredis_sync(MasterName, Db) ->
  connect_eredis_sync(MasterName, Db, #{}).

-spec connect_eredis_sync(master_name(), db_num(), options()) -> {ok, redis_conn()} | {error, error()}.

connect_eredis_sync(MasterName, Db, Options) ->
  connect(MasterName, Db, Options, {eredis_sync, connect_db}).

-spec get_master(master_name()) -> {ok, redis_addr()} | {error, error()}.

get_master(MasterName) ->
  get_master(MasterName, #{}).

-spec get_master(master_name(), options()) -> {ok, redis_addr()} |   {error, error()}.

get_master(MasterName, Options) ->
  SentinelAddrs = extract_sentinel_addrs(Options),
  Timeout = maps:get(timeout, Options, get_timeout()),
  RedisClient = maps:get(redis_client, Options, get_redis_client()),

  do_get_master(SentinelAddrs, MasterName, Timeout, RedisClient).

%%====================================================================
%% Internal functions
%%====================================================================

extract_sentinel_addrs(Options) ->
  case maps:get(sentinel_addrs, Options, undefined) of
    undefined ->
      randomize(get_sentinel_addrs());
    Addrs ->
      Addrs
  end.

connect(MasterName, Db, Options, {Module, Method}) ->
  case get_master(MasterName, Options) of
    {ok, [Host, Port]} ->
      erlang:apply(Module, Method, [Host, Port, Db]);
    Error ->
      Error
  end.

do_get_master([CurrentSentinel | OtherSentinels], MasterName, Timeout, RedisClient) ->
  case get_sentinel_master(CurrentSentinel, MasterName, Timeout, RedisClient) of
    {error, {sentinel_error, _}} ->
      do_get_master(OtherSentinels, MasterName, Timeout, RedisClient);
    {error, not_a_master} ->
      get_sentinel_master(CurrentSentinel, MasterName, Timeout, RedisClient);
    Result ->
      Result
  end;
do_get_master([], _MasterName, _Timeout, _RedisClient) ->
  {error, all_sentinels_down}.

get_sentinel_master([SentinelHost, SentinelPort], MasterName, Timeout, RedisClient) ->
  case RedisClient:connect(SentinelHost, SentinelPort) of
    {ok, Conn} ->
      Result = ask_sentinel_master(RedisClient, Conn, MasterName, Timeout),
      RedisClient:close(Conn),
      Result;
    {error, Error} ->
      {error, {sentinel_error, Error}}
  end.

ask_sentinel_master(RedisClient, Conn, MasterName, Timeout) ->
  Response = RedisClient:q(Conn, ["SENTINEL", "get-master-addr-by-name", MasterName], Timeout),
  case parse_master_response(Response) of
    {ok, Addr} ->
      check_master(RedisClient, Addr, Timeout);
    Error ->
      Error
  end.

parse_master_response({ok, [HostBin, PortBin]}) ->
    Host = binary_to_list(HostBin),
    Port = list_to_integer(binary_to_list(PortBin)),
    {ok, [Host, Port]};
parse_master_response({ok, undefined}) ->
    {error, master_unknown};
parse_master_response({error, Error}) ->
    {error, {sentinel_error, Error}}.

check_master(RedisClient, [MasterHost, MasterPort], Timeout) ->
  case RedisClient:connect(MasterHost, MasterPort, Timeout) of
    {ok, Conn} ->
      Result = case is_master(RedisClient, Conn, Timeout) of
        true ->
          {ok, [MasterHost, MasterPort]};
        false ->
          {error, not_a_master}
      end,
      RedisClient:close(Conn),
      Result;
    {error, _} ->
      {error, master_not_available}
  end.

is_master(RedisClient, Conn, Timeout) ->
  case RedisClient:q(Conn, ["ROLE"], Timeout) of
    {ok, [<<"master">> | _]} ->
      true;
    _ ->
      false
  end.

randomize(List) ->
  [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

get_sentinel_addrs() ->
  case application:get_env(?MODULE, sentinel_addrs, undefined) of
    undefined ->
      error(sentinel_addrs_not_configured);
    Addrs ->
      Addrs
  end.

get_redis_client() ->
  application:get_env(?MODULE, redis_client, eredis_sync).

get_timeout() ->
  application:get_env(?MODULE, timeout, 5000).
