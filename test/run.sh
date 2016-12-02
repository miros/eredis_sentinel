redis-sentinel test/sentinel.conf &
SENTINEL_PID=$!
./rebar3 ct skip_deps=true verbose=3
kill -s KILL $SENTINEL_PID
