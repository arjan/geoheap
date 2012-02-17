
-module(geohub_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = application:start(geohub),
    ?assert(whereis(geohub_sup) =/= undefined).
