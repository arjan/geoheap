
-module(geohub_tests).

-include_lib("eunit/include/eunit.hrl").

app_test() ->
    geohub:start(),

    ?assert(whereis(geohub_sup) =/= undefined).

store_ok_test() ->
    geohub:start(),

    ?assertEqual(pong, geohub_store:ping()).
