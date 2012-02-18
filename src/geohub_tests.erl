
-module(geohub_tests).

-include_lib("eunit/include/eunit.hrl").

app_test() ->
    geohub:start(),

    ?assert(whereis(geohub_sup) =/= undefined).

store_ok_test() ->
    geohub:start(),

    geohub_store:put(test, {value, <<"bar">>}),
    ?assertEqual(pong, geohub_store:ping()).


util_test() ->
    ?assertEqual({foo, bar}, geohub_util:proplist_to_bson([{foo, bar}])),
    ?assertEqual({foo, [1,2,3]}, geohub_util:proplist_to_bson([{foo, [1,2,3]}])),
    ?assertEqual({foo, [1,2,3], a, b}, geohub_util:proplist_to_bson([{foo, [1,2,3]}, {a,b}])),
    ?assertEqual({foo, bar, bar, baz}, geohub_util:proplist_to_bson([{foo, bar}, {bar, baz}])),

    ?assertEqual({foo, <<"bar">>}, geohub_util:json_to_bson(<<"{\"foo\": \"bar\"}">>)),
    ?assertEqual({foo, <<"bar">>, a, 123}, geohub_util:json_to_bson(<<"{\"foo\": \"bar\", \"a\": 123}">>)),

    ?assertEqual({list, [1,2,3]}, geohub_util:json_to_bson(<<"{\"list\": [1,2,3]}">>)),
    
    ok.
    
