
-module(geoheap_tests).

-include_lib("../include/geoheap.hrl").
-include_lib("eunit/include/eunit.hrl").

app_test() ->
    geoheap:start(),

    ?assert(whereis(geoheap_sup) =/= undefined).

store_ok_test() ->
    geoheap:start(),

    geoheap_store:put(test, {value, <<"bar">>}),
    ?assertEqual(pong, geoheap_store:ping()).


util_test() ->
    ?assertEqual({foo, bar}, geoheap_util:proplist_to_bson([{foo, bar}])),
    ?assertEqual({foo, [1,2,3]}, geoheap_util:proplist_to_bson([{foo, [1,2,3]}])),
    ?assertEqual({foo, [1,2,3], a, b}, geoheap_util:proplist_to_bson([{foo, [1,2,3]}, {a,b}])),
    ?assertEqual({foo, bar, bar, baz}, geoheap_util:proplist_to_bson([{foo, bar}, {bar, baz}])),

    ?assertEqual({foo, <<"bar">>}, geoheap_util:json_to_bson(<<"{\"foo\": \"bar\"}">>)),
    ?assertEqual({foo, <<"bar">>, a, 123}, geoheap_util:json_to_bson(<<"{\"foo\": \"bar\", \"a\": 123}">>)),

    ?assertEqual({list, [1,2,3]}, geoheap_util:json_to_bson(<<"{\"list\": [1,2,3]}">>)),
    
    ok.

unicode_test() ->
    %% lager:info("~p~n", [xmerl_ucs:from_utf8("øfßðáfö³²’¥‘¤ë€í³’‘íëfßœðï¶fœøßð¶gø")]),

    %% X = xmerl_ucs:from_utf8("øfßðáfö³²’¥‘¤ë€í³’‘íëfßœðï¶fœøßð¶gø"),
    %% lager:info("~p~n", [unicode:characters_to_binary(X)]),
    %%X2 = [73,32,102,101,101,108,32,109,97,100,32,115,105,99,107,32,55357,56862],
    %%lager:info("~p~n", [unicode:characters_to_binary(X2)]),

    ?DEBUG(geoheap_util:to_utf8([73,32,102,101,101,108,32,109,97,100,32,115,105,99,107,32,55357,56862])),
    ok.
    %%    [73,32,102,101,101,108,32,109,97,100,32,115,105,99,107,32,55357,56862]
                                                %[87,97,107,105,110,103,32,117,112,32,116,111,32,114,97,110,100,111,109,32,116,101,120,116,32,109,101,115,115,97,103,101,115,32,38,32,118,111,105,99,101,109,97,105,108,115,32,102,114,111,109,32,109,121,32,98,105,103,32,104,101,97,100,32,38,103,116,59,38,103,116,59,38,103,116,59,38,103,116,59,38,103,116,59,38,103,116,59,32,55357,56856,55357,56845,55357,56841,55357,56858,10084,55357,56397]
%[73,32,102,101,101,108,32,109,97,100,32,115,105,99,107,32,55357,56862]
