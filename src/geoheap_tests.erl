
-module(geoheap_tests).

-include_lib("../include/geoheap.hrl").
-include_lib("eunit/include/eunit.hrl").

start() -> 
    application:set_env(geoheap, twitter_username, "acscherp2"),
    application:set_env(geoheap, twitter_password, "aapaap"),
    geoheap:start().


app_xtest() ->
    start(),
    ?assert(whereis(geoheap_sup) =/= undefined).

store_ok_xtest() ->
    start(),
    geoheap_store:put(test, {value, <<"bar">>}),
    ?assertEqual(pong, geoheap_store:ping()).


util_test() ->
    ?assertEqual({foo, bar}, geoheap_util:proplist_to_bson([{foo, bar}])),
    ?assertEqual({foo, [1,2,3]}, geoheap_util:proplist_to_bson([{foo, [1,2,3]}])),
    ?assertEqual({foo, [1,2,3], a, b}, geoheap_util:proplist_to_bson([{foo, [1,2,3]}, {a,b}])),
    ?assertEqual({foo, bar, bar, baz}, geoheap_util:proplist_to_bson([{foo, bar}, {bar, baz}])),

    ?assertEqual({foo, <<"bar">>}, geoheap_util:json_to_bson(mochijson:decode(<<"{\"foo\": \"bar\"}">>))),
    ?assertEqual({foo, <<"bar">>, a, 123}, geoheap_util:json_to_bson(mochijson:decode(<<"{\"foo\": \"bar\", \"a\": 123}">>))),

    ?assertEqual({list, [1,2,3]}, geoheap_util:json_to_bson(mochijson:decode(<<"{\"list\": [1,2,3]}">>))),
    
    ok.

util2_test() ->
    ?assertEqual({doc, [{id, <<"foo">>}]}, geoheap_util:bson_to_solr({id, <<"foo">>})).

tweet_test() ->
    Tweet = {in_reply_to_user_id_str,<<"158314798">>,geo,null,text,<<"@Real_Liam_Payne I love you so, please follow the102">>,retweeted,false,in_reply_to_screen_name,<<"Real_Liam_Payne">>,in_reply_to_user_id,158314798,in_reply_to_status_id,null,contributors,null,truncated,false,source,<<"web">>,created_at,<<"Tue Feb 21 15:34:43 +0000 2012">>,entities,{user_mentions,[{indices,[0,16],screen_name,<<"Real_Liam_Payne">>,id_str,<<"158314798">>,name,<<"Liam Payne">>,id,158314798}],hashtags,[],urls,[]},retweet_count,0,id_str,<<"171981182903394304">>,place,{place_type,<<"country">>,url,<<"http://api.twitter.com/1/geo/id/682c5a667856ef42.json">>,country_code,<<"TR">>,attributes,{},full_name,<<"Turkey">>,name,<<"Turkey">>,id,<<"682c5a667856ef42">>,country,<<"Turkey">>,bounding_box,{type,<<"Polygon">>,coordinates,[[[25.663883,35.817497],[44.822762,35.817497],[44.822762,42.109993],[25.663883,42.109993]]]}},coordinates,null,in_reply_to_status_id_str,null,user,{favourites_count,13,profile_background_color,<<"141113">>,profile_background_tile,true,profile_background_image_url_https,<<"https://si0.twimg.com/profile_background_images/420541079/cats.jpg">>,profile_sidebar_fill_color,<<"efefef">>,profile_image_url_https,<<"https://si0.twimg.com/profile_images/1822030035/li_normal.jpg">>,contributors_enabled,false,description,<<"Directioner:) LiamPayneHarryStylesLouisTomlinsonZaynMalikNiallHoran OneDirection ">>,listed_count,0,profile_sidebar_border_color,<<"eeeeee">>,is_translator,false,notifications,null,created_at,<<"Sun Jan 08 07:24:22 +0000 2012">>,friends_count,574,screen_name,<<"GozdeOzezgi">>,default_profile,false,show_all_inline_media,false,lang,<<"tr">>,profile_use_background_image,true,url,null,id_str,<<"458157278">>,follow_request_sent,null,statuses_count,243,profile_text_color,<<"333333">>,protected,false,profile_background_image_url,<<"http://a2.twimg.com/profile_background_images/420541079/cats.jpg">>,time_zone,<<"Greenland">>,followers_count,165,profile_image_url,<<"http://a0.twimg.com/profile_images/1822030035/li_normal.jpg">>,name,<<71,195,182,122,100,101,32,40,79,110,101,68,105,114,101,99,116,105,111,110,41>>,following,null,geo_enabled,true,profile_link_color,<<"009999">>,location,<<>>,id,458157278,default_profile_image,false,verified,false,utc_offset,-10800},id,171981182903394304,favorited,false},
    Doc = geoheap_util:doc_from_tweet(Tweet),
    ?assertEqual({<<"twitter">>}, bson:lookup(source, Doc)),
    ?assertEqual({<<"171981182903394304">>}, bson:lookup(id, Doc)),
    ?assertEqual({<<"@Real_Liam_Payne I love you so, please follow the102">>}, bson:lookup(text, Doc)),
    ?assertEqual({Tweet}, bson:lookup(original, Doc)),
    %Tue Feb 21 15:34:43 +0000 2012
    ?assertEqual({<<"2012-02-21T15:34:43Z">>}, bson:lookup(date, Doc)),

    Tweet2 = {in_reply_to_user_id_str,null,text,<<"Im at My House (Erie, PA) http://t.co/pRstTqLB">>,geo,{type,<<"Point">>,coordinates,[42.13137262,-80.07763525]},in_reply_to_screen_name,null,in_reply_to_user_id,null,favorited,false,contributors,null,retweeted,false,possibly_sensitive_editable,true,retweet_count,0,created_at,<<"Tue Feb 21 21:21:20 +0000 2012">>,id_str,<<"172068410069487616">>,in_reply_to_status_id,null,entities,{user_mentions,[],hashtags,[],urls,[{indices,[27,47],display_url,<<"4sq.com/w60dil">>,url,<<"http://t.co/pRstTqLB">>,expanded_url,<<"http://4sq.com/w60dil">>}]},in_reply_to_status_id_str,null,place,{bounding_box,{type,<<"Polygon">>,coordinates,[[[-80.153583,42.077238],[-80.003376,42.077238],[-80.003376,42.173707],[-80.153583,42.173707]]]},country,<<"United States">>,attributes,{},full_name,<<"Erie, PA">>,url,<<"http://api.twitter.com/1/geo/id/29aaa88d9fe74b50.json">>,country_code,<<"US">>,name,<<"Erie">>,id,<<"29aaa88d9fe74b50">>,place_type,<<"city">>},coordinates,{type,<<"Point">>,coordinates,[-80.07763525,42.13137262]},user,{show_all_inline_media,false,following,null,profile_background_color,<<"642D8B">>,profile_image_url_https,<<"https://si0.twimg.com/profile_images/1127627222/newme_normal.jpg">>,verified,false,profile_background_tile,true,listed_count,0,time_zone,<<"Eastern Time (US & Canada)">>,profile_sidebar_fill_color,<<"7AC3EE">>,is_translator,false,geo_enabled,true,friends_count,63,followers_count,19,profile_image_url,<<"http://a3.twimg.com/profile_images/1127627222/newme_normal.jpg">>,description,<<>>,default_profile,false,profile_sidebar_border_color,<<"65B0DA">>,follow_request_sent,null,statuses_count,2097,notifications,null,profile_use_background_image,true,created_at,<<"Fri Apr 17 13:39:19 +0000 2009">>,screen_name,<<"skatkat98">>,id_str,<<"32407157">>,profile_text_color,<<"3D1957">>,protected,false,url,<<"http://www.myspace.com/thisismyplace">>,default_profile_image,false,contributors_enabled,false,lang,<<"en">>,profile_background_image_url,<<"http://a1.twimg.com/images/themes/theme10/bg.gif">>,name,<<"Kat Strohmeyer ">>,favourites_count,1,profile_link_color,<<"FF0000">>,id,32407157,profile_background_image_url_https,<<"https://si0.twimg.com/images/themes/theme10/bg.gif">>,utc_offset,-18000,location,<<"Erie, PA">>},truncated,false,id,172068410069487616,possibly_sensitive,false},

    Doc2 = geoheap_util:doc_from_tweet(Tweet2),
    ?assertEqual({[42.13137262,-80.07763525]}, bson:lookup(location, Doc2)),
    ?assertEqual({<<"skatkat98">>}, bson:lookup(screenname, Doc2)),

    {doc, Solr} = geoheap_util:bson_to_solr(Doc2),

    ?assertEqual("1d", proplists:get_value(geohash, Solr)),
    ok.

instagram_test() ->
    JSON = "{\"tags\":[\"foo\",\"bar\"],\"location\":{\"latitude\":35.646458072,\"name\":\"\u6075\u6bd4\u5bff\",\"longitude\":139.710001945,\"id\":5387540},\"comments\":{\"count\":0,\"data\":[]},\"filter\":\"Rise\",\"created_time\":\"1330205190\",\"link\":\"http:\/\/instagr.am\/p\/HcZSlJwChB\/\",\"likes\":{\"count\":0,\"data\":[]},\"images\":{\"low_resolution\":{\"url\":\"http:\/\/distilleryimage1.instagram.com\/623df26a5ff711e19896123138142014_6.jpg\",\"width\":306,\"height\":306},\"thumbnail\":{\"url\":\"http:\/\/distilleryimage1.instagram.com\/623df26a5ff711e19896123138142014_5.jpg\",\"width\":150,\"height\":150},\"standard_resolution\":{\"url\":\"http:\/\/distilleryimage1.instagram.com\/623df26a5ff711e19896123138142014_7.jpg\",\"width\":612,\"height\":612}},\"caption\":{\"text\":\"Bleh\"},\"type\":\"image\",\"id\":\"134093316919666753_864105\",\"user\":{\"username\":\"source\",\"website\":\"\",\"bio\":\"dance as you want, all through the darkest night.\u00a02012.02.10(fri)\u00a0tanze@solfa\u00a0GENRE:TECHNO\/HOUSE\u00a0OPEN:22:00~\",\"profile_picture\":\"http:\/\/images.instagram.com\/profiles\/profile_864105_75sq_1327201896.jpg\",\"full_name\":\"tomohiro\",\"id\":\"864105\"}}",
    Doc = geoheap_util:doc_from_instagram(geoheap_util:json_to_bson(mochijson:decode(JSON))),

    ?assertEqual({<<"http://distilleryimage1.instagram.com/623df26a5ff711e19896123138142014_5.jpg">>}, bson:lookup(thumbnail, Doc)),
    ?assertEqual({<<"Bleh">>}, bson:lookup(text, Doc)),
    ?assertEqual({<<"foo">>}, bson:lookup(tags, Doc)),

    ok.


verbeterdebuurt_test() ->
    XML = <<"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
 <rss version=\"2.0\" 
      xmlns:geo=\"http://www.w3.org/2003/01/geo/wgs84_pos#\">
  <channel>
     <title>Verbeterdebuurt meldingen in Amsterdam</title>
     <description>Recente openstaande meldingen op verbeterdebuurt.nl</description>
     <link>http://www.verbeterdebuurt.nl/</link>
     <item>
       <title>probleem : rijstroken rotonde (Baden Powellweg, Osdorp-Midden)</title>
       <updated>29 februari 2012 16:29:42 CET</updated>
       <description><![CDATA[ Drie van de vier wegen die op de rotonde uitkomen, bevatten twee rijstroken met een verplichte voorsortering. Hierdoor wordt een niet onaanzienlijk voordeel van de rotonde teniet gedaan: b.v. vanaf de B.Powellweg gaat het meeste verkeer linksaf (Plesmanlaan, oostelijk) en rechtdoor (Vrije Geer) en daar is maar 1 rijstrook voor beschikbaar. Beide rijstroken geschikt maken voor rechtdoor, zou het gebruik van de rotonde optimaliseren. ]]> </description>
       <votes>0</votes>
       <link>http://www.verbeterdebuurt.nl/melding/21496/rijstroken-rotonde</link>
       <geo:lat>52.347713470459</geo:lat>
       <geo:long>4.7981286048889</geo:long>
     </item></channel></rss>">>,

    [Doc] = geoheap_util:docs_from_vbdbxml(XML),

    ?assertEqual({[52.347713470459,4.7981286048889]}, bson:lookup(location, Doc)),
    ?assertEqual({<<"vbdb">>}, bson:lookup(source, Doc)),
    ?assertEqual({<<"vbdb21496">>}, bson:lookup(id, Doc)),
    ?assertEqual({<<"probleem : rijstroken rotonde (Baden Powellweg, Osdorp-Midden)  Drie van de vier wegen die op de rotonde uitkomen, bevatten twee rijstroken met een verplichte voorsortering. Hierdoor wordt een niet onaanzienlijk voordeel van de rotonde teniet gedaan: b.v. vanaf de B.Powellweg gaat het meeste verkeer linksaf (Plesmanlaan, oostelijk) en rechtdoor (Vrije Geer) en daar is maar 1 rijstrook voor beschikbaar. Beide rijstroken geschikt maken voor rechtdoor, zou het gebruik van de rotonde optimaliseren.  ">>}, bson:lookup(text, Doc)),
    ?assertEqual({<<"2012-02-29T15:29:42Z">>}, bson:lookup(date, Doc)),
    
    ok.
    

bson_to_json_test() ->
    JSON = "{\"tags\":[\"foo\",\"bar\"],\"location\":{\"latitude\":35.646458072,\"name\":\"\u6075\u6bd4\u5bff\",\"longitude\":139.710001945,\"id\":5387540},\"comments\":{\"count\":0,\"data\":[]},\"filter\":\"Rise\",\"created_time\":\"1330205190\",\"link\":\"http:\/\/instagr.am\/p\/HcZSlJwChB\/\",\"likes\":{\"count\":0,\"data\":[]},\"images\":{\"low_resolution\":{\"url\":\"http:\/\/distilleryimage1.instagram.com\/623df26a5ff711e19896123138142014_6.jpg\",\"width\":306,\"height\":306},\"thumbnail\":{\"url\":\"http:\/\/distilleryimage1.instagram.com\/623df26a5ff711e19896123138142014_5.jpg\",\"width\":150,\"height\":150},\"standard_resolution\":{\"url\":\"http:\/\/distilleryimage1.instagram.com\/623df26a5ff711e19896123138142014_7.jpg\",\"width\":612,\"height\":612}},\"caption\":{\"text\":\"Bleh\"},\"type\":\"image\",\"id\":\"134093316919666753_864105\",\"user\":{\"username\":\"source\",\"website\":\"\",\"bio\":\"dance as you want, all through the darkest night.\u00a02012.02.10(fri)\u00a0tanze@solfa\u00a0GENRE:TECHNO\/HOUSE\u00a0OPEN:22:00~\",\"profile_picture\":\"http:\/\/images.instagram.com\/profiles\/profile_864105_75sq_1327201896.jpg\",\"full_name\":\"tomohiro\",\"id\":\"864105\"}}",
    Source = mochijson:decode(JSON),
    geoheap_util:bson_to_json(geoheap_util:json_to_bson(Source)).
    
    
unicode_test() ->
    %% lager:info("~p~n", [xmerl_ucs:from_utf8("øfßðáfö³²’¥‘¤ë€í³’‘íëfßœðï¶fœøßð¶gø")]),

    %% lager:info("~p~n", [unicode:characters_to_binary(X)]),
    %%X2 = [73,32,102,101,101,108,32,109,97,100,32,115,105,99,107,32,55357,56862],
    %%lager:info("~p~n", [unicode:characters_to_binary(X2)]),

    %%?DEBUG(geoheap_util:to_utf8([73,32,102,101,101,108,32,109,97,100,32,115,105,99,107,32,55357,56862])),
    ok.
    %%    [73,32,102,101,101,108,32,109,97,100,32,115,105,99,107,32,55357,56862]
                                                %[87,97,107,105,110,103,32,117,112,32,116,111,32,114,97,110,100,111,109,32,116,101,120,116,32,109,101,115,115,97,103,101,115,32,38,32,118,111,105,99,101,109,97,105,108,115,32,102,114,111,109,32,109,121,32,98,105,103,32,104,101,97,100,32,38,103,116,59,38,103,116,59,38,103,116,59,38,103,116,59,38,103,116,59,38,103,116,59,32,55357,56856,55357,56845,55357,56841,55357,56858,10084,55357,56397]
%[73,32,102,101,101,108,32,109,97,100,32,115,105,99,107,32,55357,56862]
