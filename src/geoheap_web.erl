%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-24

%% @doc Simple web server

%% Copyright 2012 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(geoheap_web).
-include("../include/geoheap.hrl").

-export([setup/0,
         geoquery/1,
         item/1]).

setup() ->
    Dispatch = [
                {'_', [
                       {[], geoheap_web_index, []},

                       {[<<"instagram">>, <<"subscribe">>], epush_subscription_handler, 
                        [{callback, fun geoheap_instagram:subscription_callback/1}]},
                        
                       {[<<"query">>], geoheap_web_json, [{callback, fun geoheap_web:geoquery/1}, {return, raw}]},
                       {[<<"item">>], geoheap_web_json, [{callback, fun geoheap_web:item/1}]},
                       {['...'], cowboy_http_static, [{directory, "priv/www"}, {mimetypes, mime()}]}
                      ]}
               ],
    cowboy:start_listener(my_http_listener, 100,
                          cowboy_tcp_transport, [{port, 8080}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ).

mime() ->
    [
     {<<".html">>, [<<"text/html">>]},
     {<<".css">>, [<<"text/css">>]},
     {<<".png">>, [<<"image/png">>]},
     {<<".gif">>, [<<"image/gif">>]},
     {<<".jpg">>, [<<"image/jpeg">>]},
     {<<".js">>, [<<"application/javascript">>]}
    ].


geoquery(Req) ->
    {All, Req1} = cowboy_http_req:qs_vals(Req),
    From = proplists:get_value(<<"from">>, All),
    To = proplists:get_value(<<"to">>, All),

    FacetOptions = case proplists:get_value(<<"facet">>, All) of
                       <<"true">> ->
                           Start = proplists:get_value(<<"start">>, All),
                           End = proplists:get_value(<<"end">>, All),
                           [{facet_date, "{!ex=d}date", Start, End, "+1HOUR"},
                            {raw, ["facet=on&facet.field=geohash_8&f.geohash_8.facet.mincount=1&f.geohash_8.facet.method=enum&f.geohash_8.facet.limit=2000"]}
                            ];
                       _ ->
                           []
                   end,
    Query = case proplists:get_value(<<"q">>, All, <<>>) of
                <<>> -> ["*:*"];
                Q ->  Q
            end,

    FQ = ["{!tag=d}date:[", From, " TO ", To, "]"],

    Lat = proplists:get_value(<<"lat">>, All),
    Lon = proplists:get_value(<<"lon">>, All),
    D = proplists:get_value(<<"radius">>, All),
    Raw = ["sfield=location&pt=", Lat, ",", Lon, "&d=", D],

    %%{ok, Opts, Docs, Extra} = esolr:search(Query, [{facet_date, "date", Start, End, "+1HOUR"}, {return, raw}]),
    {ok, RawResponse} = esolr:search(Query, [{fields, "id,location,source"},
                                             {sort, [{date, desc}]},
                                             {rows, 2000},
                                             {fq, FQ}, 
                                             {fq, "{!bbox}"}, 
                                             {raw, Raw}, 
                                             {return, raw}] ++ FacetOptions),
    {RawResponse, Req1}.


%% @doc Serve a single item from the store as JSON.
item(Req) ->
    {Id, Req1} = cowboy_http_req:qs_val(<<"id">>, Req),
    {ok, Document} = geoheap_store:lookup(geoheap, Id),
    {geoheap_util:bson_to_json(Document), Req1}.
