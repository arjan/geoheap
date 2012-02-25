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
         geoquery/1]).

setup() ->
    Dispatch = [
                {'_', [
                       {[], geoheap_web_index, []},
                       {[<<"test">>], geoheap_web_json, [{callback, fun(Req) -> {{array, [1,2,3]}, Req} end}]},
                       {[<<"query">>], geoheap_web_json, [{callback, fun geoheap_web:geoquery/1}, {return, raw}]},
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
     {<<".jpg">>, [<<"image/jpeg">>]},
     {<<".js">>, [<<"application/javascript">>]}
    ].


geoquery(Req) ->
    {All, Req1} = cowboy_http_req:qs_vals(Req),
    From = proplists:get_value(<<"from">>, All),
    To = proplists:get_value(<<"to">>, All),
    Start = proplists:get_value(<<"start">>, All),
    End = proplists:get_value(<<"end">>, All),
    Q = proplists:get_value(<<"q">>, All, "*"),
    Query = ["text:",Q],

    FQ = ["{!tag=d}date:[", From, " TO ", To, "]"],

    Lat = proplists:get_value(<<"lat">>, All),
    Lon = proplists:get_value(<<"lon">>, All),
    D = proplists:get_value(<<"radius">>, All),
    Raw = ["sfield=location&pt=", Lat, ",", Lon, "&d=", D],

    %%{ok, Opts, Docs, Extra} = esolr:search(Query, [{facet_date, "date", Start, End, "+1HOUR"}, {return, raw}]),
    {ok, RawResponse} = esolr:search(Query, [
                                             {rows, 2000},
                                             {fq, FQ}, 
                                             {fq, "{!bbox}"}, 
                                             {raw, Raw}, 
                                             {facet_date, "{!ex=d}date", Start, End, "+1HOUR"}, 
                                             {return, raw}]),
    {RawResponse, Req1}.

