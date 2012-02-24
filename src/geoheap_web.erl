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

-export([setup/0]).

setup() ->
    Dispatch = [
                {'_', [
                       {[], geoheap_web_index, []},
                       {[<<"json">>], geoheap_web_json, [{callback, fun(Req) -> {{array, [1,2,3]}, Req} end}]},
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
     {<<".js">>, [<<"application/javascript">>]}
    ].
