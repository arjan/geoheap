%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-24

%% @doc Geoheap: JSON request handler

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

-module(geoheap_web_json).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-record(state, {callback, return}).

init({_Any, http}, Req, Args) ->
    {callback, Callback} = proplists:lookup(callback, Args),
    Return = proplists:get_value(return, Args, json),
    {ok, Req, #state{callback=Callback, return=Return}}.

handle(Req, State=#state{callback=Callback, return=Return}) ->
    {Response, Req2} = Callback(Req),
    Body = case Return of
               json -> mochijson:encode(Response);
               raw -> Response
           end,
    {ok, Req3} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], Body, Req2),
    {ok, Req3, State}.

terminate(_Req, _State) ->
    ok.
