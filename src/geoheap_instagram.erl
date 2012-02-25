%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-25

%% @doc Geoheap: instagram integration

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

-module(geoheap_instagram).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(SUBSCRIBE_URL, "https://api.instagram.com/v1/subscriptions/").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, t/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

t() ->
    {ok, BaseURL} = application:get_env(geoheap, base_url),
    {ok, ClientId} = application:get_env(geoheap, instagram_client_id),
    {ok, Secret} = application:get_env(geoheap, instagram_client_secret),
    SubscribeArgs = [{client_id, ClientId},
                     {client_secret, Secret},
                     {object, "geography"},
                     {aspect, "media"},
                     {lat, "35.657872"},
                     {lng, "139.70232"},
                     {radius, "5000"}],

    epush_client:subscribe(?SUBSCRIBE_URL, BaseURL ++ "instagram/subscribe", SubscribeArgs).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

