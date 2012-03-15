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
-define(API_URL, "https://api.instagram.com/v1/").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, 
         subscribe/0,
         subscribe/3,
         fetch/1,
         unsubscribe_all/0,
         subscription_callback/1]).

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


%% @doc Remove all subscriptions.
unsubscribe_all() ->
    {ok, ClientId} = application:get_env(geoheap, instagram_client_id),
    {ok, Secret} = application:get_env(geoheap, instagram_client_secret),
    Url = ?API_URL ++ "subscriptions?object=all&client_secret=" ++ Secret ++ "&client_id=" ++ ClientId,
    httpc:request(delete, {Url, []}, [], []).


subscribe() ->
    subscribe(4.913635, 52.376829, 5000). %% amsterdam

%% @doc Subscribe to a location.
subscribe(Long, Lat, Radius) ->
    {ok, BaseURL} = application:get_env(geoheap, base_url),
    {ok, ClientId} = application:get_env(geoheap, instagram_client_id),
    {ok, Secret} = application:get_env(geoheap, instagram_client_secret),
    SubscribeArgs = [{client_id, ClientId},
                     {client_secret, Secret},
                     {object, "geography"},
                     {aspect, "media"},
                     {lat, lists:flatten(io_lib:format("~.7f", [Lat]))},
                     {lng, lists:flatten(io_lib:format("~.7f", [Long]))},
                     {radius, integer_to_list(Radius)}],

    epush_client:subscribe(?API_URL ++ "subscriptions", BaseURL ++ "instagram/subscribe", SubscribeArgs).

%% @doc Called by the pubsubhubub handler when there is data for us.
subscription_callback(Body) ->
    gen_server:cast(?SERVER, {subscription_callback, Body}).

%% @doc fetch recent media for given object id
fetch(ObjectId) ->
    gen_server:call(?SERVER, {fetch, ObjectId}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    %% Setup subscriptions according to application environment
    unsubscribe_all(),
    {ok, Locations} = application:get_env(geoheap, instagram_locations),
    spawn(fun() ->
                  [subscribe(Long, Lat, Radius) ||
                      {Long, Lat, Radius} <- Locations]
          end),
    {ok, _StatzId} = statz:new(?MODULE),
    {ok, Args}.

handle_call({fetch, ObjectId}, _From, State) ->
    do_fetchitems(ObjectId),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({subscription_callback, Body}, State) ->
    {array, Handlers} = mochijson:decode(Body),
    [begin 
         {"object_id", ObjectId} = proplists:lookup("object_id", Handler),
         do_fetchitems(ObjectId) 
     end || {struct, Handler} <- Handlers],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_Ref, HttpResponse}}, State) ->
    {{_, 200, _}, _Headers, Body} = HttpResponse,
    {struct, P} = mochijson:decode(Body),
    {array, All} = proplists:get_value("data", P),
    [begin
         BSON = geoheap_util:json_to_bson(J),
         Doc = geoheap_util:doc_from_instagram(BSON),
         geoheap_store:put(geoheap, Doc),
         geoheap_indexer:put(Doc),
         statz:incr(?MODULE)
     end || J <- All],
    %%lager:info("Instagram: updated ~p.~n", [length(All)]),
    {noreply, State};

handle_info(Info, State) ->
    lager:info("instagram: Unhandled info message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_fetchitems(ObjectId) ->
    %% Schiet request in 
    {ok, ClientId} = application:get_env(geoheap, instagram_client_id),
    Url = ?API_URL ++ "geographies/" ++ ObjectId ++ "/media/recent?client_id=" ++ ClientId,
    {ok,_RequestId} = httpc:request(get,{Url, []}, [], [{sync,false}]),
    ok.
    
