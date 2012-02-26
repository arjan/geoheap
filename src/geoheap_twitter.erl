%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-21

%% @doc geohub: twitter aggregator.

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

-module(geoheap_twitter).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("../include/geoheap.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {request_id, statz_id}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, StatzId} = statz:new(?MODULE),
    State = #state{statz_id=StatzId},
    {ok, reconnect(State)}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({http, {_, stream_start, _Headers}}, State) ->
    {noreply, State};

handle_info({http, {R, stream, <<"\n">>}}, State=#state{request_id=R}) ->
    %% Twitter keepalive
    {noreply, State};

handle_info({http, {R, stream, <<"{",_/binary>>=Content}}, State=#state{request_id=R}) ->
    try
        BSON = geoheap_util:json_to_bson(mochijson:decode(Content)),
        Doc = geoheap_util:doc_from_tweet(BSON),
        {ok, Id} = geoheap_store:put(geoheap, Doc),
        geoheap_indexer:put(Id, Doc),
        statz:incr(?MODULE)
    catch
        _:{badmatch,{}} -> nop;
        _:E ->
            lager:error("~p: ~p~n", [E, Content]),
            ok
    end,
    {noreply, State};

handle_info({http,{R,{error,socket_closed_remotely}}}, State=#state{request_id=R}) ->
    {noreply, reconnect(State)};

handle_info(Info, State) ->
    lager:info("twitter: Unhandled message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

reconnect(State) ->
    lager:info("twitter: Reconnect."),

    {ok, Login} = application:get_env(geoheap, twitter_username),
    {ok, Password} = application:get_env(geoheap, twitter_password),
    URL = "https://" ++ Login ++ ":" ++ Password ++ "@stream.twitter.com/1/statuses/filter.json",
    %Body = "locations=-180,-90,180,90", % whole world
    %Body = "locations=50.79,3.76,53.38,7.11", % NL
    %Body = "locations=3.762817,50.790195,7.113647,53.388134", % NL
    Body = "locations=3.762817,50.790195,7.113647,53.388134&track=radar", % NL
    %Body = "track=nederland",
    %%Body = "track=instagram",
    %%Body = "locations=-122.75,36.8,-121.75,37.8,-74,40,-73,41", % ny + la
    {ok, RequestId} = httpc:request(post,
                                    {URL, [], "application/x-www-form-urlencoded", Body},
                                    [],
                                    [{sync, false},
                                     {stream, self}]),
    State#state{request_id=RequestId}.
