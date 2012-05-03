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

-record(state, {request_id, statz_id, backoff=1}).

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


handle_info({http, {R, {{_, 420, _}, _H, _Body}}}, State=#state{request_id=R, backoff=B}) ->
    {noreply, reconnect(State#state{backoff=2*B})};

handle_info({http, {_, stream_start, _Headers}}, State) ->
    {noreply, State};

handle_info({http, {R, stream, <<"\n">>}}, State=#state{request_id=R}) ->
    %% Twitter keepalive
    {noreply, State};

handle_info({http, {R, stream, <<"{",_/binary>>=Content}}, State=#state{request_id=R}) ->
    try
        BSON = geoheap_util:json_to_bson(mochijson:decode(Content)),
        Doc = geoheap_util:doc_from_tweet(BSON),
        geoheap_store:put(geoheap, Doc),
        geoheap_indexer:put(Doc),
        statz:incr(?MODULE)
    catch
        _:{badmatch,{}} -> nop;
        _:E ->
            lager:error("~p: ~p~n", [E, Content]),
            ok
    end,
    {noreply, State#state{backoff=1}}; % mark success

handle_info({http, {R, stream_end, _Headers}}, State=#state{request_id=R, backoff=B}) ->
    lager:info("Stream end. Retrying after a while..."),
    timer:sleep(B*10),
    {noreply, reconnect(State#state{backoff=2*B})};

handle_info({http, {_, stream_end, _Headers}}, State) ->
    %% stream_end of previous stream, we're reconnecting
    {noreply, State};

handle_info({http,{R,{error,Reason}}}, State=#state{request_id=R, backoff=B}) ->
    lager:info("twitter: Error ~p, restarting~n", [Reason]),
    {noreply, reconnect(State#state{backoff=2*B})};


handle_info(reconnect, State) ->
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

reconnect(State=#state{request_id=R}) when R =/= undefined ->
    httpc:cancel_request(R),
    reconnect(State#state{request_id=undefined});

reconnect(State=#state{backoff=B}) ->
    lager:info("twitter: Reconnect waiting ~p secs...", [B]),
    timer:sleep(1000*B),
    lager:info("twitter: Reconnect"),

    {ok, Login} = application:get_env(geoheap, twitter_username),
    {ok, Password} = application:get_env(geoheap, twitter_password),
    {ok, Body} = application:get_env(geoheap, twitter_apifilter),

    URL = "https://" ++ Login ++ ":" ++ Password ++ "@stream.twitter.com/1/statuses/filter.json",
    {ok, RequestId} = httpc:request(post,
                                    {URL, [], "application/x-www-form-urlencoded", Body},
                                    [],
                                    [{sync, false},
                                     {stream, self}]),

    erlang:send_after(7200*1000, self(), reconnect),
    State#state{request_id=RequestId}.
