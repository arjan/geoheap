%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-03-01

%% @doc Geoheap: verbeter de buurt periodic poller.

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

-module(geoheap_vbdb).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(POLL_INTERVAL, 600*1000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    lager:info("Verbeter de buurt: starting"),
    timer:send_interval(?POLL_INTERVAL, poll),
    {ok, _StatzId} = statz:new(?MODULE),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_Ref, HttpResponse}}, State) ->
    {{_, 200, _}, _Headers, XML} = HttpResponse,
    All = geoheap_util:docs_from_vbdbxml(XML),
    [begin
         geoheap_store:put(geoheap, Doc),
         geoheap_indexer:put(Doc),
         statz:incr(?MODULE)
     end || Doc <- All],
    %%lager:info("vbdb: updated ~p.~n", [length(All)]),
    {noreply, State};

handle_info(poll, State) ->
    {ok, Url} = application:get_env(geoheap, vbdb_rss_url),
    {ok,_RequestId} = httpc:request(get,{Url, []}, [], [{sync,false}]),
    {noreply, State};

handle_info(_Info, State) ->
    lager:info("vbdb: unhandled message ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

