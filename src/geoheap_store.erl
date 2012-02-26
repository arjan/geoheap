%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-18

%% @doc Geohub: store

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

-module(geoheap_store).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

%% interface functions
-export([
         ping/0,
         put/2
]).

-record(state, {db, conn}).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ping() ->
    gen_server:call(?MODULE, ping).

-spec put(atom(), any()) -> {ok, Id::integer()}.
put(Collection, Document) ->
    gen_server:call(?MODULE, {put, Collection, Document}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init(_Args) ->
    lager:info("~p started", [?MODULE]),
    Host = {localhost, 27017},
    {ok, Conn} = mongo:connect (Host),
    {ok, #state{conn=Conn, db=test}}.


%% @doc Trap unknown calls
handle_call(ping, _From, State) ->
    {reply, pong, State};


%% @doc Trap unknown calls
handle_call({put, Collection, Document}, _From, State) ->
    {ok, {Id}} = mongo:do(safe, master, State#state.conn, State#state.db,
                 fun() ->
                         mongo:insert(Collection, Document)
                 end),
    {reply, {ok, Id}, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.



%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to terminate.
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

