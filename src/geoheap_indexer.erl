-module(geoheap_indexer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("../include/geoheap.hrl").

-record(state, {adder, pending=[]}).
-define(COMMIT_INTERVAL, 1000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, put/1, put_sync/1, reindex_all/0]).

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

put(Document) ->
    gen_server:cast(?SERVER, {put, Document}),
    ok.

put_sync(Document) ->
    gen_server:call(?SERVER, {put, Document}).

reindex_all() ->
    {ok, Cursor} = geoheap_store:all(geoheap),
    reindex(Cursor, mongo_cursor:next(Cursor), 0, 0).

reindex(C, D, 10000, N)->
    lager:info("Num done: ~p~n", [N]),
    timer:sleep(1000),
    reindex(C, D, 0, N);
reindex(_C, {}, _C, N) ->
    lager:info("Reindex finished: ~p~n", [N]),
    ok;
reindex(Cursor, {Doc}, B, N) ->
    put_sync(bson:exclude(['_id'], Doc)),
    reindex(Cursor, mongo_cursor:next(Cursor), B+1, N+1).

        

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    esolr:set_auto_commit({time, 10000}),
    {ok, #state{}}.

handle_call({put, Document}, _From, State=#state{pending=P}) ->
    State1 = State#state{pending=[geoheap_util:bson_to_solr(Document)|P]},
    {reply, ok, bump_add(State1)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({put, Document}, State=#state{pending=P}) ->
    State1 = State#state{pending=[geoheap_util:bson_to_solr(Document)|P]},
    {noreply, bump_add(State1)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(add, State) ->
    {noreply, do_add(State)};
     
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_add(State=#state{pending=Pending}) ->
    lager:info("Adding ~p docs~n", [length(Pending)]),
    esolr:add(Pending),
    State#state{pending=[], adder=undefined}.

%% bump a commit timer
bump_add(State=#state{adder=undefined}) ->
    State#state{adder=erlang:send_after(?COMMIT_INTERVAL, self(), add)};
bump_add(State) ->
    State.
