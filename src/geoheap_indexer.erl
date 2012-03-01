-module(geoheap_indexer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("../include/geoheap.hrl").

-record(state, {committer, pending=[]}).
-define(COMMIT_INTERVAL, 1500).
-define(COMMIT_MAXCOUNT, 2000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, put/1, reindex_all/0]).

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

reindex_all() ->
    {ok, Cursor} = geoheap_store:all(geoheap),
    reindex(Cursor, mongo_cursor:next(Cursor)).

reindex(_C, {}) ->
    ok;
reindex(Cursor, {Doc}) ->
    ?MODULE:put(Doc),
    reindex(Cursor, mongo_cursor:next(Cursor)).

        

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, Document}, State=#state{pending=P}) ->
    State1 = State#state{pending=[geoheap_util:bson_to_solr(Document)|P]},
    State2 = case length(State1#state.pending) >= ?COMMIT_MAXCOUNT of
                 true ->
                     commit(State1);
                 false ->
                     State1
             end,
    {noreply, bump_commit(State2)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(commit, State) ->
    {noreply, commit(State)};
     
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

commit(State=#state{pending=Pending}) ->
    esolr:add(Pending),
    esolr:commit(),
    State#state{pending=[]}.

%% bump a commit timer
bump_commit(State=#state{committer=undefined}) ->
    State#state{committer=erlang:send_after(?COMMIT_INTERVAL, self(), commit)};
bump_commit(State=#state{committer=C}) ->
    erlang:cancel_timer(C),
    bump_commit(State#state{committer=undefined}).
