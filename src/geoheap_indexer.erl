-module(geoheap_indexer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {committer, uncommitted=0}).
-define(COMMIT_INTERVAL, 1500).
-define(COMMIT_MAXCOUNT, 100).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, put/1]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, Document}, State=#state{uncommitted=C}) ->
    esolr:add([geoheap_util:bson_to_solr(Document)]),
    State1 = case C >= ?COMMIT_MAXCOUNT of
                 true ->
                     esolr:commit(),
                     State#state{uncommitted=0};
                 false ->
                     State#state{uncommitted=C+1}
             end,
    {noreply, bump_commit(State1)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(commit, State) ->
    esolr:commit(),
    {noreply, State#state{committer=undefined, uncommitted=0}};
     
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% bump a commit timer
bump_commit(State=#state{committer=undefined}) ->
    State#state{committer=erlang:send_after(?COMMIT_INTERVAL, self(), commit)};
bump_commit(State=#state{committer=C}) ->
    erlang:cancel_timer(C),
    bump_commit(State#state{committer=undefined}).
