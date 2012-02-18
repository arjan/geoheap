-module(geohub_twitter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
    Login = "acscherp",
    Pass = "cooljoe8",
    URL = "https://" ++ Login ++ ":" ++ Pass ++ "@stream.twitter.com/1/statuses/filter.json",
    %%Body = "locations=52.333968,4.845314,52.390576,4.943504",
    Body = "locations=-122.75,36.8,-121.75,37.8,-74,40,-73,41",
    {ok, RequestId} = httpc:request(post,
                                    {URL, [], "application/x-www-form-urlencoded", Body},
                                    [],
                                    [{sync, false},
                                     {stream, self}]),
    lager:info("Twitter starting.."),
    {ok, #state{request_id=RequestId, statz_id=StatzId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {R, stream, <<"{",_/binary>>=Content}}, State=#state{request_id=R}) ->
    %%lager:info("~p~n", []),
    statz:incr(?MODULE),
    Doc = {source, twitter, data, geohub_util:json_to_bson(Content)},
    geohub_store:put(twitter, Doc),
    {noreply, State};

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

