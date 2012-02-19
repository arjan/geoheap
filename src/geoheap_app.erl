-module(geoheap_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(geoheap).

start(_StartType, _StartArgs) ->
    geoheap_sup:start_link().

stop(_State) ->
    ok.

