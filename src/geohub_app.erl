-module(geohub_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = application:start(lager),
    ok = application:start(mongodb),
    geohub_sup:start_link().

stop(_State) ->
    ok.

