-module(geohub_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    geohub_sup:start_link().

stop(_State) ->
    ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = application:start(geohub),
    ?assertNot(undefined == whereis(geohub_sup)).

-endif.
