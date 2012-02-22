%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-18

%% @doc Geohub: supervisor

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

-module(geoheap_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    %% Dispatch = [
    %%             {'_', [
    %%                    {['...'], cowboy_http_static, [{directory, {priv_dir, cowboy, []}}, {etag, default}]}
    %%                   ]}
    %%            ],
    %% cowboy:start_listener(my_http_listener, 100,
    %%                       cowboy_tcp_transport, [{port, 8080}],
    %%                       cowboy_http_protocol, [{dispatch, Dispatch}]
    %%                      ),
    
    All = [
           ?CHILD(geoheap_store, worker),
           ?CHILD(geoheap_indexer, worker),
           ?CHILD(geoheap_twitter, worker)
          ],
    {ok, { {one_for_one, 5, 10}, All} }.

