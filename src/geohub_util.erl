%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-18

%% @doc Geohub: utilities

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

-module(geohub_util).

-export([proplist_to_bson/1,
         json_to_bson/1
        ]).

proplist_to_bson(List) ->
    list_to_tuple(
      lists:foldr(fun({K,V}, Acc) -> [K, V | Acc] end, [], List)).

json_to_bson(Json) ->
    decode_json(mochijson:decode(Json)).

decode_json({struct, KeyValues}) ->
    P = [{list_to_atom(K), decode_json(V)} || {K,V} <- KeyValues],
    proplist_to_bson(P);

decode_json({array, List}) ->
    [decode_json(V) || V <- List];

decode_json(V) when is_integer(V); is_atom(V); is_float(V); is_binary(V) ->
    V;

decode_json(L) when is_list(L) ->
    bson:utf8(L);
decode_json(R) ->
    lager:error("Unknown json decode: ~p", [R]).



    
