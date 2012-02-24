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

-module(geoheap_util).
-include("../include/geoheap.hrl").

-export([proplist_to_bson/1,
         json_to_bson/1,
         bson_to_solr/1,
         to_utf8/1,
         doc_from_tweet/1
        ]).

proplist_to_bson(List) ->
    list_to_tuple(
      lists:foldr(fun({K,V}, Acc) -> [K, V | Acc] end, [], List)).

bson_to_solr(Document) ->
    Props1 = bson:fields(Document),
    Location = case proplists:get_value(location, Props1) of
                   undefined -> [];
                   [0,0] -> [];
                   [0.0,0.0] -> [];
                   [X, Y] ->
                       Hash = geohash:encode(X, Y),
                       Hashes = [{geohash, lists:flatten([io_lib:format("~1.16B", [N])
                                                          | lists:sublist(Hash, N)])}
                                 || N <- lists:seq(1,12)],
                       [{location, list_to_binary(lists:flatten(
                                                    io_lib:format("~.5f,~.5f", [X+0.0, Y+0.0])))}|Hashes]
               end,
    Props2 = proplists:delete(original,
                              proplists:delete(location,
                                               Props1)),
    Props3 = Location ++ Props2,
    {doc, Props3}.

json_to_bson(Json) ->
    decode_json(mochijson:decode(Json)).


doc_from_tweet(Tweet) ->
    {Id} = bson:lookup(id_str, Tweet),
    {Text} = bson:lookup(text, Tweet),
    {RawDate} = bson:lookup(created_at, Tweet),
    Date = dh_date:parse(binary_to_list(RawDate)),
    FormattedDate = list_to_binary(dh_date:format("Y-m-d\TH:i:sZ", Date)),
    Location = case bson:lookup(geo, Tweet) of
                   {null} -> undefined;
                   {{type, <<"Point">>, coordinates, P=[_,_]}} -> P
               end,
    {User} = bson:lookup(user, Tweet),
    {ScreenName} = bson:lookup(screen_name, User),
    list_to_tuple([source, <<"twitter">>,
                   original, Tweet,
                   location, Location,
                   screenname, ScreenName,
                   id, Id,
                   text, Text,
                   date, FormattedDate
                  ]).


decode_json({struct, KeyValues}) ->
    P = [{list_to_atom(K), decode_json(V)} || {K,V} <- KeyValues],
    proplist_to_bson(P);

decode_json({array, List}) ->
    [decode_json(V) || V <- List];

decode_json(V) when is_integer(V); is_atom(V); is_float(V); is_binary(V) ->
    V;

decode_json(L) when is_list(L) ->
    list_to_binary(to_utf8(L));

decode_json(R) ->
    lager:error("Unknown json decode: ~p", [R]).




%%% UTF-8 encoding. Note that characters outside unicode are replaced with ?
to_utf8(List) when is_list(List) -> lists:flatmap(fun to_utf8/1, List);
to_utf8(Ch) -> char_to_utf8(Ch).

char_to_utf8(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch < 128 ->
	    %% 0yyyyyyy
	    [Ch];
       Ch < 16#800 ->
	    %% 110xxxxy 10yyyyyy
	    [16#C0 + (Ch bsr 6),
	     128+(Ch band 16#3F)];
       Ch < 16#10000 ->
	    %% 1110xxxx 10xyyyyy 10yyyyyy
	    if Ch < 16#D800; Ch > 16#DFFF, Ch < 16#FFFE ->
		    [16#E0 + (Ch bsr 12),
		     128+((Ch bsr 6) band 16#3F),
		     128+(Ch band 16#3F)];
               true -> [$?]
	    end;
       Ch < 16#200000 ->
	    %% 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
	    [16#F0+(Ch bsr 18),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#4000000 ->
	    %% 111110xx 10xxxyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#F8+(Ch bsr 24),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#80000000 ->
	    %% 1111110x 10xxxxyy 10yyyyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#FC+(Ch bsr 30),
	     128+((Ch bsr 24) band 16#3F),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       true -> [$?]
    end.



