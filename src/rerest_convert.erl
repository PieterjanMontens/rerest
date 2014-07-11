%% @author Pieterjan Montens <pieterjan@montens.net>
%% @doc == rerest RRIF format tokenizer ==
%% RRIF is just (ReRest Internal Format)
%%
%% This Source Code Form is subject to the terms of the Mozilla Public 
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% @doc _RRIF Format_
%% Hierarchichal tree, containing the data node's schema, content and flags
%% RIFF node: {Schema, Element}
%%  Schema : {Fields, Flags}
%%      Fields: #{Fieldname -> {Index,Flags}}
%%          Fieldname: binary string
%%          Index: integer
%%          Field flags: []
%%      Schema flags : []
%%  Element: {Data, flags}
%%      Data: [ContentElement]
%%          ContentElement: binary string | integer | float | Boolean | List | Record link
%%              List : [ContentElement]
%%              Boolean : true | false
%%              Record link : {rl,Record Id}
%%      Data flags: []
%%
-module(rerest_convert).
-include_lib("configuration.hrl").

-export([from_json/1, to_json/1
        ]).

%-define(NOMAPS,true).

%% ===================================================================
%% Exported functions
%% ===================================================================
%% @doc Build RRIF from Json input
-ifdef(NOMAPS).
from_json(JSONRaw) ->
    {Decoded} = jiffy:decode(JSONRaw),
    {_,FR,DR} = lists:foldl(fun({Key, Value},{Pos,FieldsRev,DataRev}) ->
                              {Pos+1, [{Key,Pos}| FieldsRev], [Value| DataRev]} end,
                           {1,[],[]},
                           Decoded),
    {{lists:reverse(FR),[]},{lists:reverse(DR),[]}}.
-else.
-spec from_json(binary()) -> rrif().
from_json(JSONRaw) ->
    DecodedMap = jiffy:decode(JSONRaw,[return_maps]),
    {_,FR,DR} = maps:fold(fun(Key,Value, {Pos,FieldMap,DataRev}) ->
                              {Pos+1, maps:put(Key,{Pos,[]},FieldMap), [Value| DataRev]} end,
                           {1,#{},[]},
                           DecodedMap),
    {{FR,[]},{lists:reverse(DR),[]}}.
-endif.


%% @doc Build Json output from RRIF
-ifdef(NOMAPS).
to_json(RRIF) ->
    {{Schema,_},{Data,_}} = RRIF,
    Jiffed = lists:foldl(fun({Key,Pos},Out) ->
                            [{Key,lists:nth(Pos,Data)} | Out] end,
                         [],
                         lists:reverse(Schema)),
    jiffy:encode({Jiffed}).
-else.
-spec to_json(rrif()) -> binary().
to_json(RRIF) ->
    {{Schema,_},{Data,_}} = RRIF,
    Jiffed = maps:fold(fun(Key,{Pos,[]},Out) ->
                            [{Key,lists:nth(Pos,Data)} | Out] end,
                       [],
                       Schema),
    jiffy:encode({lists:reverse(Jiffed)}).
-endif.

%% ===================================================================
%% TESTS
%% ===================================================================
-include_lib("eunit/include/eunit.hrl").

-ifdef(NOMAPS).
from_json_simple_test() ->
    JsonIn  = <<"{\"age\":981,\"earthling\":true,\"foo\":\"bar\",\"name\":\"John\",\"surname\":\"Doe\"}">>,
    Schema  = {[{<<"foo">>,1}, {<<"name">>,2}, {<<"surname">>,3}, {<<"age">>,4}, {<<"earthling">>,5}],[]},
    Element = {[<<"bar">>,<<"John">>,<<"Doe">>,981,true],[]},
    RRIFOut = {Schema,Element},
    ?assertEqual(RRIFOut,from_json(JsonIn)).
-else.
from_json_simple_test() ->
    JsonIn  = <<"{\"age\":981,\"earthling\":true,\"foo\":\"bar\",\"name\":\"John\",\"surname\":\"Doe\"}">>,
    Schema  = {#{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}}, []},
    Element = {[981,true,<<"bar">>,<<"John">>,<<"Doe">>],[]},
    RRIFOut = {Schema,Element},
    ?assertEqual(RRIFOut,from_json(JsonIn)).
-endif.

-ifdef(NOMAPS).
to_json_simple_test() ->
    Schema  = {[{<<"foo">>,1}, {<<"name">>,2}, {<<"surname">>,3}, {<<"age">>,4}, {<<"earthling">>,5}],[]},
    Element = {[<<"bar">>,<<"John">>,<<"Doe">>,981,true],[]},
    RRIFIn  = {Schema,Element},
    JsonOut = <<"{\"foo\":\"bar\",\"name\":\"John\",\"surname\":\"Doe\",\"age\":981,\"earthling\":true}">>,
    ?assertEqual(JsonOut,to_json(RRIFIn)).
-else.
to_json_simple_test() ->
    Schema  = {#{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}}, []},
    Element = {[981,true,<<"bar">>,<<"John">>,<<"Doe">>],[]},
    RRIFIn  = {Schema,Element},
    JsonOut = <<"{\"age\":981,\"earthling\":true,\"foo\":\"bar\",\"name\":\"John\",\"surname\":\"Doe\"}">>,
    ?assertEqual(JsonOut,to_json(RRIFIn)).
-endif.
