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


%% ===================================================================
%% Exported functions
%% ===================================================================
%% @doc Build RRIF from Json input
-spec from_json(json()) -> rrif().
from_json(JSONRaw) ->
    %WARNING: Bad quality code below
    Handle = fun(Value,Acc) -> if is_list(Value) -> F = maps:get(handlelist,Acc),
                                                    O = lists:foldl(F,Acc#{datarev := []},Value),
                                                    lists:reverse(maps:get(datarev,O));
                                  is_map(Value)  -> F = maps:get(handlemap,Acc),
                                                    O = maps:fold(F,Acc#{pos := 1, fieldmap := #{}, datarev := []},Value),
                                                    {{maps:get(fieldmap,O),[]},{lists:reverse(maps:get(datarev,O)),[]}};
                                  true           -> Value end end,

    HandleMap = fun(Key,Value,Acc) ->
                    Out = Handle(Value,Acc),
                    Acc#{pos := maps:get(pos,Acc) + 1,
                         fieldmap := maps:put(Key,{maps:get(pos,Acc),[]}, maps:get(fieldmap,Acc)), 
                         datarev := [Out | maps:get(datarev,Acc)]}
                end,
    
    HandleList = fun(Value,Acc) ->
                    Out = Handle(Value,Acc),
                    Acc#{datarev := [Out | maps:get(datarev,Acc)]}
                 end,
                    
    DecodedMap = jiffy:decode(JSONRaw,[return_maps]),

    Result = maps:fold(HandleMap,
                       #{pos => 1, fieldmap => #{}, datarev => [], handlemap => HandleMap, handlelist => HandleList},
                       DecodedMap),

    {{maps:get(fieldmap,Result),[]},{lists:reverse(maps:get(datarev,Result)),[]}}.


%% @doc Build Json output from RRIF
-spec to_json(rrif()) -> json().
-record(ths, {handle,handleriff}).
to_json(RRIF) ->
    %WARNING: Bad quality code below
    Handle = fun(Value,S) ->
                if is_list(Value)  -> F = S#ths.handle,
                                      [F(El,S) || El <- Value];
                   is_tuple(Value) -> F = S#ths.handleriff,
                                      F(Value,S);
                   true            -> Value end end,
        
    HandleRRIF = fun({{Schema,_},{Data,_}},S) ->
                    maps:fold(fun(Key,{Pos,[]},Out) ->
                                F = S#ths.handle,
                                maps:put(Key,
                                         F(lists:nth(Pos,Data),S),
                                         Out) end,
                               #{},
                               Schema) end,
                    
    Jiffed = HandleRRIF(RRIF,#ths{handle = Handle, handleriff = HandleRRIF}),
    jiffy:encode(Jiffed).

%% ===================================================================
%% TESTS
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
from_json_simple_test() ->
    JsonIn  = <<"{\"age\":981,\"earthling\":true,\"foo\":\"bar\",\"name\":\"John\",\"surname\":\"Doe\"}">>,
    Schema  = {#{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}}, []},
    Element = {[981,true,<<"bar">>,<<"John">>,<<"Doe">>],[]},
    RRIFOut = {Schema,Element},
    ?assertEqual(RRIFOut,from_json(JsonIn)).

to_json_simple_test() ->
    Schema  = {#{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}}, []},
    Element = {[981,true,<<"bar">>,<<"John">>,<<"Doe">>],[]},
    RRIFIn  = {Schema,Element},
    JsonOut = <<"{\"surname\":\"Doe\",\"name\":\"John\",\"foo\":\"bar\",\"earthling\":true,\"age\":981}">>,
    io:format("\n\nMe: ~P \n",[JsonOut,100]),
    io:format("\n\nErl: ~P \n",[to_json(RRIFIn),100]),
    ?assertEqual(JsonOut,to_json(RRIFIn)).

from_json_nested_test() ->
    JsonIn = <<"{\"foo\":[\"bar\",\"maid\"],\"person\": { \"name\" :\"John\",\"surname\":\"Doe\"},\"numbers\":[{\"num\": 981},{\"num\" : 982}],\"earthling\":true}">>,
    SchemaRootEl = {#{<<"earthling">> => {1,[]},<<"foo">> => {2,[]}, <<"numbers">> => {3,[]}, <<"person">> => {4,[]}},[]},
    SchemaPersonEl = {#{<<"name">> => {1,[]}, <<"surname">> => {2,[]}},[]},
    SchemaNumberEl = {#{<<"num">> => {1,[]}},[]},
    DataNumberEl1 = {[981],[]},
    DataNumberEl2 = {[982],[]},
    DataPerson = {[<<"John">>,<<"Doe">>],[]},
    RRIFOut = {SchemaRootEl,{[true,[<<"bar">>,<<"maid">>],[{SchemaNumberEl,DataNumberEl1},{SchemaNumberEl,DataNumberEl2}],{SchemaPersonEl,DataPerson}],[]}},
    %Out = from_json(JsonIn),
    %io:format("\n\nMe: ~P \n",[RRIFOut,100]),
    %io:format("\n\nErl: ~P \n",[Out,100]),
    ?assertEqual(RRIFOut, from_json(JsonIn)).

to_json_nested_test() ->
    JsonOut =   <<"{\"person\":{\"surname\":\"Doe\",\"name\":\"John\"},\"numbers\":[{\"num\":981},{\"num\":982}],\"foo\":[\"bar\",\"maid\"],\"earthling\":true}">>,
    SchemaRootEl = {#{<<"earthling">> => {1,[]},<<"foo">> => {2,[]}, <<"numbers">> => {3,[]}, <<"person">> => {4,[]}},[]},
    SchemaPersonEl = {#{<<"name">> => {1,[]}, <<"surname">> => {2,[]}},[]},
    SchemaNumberEl = {#{<<"num">> => {1,[]}},[]},
    DataNumberEl1 = {[981],[]},
    DataNumberEl2 = {[982],[]},
    DataPerson = {[<<"John">>,<<"Doe">>],[]},
    RRIFIn = {SchemaRootEl,{[true,[<<"bar">>,<<"maid">>],[{SchemaNumberEl,DataNumberEl1},{SchemaNumberEl,DataNumberEl2}],{SchemaPersonEl,DataPerson}],[]}},
    % Jiffy reverts the map keys when encoding to json.
    ?assertEqual(JsonOut, to_json(RRIFIn)).


%  Example nested RRIF:
%  {{#{<<"earthling">> => {1,[]},
%           <<"foo">> => {2,[]},
%           <<"numbers">> => {3,[]},
%           <<"person">> => {4,[]}},
%         []},
%        {[true,<<"bar">>,
%          [{{#{<<"num">> => {1,[]}},[]},{[981],[]}},
%           {{#{<<"num">> => {1,[]}},[]},{[981],[]}}],
%          {{#{<<"name">> => {1,[]},<<"surname">> => {2,[]}},[]},
%           {[<<"John">>,<<"Doe">>],[]}}],
%         []}}


-endif.

