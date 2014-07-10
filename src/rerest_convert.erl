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
%%      Fields: [{Fieldname, index, Flags}]
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

-export([from_json/1, to_json/1
        ]).

%% ===================================================================
%% Exported functions
%% ===================================================================
%% @doc Build RRIF from Json input
from_json(JSONRaw) ->
    {Decoded} = jiffy:decode(JSONRaw),
    {_,FR,DR} = lists:foldl(fun({Key, Value},{Pos,FieldsRev,DataRev}) ->
                              {Pos+1, [{Key,Pos}| FieldsRev], [Value| DataRev]} end,
                           {1,[],[]},
                           Decoded),
    {{lists:reverse(FR),[]},{lists:reverse(DR),[]}}.

to_json(RRIF) ->
    {{Schema,_},{Data,_}} = RRIF,
    Jiffed = lists:foldl(fun({Key,Pos},Out) ->
                            [{Key,lists:nth(Pos,Data)} | Out] end,
                         [],
                         lists:reverse(Schema)),
    jiffy:encode({Jiffed}).

%% ===================================================================
%% TESTS
%% ===================================================================
-include_lib("eunit/include/eunit.hrl").


from_json_simple_test() ->
    JsonIn  = <<"{\"foo\": \"bar\",\"name\": \"John\", \"surname\":\"Doe\", \"age\":981, \"earthling\":true }">>,
    Schema  = {[{<<"foo">>,1}, {<<"name">>,2}, {<<"surname">>,3}, {<<"age">>,4}, {<<"earthling">>,5}],[]},
    Element = {[<<"bar">>,<<"John">>,<<"Doe">>,981,true],[]},
    RRIFOut = {Schema,Element},
    ?assertEqual(RRIFOut,from_json(JsonIn)).

to_json_simple_test() ->
    Schema  = {[{<<"foo">>,1}, {<<"name">>,2}, {<<"surname">>,3}, {<<"age">>,4}, {<<"earthling">>,5}],[]},
    Element = {[<<"bar">>,<<"John">>,<<"Doe">>,981,true],[]},
    RRIFIn  = {Schema,Element},
    JsonOut = <<"{\"foo\":\"bar\",\"name\":\"John\",\"surname\":\"Doe\",\"age\":981,\"earthling\":true}">>,
    ?assertEqual(JsonOut,to_json(RRIFIn)).
