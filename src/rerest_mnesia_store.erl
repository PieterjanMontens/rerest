%% @author Pieterjan Montens <pieterjan@montens.net>
%% @doc == rerest RRIF Mnesia Storage ==
%% RRIF is just (ReRest Internal Format)
%%
%% This Source Code Form is subject to the terms of the Mozilla Public 
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(rerest_mnesia_store).
-include_lib("configuration.hrl").

-export([init/0]).

-export([create/1
        ,read/1
        ,update/2
        ,delete/1
        ]).

-define(LAGER_I,?LAGER_LN ++ "rerest mnesia store:").

%% ===================================================================
%% Mnesia Storage Configuration
%% ===================================================================
-record(rerest_schema,{id     = <<"">> :: schema_id(),
                       keys   = [] :: list(),
                       keymap = #{} :: schema(),
                       flags  = [] :: schema_flags()
                       }).
-type(rec_schema() :: #rerest_schema{}).

-record(rerest_data,{id     = <<"">> :: data_id(),
                     schema_id = <<"">> :: schema_id(),
                     data   = [] :: list(),
                     flags  = [] :: data_flags()
                     }).

-type(rec_data() :: #rerest_data{}).

-record(rerest_counters,{key  :: atom(),
                         cntr :: integer()
                         }).

-define(REREST_RECORD_DATA,#{rerest_schema => record_info(fields,rerest_schema),
                             rerest_data   => record_info(fields,rerest_data),
                             rerest_counters => record_info(fields,rerest_counters)}).

-define(REREST_RECORD_INDEX,#{rerest_schema => [keys],
                              rerest_data => [],
                              rerest_counters => []}).

-ifdef(TEST).

-define(SCHEMA_DB,rerest_test_schema).
-define(DATA_DB,rerest_test_data).
-define(AUTOINCREMENT(_Type), fun() -> _C = case get(test_cntr) of undefined -> 0; _N -> _N end,put(test_cntr,_C+1),_C end()).

-else.

-define(AUTOINCREMENT(Type), mnesia:dirty_update_counter(rerest_counters,Type,1)).
-define(SCHEMA_DB,rerest_schema).
-define(DATA_DB,rerest_data).

-endif.

%% ===================================================================
%% Exported functions
%% ===================================================================
-spec init() -> ok.
init() ->
    application:start(mnesia),
    Nodes = ?RUNNING_NODES,
    ensure_mnesia_init(Nodes),
    CheckTable = fun(T) ->
                    case mnesia:create_table(T, [
                            {attributes, maps:get(T,?REREST_RECORD_DATA)},
                            {access_mode, read_write},
                            {disc_copies,Nodes},
                            {index,maps:get(T,?REREST_RECORD_INDEX)}]) of
                        {aborted,{already_exists,_}} ->
                            lager:info("~s table ~s present",[?LAGER_I,T]);
                        {atomic,ok} ->
                            lager:notice("~s table ~s created",[?LAGER_I,T]);
                        Error ->
                            lager:critical("~s table ~s could not be created\n\terror: ~p",
                                           [?LAGER_I,T,Error]),
                            throw({rerest,failed_mnesia_startup})
                    end end,
    [CheckTable(T) || T <- maps:keys(?REREST_RECORD_DATA)],
    ok.

%% @doc Create record in storage
-spec create(rrif()) -> ok.
create(RRIFRaw) ->
    RRIF_flat = rrif_flatten(RRIFRaw),
    create_flat(RRIF_flat).

    create_flat(RRIF_flat) ->
        Length = maps:get(length,RRIF_flat),
        walk_and_store_flatrrif(RRIF_flat,Length,#{}).

        walk_and_store_flatrrif(_,0,Temp2Id) -> Temp2Id;
        walk_and_store_flatrrif(Map,Index,Temp2Id) ->
            RRIFRaw = maps:get(Index,Map),
            {{RawSch,_},_} = RRIFRaw,
            SchemaR = lookup_schema(RawSch),
            AlignedRRIF = align_rrif(RRIFRaw,SchemaR),
            RRIF = temp2id(AlignedRRIF,Temp2Id),
            DataRec = build_data(RRIF),
            write_data(DataRec),
            walk_and_store_flatrrif(Map,Index-1,maps:put(Index,DataRec#rerest_data.id,Temp2Id)).



%% @doc Retrieve record from storage
-spec read(data_id()) -> rrif().
read(Id) ->
    %TODO: Support for nested and flattened RIFFS
    DataRec = lookup_data(Id),
    SchemaRec = lookup_schema(DataRec#rerest_data.schema_id),
    build_rrif(SchemaRec,DataRec).

%% @doc Update record in storage
-spec update(data_id(),rrif()) -> ok.
update(Id,RRIFRaw) ->
    %TODO: Support for nested and flattened RIFFS
    SchemaR = lookup_schema(?RRIF_SCHEMA(RRIFRaw)),
    RRIF = align_rrif(RRIFRaw,SchemaR),
    DataRec = build_data(Id,RRIF),
    write_data(DataRec).

%% @doc Delete record from storage
-spec delete(data_id()) -> ok.
delete(Id) ->
    %TODO: Support for nested and flattened RIFFS
    DataRec = lookup_data(Id),
    remove_data(DataRec#rerest_data.id),
    check_schema_removal(DataRec#rerest_data.schema_id),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
%% @doc Build new schema
-spec build_schema(schema(),fun()) -> rec_schema().
build_schema(Sch,ID_GEN) ->
    Id = erlang:integer_to_binary(ID_GEN(),36),
    Fields = lists:sort(maps:keys(Sch)),
    #rerest_schema{id = Id,
                   keys = Fields,
                   keymap = Sch,
                   flags  = [?CURRENT_VERSION]
                  }.

%% @doc Build data object
-spec build_data(rrif()) -> rec_data().
build_data(RRIF) ->
    build_data(RRIF,fun() -> ?AUTOINCREMENT(rerest_data) end).

-spec build_data(rrif(),fun() | data_id()) -> rec_data().
build_data(RRIF,ID_GEN) when is_function(ID_GEN) ->
    Id = erlang:integer_to_binary(ID_GEN(),36),
    build_data(RRIF,Id);

build_data({{_,SchFlags},{Data,DataFlags}},Id) ->
    #rerest_data{id = Id,
                  schema_id = proplists:get_value(id,SchFlags),
                  data = Data,
                  flags = DataFlags
                 }.

%% @doc Build Rrif object from provided schema and data
-spec build_rrif(rec_schema(),rec_data()) -> rrif().
build_rrif(S,D) ->
    {{S#rerest_schema.keymap,
      [{id,S#rerest_schema.id}| S#rerest_schema.flags]},
     {D#rerest_data.data,
      D#rerest_data.flags}}.



%% @doc Align RRIF fields on provided schema keymap
-spec align_rrif(rrif(),rec_schema()) -> rrif().
align_rrif({{KeyMapIn,SchFlagsIn},{DataIn,DataFlagsIn}},S) ->
    Schema = S#rerest_schema.keymap,
    DataOutRev = maps:fold(fun(Key,_,Acc) ->
                               case  maps:get(Key,KeyMapIn) of
                                   undefined -> Acc;
                                   {InPos,_} when is_integer(InPos) -> [lists:nth(InPos,DataIn) | Acc] 
                               end end,
                             [],
                             Schema),
    SchFlagsOut = [{id,S#rerest_schema.id} | SchFlagsIn],
    DataFlagsOut = DataFlagsIn,
    {{Schema,SchFlagsOut},{lists:reverse(DataOutRev),DataFlagsOut}}.

%% @doc Sort schema fields in a standard manner
-spec sort_schema(schema()) -> schema().
sort_schema(Schema) ->
    {_,Out} = lists:foldl(fun(Key,{Pos,MapIn}) ->
                              {_,Flags} = maps:get(Key,Schema),
                              {Pos+1,maps:put(Key,{Pos,Flags},MapIn)}
                              end,
                          {1,#{}},
                          lists:sort(maps:keys(Schema))),
    Out.

%% @doc Flatten a rrif (ie: un-nest nested data)
%% This function returns a map, where each nested rrif has been replaced with a
%% temporary id, which happens to be the key of that rrif in the map.
%% The map also has a "length" key, with its size.
-spec rrif_flatten(rrif()) -> rrif_flat().
rrif_flatten(RRIF) ->
    walk_riff_stack([RRIF],#{},0,1).
    
    walk_riff_stack([],Map,_,Length) -> maps:put(length,Length,Map);

    walk_riff_stack([H|Tail],Map,PrecPos,OldCounter) ->
        Pos = PrecPos + 1,
        {MoreTail,RRIF,NewCounter} = walk_rrif(H,OldCounter),
        NewMap = maps:put(Pos,RRIF,Map),
        walk_riff_stack(Tail ++ MoreTail, NewMap,Pos,NewCounter).

    walk_rrif(RRIFIn,PrecPos) ->
        {SchemaNode,{DataIn,DataFlags}} = RRIFIn,
        {DataOut,MoreTail,NewPos} = walk_list(DataIn,PrecPos,[],[]),
        RRIFOut = {SchemaNode,{DataOut,DataFlags}},

        {MoreTail, RRIFOut, NewPos}.

    walk_list([],Pos,DataAcc,TailAcc) ->
        {lists:reverse(DataAcc), lists:reverse(TailAcc),Pos};

    walk_list([H|Tail],PrecPos,DataAcc,TailAcc) ->
        if 
            is_tuple(H) ->
                Pos = PrecPos + 1,
                walk_list(Tail,Pos,[{rtemp,Pos}|DataAcc],[H|TailAcc]);
            is_list(H) ->
                {DataOut, MoreTail, NewPos} = walk_list(H,PrecPos,[],[]),
                walk_list(Tail,NewPos,[DataOut|DataAcc],lists:append(lists:reverse(MoreTail),TailAcc));
            true -> 
                walk_list(Tail,PrecPos,[H|DataAcc],TailAcc)
        end.

%% @doc Replace a flattened RRIF's data temporary id's with the final ones
-spec temp2id(rrif(),temp2id()) -> rec_data().
temp2id({Schema,{DataIn,DataFlags}},Temp2Id) ->
    DataOut = temp2id_walk(DataIn,Temp2Id,[]),
    {Schema,{DataOut,DataFlags}}.

    temp2id_walk([],_,Acc) -> lists:reverse(Acc);

    temp2id_walk([H|Tail],Temp2id,Acc) ->
        if  is_tuple(H) -> {rtemp,TId} = H,
                           temp2id_walk(Tail,Temp2id,[{r,maps:get(TId,Temp2id)} | Acc]);
            is_list(H) ->  NewList = temp2id_walk(H,Temp2id,[]),
                           temp2id_walk(Tail,Temp2id,[NewList|Acc]);
            true -> temp2id_walk(Tail,Temp2id,[H|Acc])
        end.


%% ===================================================================
%% Internal MNESIA related functions
%% ===================================================================
%% @doc Check for present tables, init disc node if no tables present (aka first run check)
-spec ensure_mnesia_init(list()) -> list().
ensure_mnesia_init(Nodes) ->
    case mnesia:system_info(tables) of
        [schema] = T -> mnesia:stop(),
                    mnesia:create_schema(Nodes),
                    mnesia:start(),
                    T;
        Tables -> Tables
    end.

%% @doc Find corresponding schema in collection, create if not present
-spec lookup_schema(schema()) -> rec_schema().
lookup_schema(SchUnsorted) when is_map(SchUnsorted) ->
    Sch = sort_schema(SchUnsorted),
    Fields = lists:sort(maps:keys(Sch)),
    Pattern = {rerest_schema,'_',Fields,'_','_'},
    case mnesia:dirty_match_object(?SCHEMA_DB,Pattern) of
        [Schema] -> Schema;
        _        -> NewSchema   = build_schema(Sch,fun() -> ?AUTOINCREMENT(?SCHEMA_DB) end),
                    {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(?SCHEMA_DB,NewSchema,write) end),
                    lager:notice("~s created new schema with fields:\n\t~p",
                                [?LAGER_I,NewSchema#rerest_schema.keys]),
                    NewSchema
    end;

lookup_schema(Id) when is_binary(Id) ->
    case mnesia:dirty_read(?SCHEMA_DB,Id) of
        [Record] -> Record;
        _        -> undefined
    end.

%% @doc Check if schema is still used. Of no data record uses the schema, delete it.
-spec check_schema_removal(schema_id()) -> ok.
check_schema_removal(Id) ->
    case count_schema_uses(Id) of
        0 -> remove_schema(Id);
        _ -> ok
    end.

remove_schema(Id) ->
    lager:notice("~s Removing schema record ~s", [?LAGER_I,Id]),
    {atomic,ok} = mnesia:transaction(fun() ->
                    [R|_] = mnesia:read(?SCHEMA_DB,Id),
                    mnesia:delete_object(?SCHEMA_DB,R,write) 
                    end),
    ok.


%% @doc Count uses of schema by data records
-spec count_schema_uses(schema_id()) -> ok.
count_schema_uses(Id) ->
    {atomic,C} = mnesia:transaction(fun() ->
                    mnesia:foldl(fun(DataRec,Acc) ->
                                    if DataRec#rerest_data.schema_id =:= Id -> Acc + 1;
                                       true -> Acc end end,
                                 0,
                                 ?DATA_DB) end),
    C.

%% @doc Lookup data object in storage
-spec lookup_data(data_id()) -> rec_data().
lookup_data(Id) ->
    case mnesia:dirty_read(?DATA_DB,Id) of
        [Record|_] -> Record;
        _        -> undefined
    end.

%% @doc Write RRIF record to storage
-spec write_data(rec_data()) -> ok.
write_data(D) ->
    {atomic,ok} = mnesia:transaction(fun() ->
                    mnesia:write(?DATA_DB,D,write) end),
    ok.

%% @doc Remove data record from storage
-spec remove_data(data_id()) -> ok.
remove_data(Id) ->
    lager:notice("~s Removing data record ~s", [?LAGER_I,Id]),
    {atomic,ok} = mnesia:transaction(fun() ->
                    [R|_] = mnesia:read(?DATA_DB,Id),
                    mnesia:delete_object(?DATA_DB,R,write) 
                    end),
    ok.

%% ===================================================================
%% TESTS
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_schema_test() ->
    SchemaIn  = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    SchemaOut = #rerest_schema{id = <<"3A">>,
                               keys = [<<"age">>, <<"earthling">>, <<"foo">>, <<"name">>, <<"surname">>],
                               keymap = SchemaIn,
                               flags  = [?CURRENT_VERSION]},
    ?assertMatch(SchemaOut, build_schema(SchemaIn,fun() -> 118 end)).

build_data_test_() ->
    SchemaIn = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    ElemenIn = {[981,true,<<"bar">>,<<"John">>,<<"Doe">>],[]},
    RRIFIn  = {{SchemaIn,[{id,1}]},ElemenIn},
    DataOut = #rerest_data{ id = <<"E">>,
                            schema_id = 1,
                            data = [981,true,<<"bar">>,<<"John">>,<<"Doe">>],
                            flags = []},
    [?_assertEqual(DataOut, build_data(RRIFIn, fun() -> 14 end)),
     ?_assertEqual(DataOut, build_data(RRIFIn, <<"E">>))].

build_rrif_test() ->
    DataIn = #rerest_data{ id = <<"E">>,
                           schema_id = 1,
                           data = [981,true,<<"bar">>,<<"John">>,<<"Doe">>],
                           flags = []},
    SchemaIn = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    SchemaRecIn  = #rerest_schema{id = <<"3A">>,
                             keys = [<<"age">>, <<"earthling">>, <<"foo">>, <<"name">>, <<"surname">>],
                             keymap = SchemaIn,
                             flags  = [?CURRENT_VERSION]},
    RrifOut = {{SchemaIn,[{id,<<"3A">>},?CURRENT_VERSION]}
              ,{[981,true,<<"bar">>,<<"John">>,<<"Doe">>],[]}},
    ?assertEqual(RrifOut, build_rrif(SchemaRecIn,DataIn)).

align_element_test() ->
    SchemaIn = #{<<"earthling">> => {1,[]}, <<"age">> => {2,[]}, <<"foo">> => {3,[]}, <<"surname">> => {4,[]}, <<"name">> => {5,[]}},
    ElemenIn = {[true,981,<<"bar">>,<<"Doe">>,<<"John">>],[]},
    RRIFIn   = {{SchemaIn,[]},ElemenIn},
    SchemaOu = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    ElemenOu = {[981,true,<<"bar">>,<<"John">>,<<"Doe">>],[]},
    RRIFOut  = {{SchemaOu,[{id,1}]},ElemenOu},
    ?assertEqual(RRIFOut, align_rrif(RRIFIn,#rerest_schema{id=1,keymap=SchemaOu})).

sort_schema_test_() ->
    Test1In  = #{<<"earthling">> => {1,[]}, <<"age">> => {2,[test]}, <<"foo">> => {3,[]}, <<"surname">> => {4,[]}, <<"name">> => {5,[]}},
    Test1Out = #{<<"age">> => {1,[test]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    Test2In  = #{<<"aba">> => {1,[]}, "a" => {2,[]}, 1 => {3,[test]}},
    Test2Out = #{1 => {1,[test]},"a" => {2,[]},<<"aba">> => {3,[]}},
    [?_assertEqual(Test1Out, sort_schema(Test1In))
    ,?_assertEqual(Test2Out, sort_schema(Test2In))].

rrif_flatten_test() ->
    SchemaRootEl = {#{<<"earthling">> => {1,[]},<<"foo">> => {2,[]}, <<"numbers">> => {3,[]}, <<"person">> => {4,[]}},[]},
    SchemaPersonEl = {#{<<"name">> => {1,[]}, <<"surname">> => {2,[]}},[]},
    SchemaNumberEl = {#{<<"num">> => {1,[]}},[]},
    DataNumberNode = [{SchemaNumberEl,{[981],[]}},{SchemaNumberEl,{[982],[]}}],
    DataPerson = {SchemaPersonEl,{[<<"John">>,<<"Doe">>],[]}},
    RRIFIn = {SchemaRootEl,{[true,[<<"bar">>,<<"maid">>],DataNumberNode,DataPerson],[]}},

    RRIFFlat = #{ 1 => {SchemaRootEl,{[true,[<<"bar">>,<<"maid">>],[{rtemp,2},{rtemp,3}],{rtemp,4}],[]}},
                  2 => {SchemaNumberEl,{[981],[]}},
                  3 => {SchemaNumberEl,{[982],[]}},
                  4 => {SchemaPersonEl,{[<<"John">>,<<"Doe">>],[]}},
                  length => 4},

    io:format("\n\nComputed: ~P \n",[rrif_flatten(RRIFIn),100]),
    io:format("\n\nExpected: ~P \n",[RRIFFlat,100]),
    ?assertEqual(RRIFFlat, rrif_flatten(RRIFIn)).
                
temp2id_test() ->
    RiffIn = {{#{<<"earthling">> => {1,[]}, <<"foo">> => {2,[]}, <<"numbers">> => {3,[]}, <<"person">> => {4,[]}}, []},
              {[true, [<<"bar">>,<<"maid">>], [{rtemp,2},{rtemp,3}], {rtemp,4}], []}},

    Map = #{2 => <<"ABA">>, 3 => <<"BCB">>, 4 => <<"AVA">>},

    RiffOut = {{#{<<"earthling">> => {1,[]}, <<"foo">> => {2,[]}, <<"numbers">> => {3,[]}, <<"person">> => {4,[]}}, []},
              {[true, [<<"bar">>,<<"maid">>], [{r,<<"ABA">>},{r,<<"BCB">>}], {r,<<"AVA">>}], []}},

    ?assertEqual(RiffOut, temp2id(RiffIn,Map)).


mnesia_data_test_() ->
    {spawn,
        {setup,
            fun testdbs_start/0,
            fun testdbs_stop/1,
            {inorder,
            [
                fun test_data_insert/0,
                fun test_data_lookup/0,
                fun test_schema_count/0,
                fun test_data_remove/0
            ]}
        }
    }.

test_data_insert() ->
    TestData = #rerest_data{ id = <<"E">>,
                              schema_id = <<"S">>,
                              data = [981,true,<<"bar">>,<<"John">>,<<"Doe">>],
                              flags = []},
    ?assert(ok =:= write_data(TestData)),
    ?assert(1 =:= length(mnesia:dirty_all_keys(rerest_test_data))).

test_data_lookup() ->
    TestData = #rerest_data{ id = <<"E">>,
                              schema_id = <<"S">>,
                              data = [981,true,<<"bar">>,<<"John">>,<<"Doe">>],
                              flags = []},
    ?assertEqual(TestData, lookup_data(<<"E">>)).

test_data_remove() ->
    Id = <<"E">>,
    ?assert(ok =:= remove_data(Id)),
    ?assert(0 =:= length(mnesia:dirty_all_keys(rerest_test_data))).

test_schema_count() ->
    Id = <<"S">>,
    ?assertEqual(1, count_schema_uses(Id)),
    ?assert(0 =:= count_schema_uses(<<"AQ">>)).

mnesia_schema_test_() ->
    {spawn,
        {setup,
            fun testdbs_start/0,
            fun testdbs_stop/1,
            {inorder,
            [
                fun test_schema_emptylookup/0,
                fun test_schema_presentlookup_/0,
                fun test_schema_removal/0,
                fun test_schema_emptylookup/0
            ]}
        }
    }.


test_schema_emptylookup() ->
    PreCheck  = mnesia:dirty_all_keys(rerest_test_schema),
    SchemaIn  = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    Out       = lookup_schema(SchemaIn),
    PostCheck =  mnesia:dirty_all_keys(rerest_test_schema),
    ?assert(0 =:= length(PreCheck)),
    ?assert(1 =:= length(PostCheck)),
    ?assert(is_record(Out,rerest_schema)).

test_schema_presentlookup_() ->
    SchemaIn  = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    Out       = lookup_schema(SchemaIn),
    LookupById = lookup_schema(Out#rerest_schema.id),
    [?_assertEqual(1,length(mnesia:dirty_all_keys(rerest_test_schema))),
     ?_assert(is_record(Out,rerest_schema)),
     ?_assertEqual(LookupById,Out)].

test_schema_removal() ->
    PreCheck =  mnesia:dirty_all_keys(rerest_test_schema),
    Schema  = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    SchemaRec  = lookup_schema(Schema),
    ok = remove_schema(SchemaRec#rerest_schema.id),
    PostCheck =  mnesia:dirty_all_keys(rerest_test_schema),
    ?assert(1 =:= length(PreCheck)),
    ?assert(0 =:= length(PostCheck)).


%% Mnesia Setup & Teardown testing
-define(REREST_TEST_DBDATA,#{rerest_test_schema => record_info(fields,rerest_schema),
                             rerest_test_data   => record_info(fields,rerest_data) }).
-define(REREST_TEST_RECMAP,#{rerest_test_schema => rerest_schema,
                             rerest_test_data => rerest_data}).

testdbs_start() ->
    mnesia:start(),
    CheckTable = fun(T) -> mnesia:create_table(T, [
                            {attributes, maps:get(T,?REREST_TEST_DBDATA)},
                            {record_name,maps:get(T,?REREST_TEST_RECMAP)},
                            {access_mode, read_write},
                            {ram_copies,[node()]} ]) end,
    [CheckTable(T) || T <- maps:keys(?REREST_TEST_DBDATA)],
    ok.

testdbs_stop(_) ->
    [mnesia:delete_table(T) || T <- maps:keys(?REREST_TEST_DBDATA)].

-endif.
