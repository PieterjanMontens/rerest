%% @author Pieterjan Montens <pieterjan@montens.net>
%% @doc == rerest RRIF format tokenizer ==
%% RRIF is just (ReRest Internal Format)
%%
%% This Source Code Form is subject to the terms of the Mozilla Public 
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(rerest_mnesia_store).
-include_lib("configuration.hrl").

-compile(export_all).

-export([init/0]).

-export([create/1
        ,read/1
        ,update/1
        ,delete/1
        ]).

-define(LAGER_I,?LAGER_LN ++ "rerest mnesia store:").

%% ===================================================================
%% Mnesia Store Configuration
%% ===================================================================
-define(SCHEMA_DB,rerest_schema).
-record(rerest_schema,{id     = <<"">> :: binary(),
                       keys   = [] :: list(),
                       keymap = #{} :: schema(),
                       flags  = [] :: schema_flags()
                       }).
-type(rec_schema() :: #rerest_schema{}).

-record(rerest_data,{id     = <<"">> :: binary(),
                     schema_id = <<"">> :: binary(),
                     data   = [] :: list(),
                     flags  = [] :: data_flags()
                     }).

-record(rerest_counters,{key  :: atom(),
                         cntr :: integer()
                         }).

-define(REREST_RECORD_DATA,#{rerest_schema => record_info(fields,rerest_schema)
                            ,rerest_data   => record_info(fields,rerest_data)
                            ,rerest_counters => record_info(fields,rerest_counters)}).

-define(AUTOINCREMENT(Type), mnesia:dirty_update_counter(rerest_counters,Type,1)).

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
                            {disc_copies,Nodes} ]) of
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
create({{RawSch,_},_} = RRIF) ->
    _Sch = lookup_schema(RawSch,?SCHEMA_DB),
    %TODO: Find schema: if not existing, create
    %TODO: Make data correspond to found schema
    %TODO: Write data to DB
    ok.

%% @doc Retrieve record from storage
read(_RRIF) ->
    ok.

%% @doc Update record in storage
update(_RRIF) ->
    ok.

%% @doc Delete record from storage
delete(_RRIF) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
%% @doc Find corresponding schema in collection, create if not present
-spec lookup_schema(schema(),atom()) -> rec_schema().
lookup_schema(Sch,T) ->
    Fields = lists:sort(maps:keys(Sch)),
    Pattern = {T,Fields,'_','_'},
    case mnesia:dirty_match_object(Pattern) of
        [Schema] -> Schema;
        _        -> NewSchema   = build_schema(Sch,fun() -> ?AUTOINCREMENT(T) end),
                    {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(T,NewSchema,write) end),
                    lager:notice("~s created new schema with fields:\n\t~p",
                                [?LAGER_I,NewSchema#rerest_schema.keys]),
                    NewSchema
    end.

% @doc Build new schema
-spec build_schema(schema(),fun()) -> rec_schema().
build_schema(Sch,ID_GEN) ->
    Id = erlang:integer_to_binary(ID_GEN(),36),
    Fields = lists:sort(maps:keys(Sch)),
    #rerest_schema{id = Id,
                   keys = Fields,
                   keymap = Sch,
                   flags  = [?CURRENT_VERSION]
                  }.


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

%% ===================================================================
%% TESTS
%% ===================================================================
-include_lib("eunit/include/eunit.hrl").

build_schema_test() ->
    SchemaIn  = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    SchemaOut = #rerest_schema{id = <<"3A">>,
                               keys = [<<"age">>, <<"earthling">>, <<"foo">>, <<"name">>, <<"surname">>],
                               keymap = SchemaIn,
                               flags  = [?CURRENT_VERSION]},
    ?assertMatch(SchemaOut, build_schema(SchemaIn,fun() -> 118 end)).

% Check if there is some way to define macros according to if we are testing or not (some eunit env var or something)
lookup_schema_test_() ->
    {spawn,
        {setup,
            fun testdbs_start/0,
            fun testdbs_stop/1,
            {inorder,
            [
                fun test_schema_emptylookup/0,
                fun test_schema_presentlookup/0
            ]}
        }
    }.


test_schema_emptylookup() ->
    PreCheck  = mnesia:dirty_all_keys(rerest_test_schema),
    SchemaIn  = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    Out       = lookup_schema(SchemaIn,rerest_test_schema),
    PostCheck =  mnesia:dirty_all_keys(rerest_test_schema),
    ?assert(0 =:= length(PreCheck)),
    ?assert(1 =:= length(PostCheck)),
    ?assert(is_record(Out,rerest_schema)).

test_schema_presentlookup() ->
    SchemaIn  = #{<<"age">> => {1,[]}, <<"earthling">> => {2,[]}, <<"foo">> => {3,[]}, <<"name">> => {4,[]}, <<"surname">> => {5,[]}},
    Out       = lookup_schema(SchemaIn,rerest_test_schema),
    ?assert(1 =:= length(mnesia:dirty_all_keys(rerest_test_schema))),
    ?assert(is_record(Out,rerest_schema)).

-define(REREST_TEST_DBDATA,#{rerest_test_schema => record_info(fields,rerest_schema),
                             rerest_test_data   => record_info(fields,rerest_data) }).
-define(REREST_TEST_RECMAP,#{rerest_test_schema => rerest_schema,
                             rerest_test_data =>rerest_data}).

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
