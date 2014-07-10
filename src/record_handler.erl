%% @author Pieterjan Montens <pieterjan@montens.net>
%% @doc == rerest root handler ==
%%
%% This Source Code Form is subject to the terms of the Mozilla Public 
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% @doc API behaviour:
%% ===================================================================
%% verb     | Consequence on RECORD         | Parameters
%% -------------------------------------------------------------------
%% HEAD     | Reply headers
%% -------------------------------------------------------------------
%% GET      | Reply headers + content       | filtering (fields)
%%          |                               | depth(sublist expansion)
%%          |                               | structure(sideline/nest)
%% -------------------------------------------------------------------
%% POST     | NOT AVAILABLE
%% -------------------------------------------------------------------
%% PUT      | Update record with data provided in request body
%%          | (id's must correspond)
%% -------------------------------------------------------------------
%% DELETE   | Delete record (and all child records)
%% -------------------------------------------------------------------
%% PATCH    | NOT AVAILABLE - act on fields for partial updates
%% ===================================================================
%%
%% @doc link relations provided by resource:
%% self         - own IRI
%% help         - documentation index (for record page)
%% index        - table of contents (root)
%% first        - first record of collection
%% last         - last record of collection
%% next         - next record
%% prev         - previous record
%% up           - URI of possible parent object 
%%                should current record be part of a nested collection

-module(record_handler).

%% Required Exports
-export([init/3
        ,rest_init/2
        ,rest_terminate/2]).

%% Optional REST callbacks
-export([content_types_provided/2
        ]).

%% Media Content Providers
-export([to_html/2
        ]).

%% ===================================================================
%% Required API functions
%% ===================================================================

%% @doc upgrade http request to rest
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


rest_init(Req, _Opts) ->
    {ok, Req, dict:new()}.

rest_terminate(_Req, _State) ->
    ok.

%% ===================================================================
%% REST Callbacks
%% ===================================================================

content_types_provided(Req,State) ->
    Out = [{{<<"text">>,<<"html">>,'*'},to_html}],
    {Out,Req,State}.


%% ===================================================================
%% Content Providers
%% ===================================================================
to_html(Req,State) ->
    Body = atom_to_binary(?MODULE,utf8),
    {Body,Req,State}.
