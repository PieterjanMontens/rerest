%% @author Pieterjan Montens <pieterjan@montens.net>
%% @doc == rerest root handler ==
%%
%% This Source Code Form is subject to the terms of the Mozilla Public 
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% @doc API behaviour:
%% ===================================================================
%% verb     | Consequence on ROOT           | Parameters
%% -------------------------------------------------------------------
%% HEAD     | Reply headers
%% -------------------------------------------------------------------
%% GET      | Reply headers + content       | filtering (fields)
%%          |                               | paging (limit & offset)
%%          |                               | depth(sublist expansion)
%%          |                               | structure(sideline/nest)
%% -------------------------------------------------------------------
%% POST     | New record created            | Request body contains
%%          |                               | data for new record
%% -------------------------------------------------------------------
%% PUT      | NOT AVAILABLE
%% -------------------------------------------------------------------
%% DELETE   | NOT AVAILABLE
%% -------------------------------------------------------------------
%% PATCH    | NOT AVAILABLE
%% ===================================================================
%%
%% @doc link relations provided by resource:
%% self         - own IRI
%% help         - documentation index (for root page)
%%


-module(root_handler).

%% Required Exports
-export([init/3
        ,rest_init/2
        ,rest_terminate/2]).

%% Optional REST callbacks
-export([content_types_provided/2
        ,content_types_accepted/2
        ,allowed_methods/2
        ]).

%% Media Content acceptors
-export([from_form/2
        ,from_json/2
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
allowed_methods(Req,State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>] ,Req ,State}.

content_types_accepted(Req,State) ->
    Out = [{{<<"application">>,<<"json">>,'*'},from_json}
          ,{{<<"application">>,<<"x-www-form-urlencoded">>,'*'},from_form}
          ],
    {Out,Req,State}.
    
content_types_provided(Req,State) ->
    Out = [{{<<"text">>,<<"html">>,'*'},to_html}],
    {Out,Req,State}.


%% ===================================================================
%% Content Acceptors
%% ===================================================================
% POST should reply with 302 redirect to resource
% or 201 created ?
% and self link, of course

from_form(Req,State) ->
    Body = <<"<h1>Wow, thanks!">>,
    {Body,Req,State}.

from_json(Req,State) ->
    {ok,JSON,_} = cowboy_req:body(Req),
    _RRIFF = rerest_convert:from_json(JSON),

    Body  = <<"{'wow':'thanks'}">>,
    {Body,Req,State}.

%% ===================================================================
%% Content Providers
%% ===================================================================
to_html(Req,State) ->
    Body = atom_to_binary(?MODULE,utf8),
    {Body,Req,State}.
