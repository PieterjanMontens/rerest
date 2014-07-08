%% @author Pieterjan Montens <pieterjan@montens.net>
%% @doc == rerest basic handler ==
%% This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% See Cowboy Rest documentation at 
%% https://github.com/extend/cowboy/blob/master/guide/rest_handlers.md
%% http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_rest/
-module(rerest_handler).

-export([init/3
        ,rest_init/2
        ,rest_terminate/2]).

%% ===================================================================
%% Required API functions
%% ===================================================================

%% @doc upgrade http request to rest
init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest}.


rest_init(Req, Opts) ->
    {ok, Req, dict:new()}.

rest_terminate(Req, State) ->
    ok.

%% ===================================================================
%% REST Callbacks
%% ===================================================================

content_types_provided(Req,State) ->
    Out = [{<<"text">>,<<"html">>,'*',to_html}],
    {Out,Req,State}.


%% ===================================================================
%% Content Providers
%% ===================================================================
to_html(Req,State) ->
    Body = <<"salut">>,
    {Body,Req,State}.
