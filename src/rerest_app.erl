-module(rerest_app).

-behaviour(application).

-include_lib("configuration.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-define(COWBOY_ACCEPTORS,100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_cowboy(),
    rerest_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal Functions
%% ===================================================================

start_cowboy() ->
    Dispatch = cowboy_router:compile(?COWBOY_ROUTES),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(rerest, ?COWBOY_ACCEPTORS,
        [{port, port()}],
        [{env, [{dispatch, Dispatch}]}]
    ).


port() ->
    {ok, Port} = application:get_env(http_port),
    Port.
