%% Define the nodes on which mnesia should run
-define(RUNNING_NODES,[node()]).
-define(CURRENT_VERSION,alpha).


%% [ {URIHost, list({URIPath, Handler, Opts})} | _ ]
-define(ROOT_URI, "/").
-define(STATIC_URI, ?ROOT_URI ++ "static").
-define(DOCS_URI, ?ROOT_URI ++ "docs").

-define(RECORD_URI, ?ROOT_URI ++ ":record_id").
-define(FIELD_URI, ?RECORD_URI ++ "/:field").

-define(LAGER_LN, "==== ").


-define(COWBOY_ROUTES, [ {'_', [ { ?STATIC_URI ++ "/[...]"
                                 , cowboy_static
                                 , {priv_dir, rerest, "static",[{mimetypes,{<<"text">>,<<"html">>,[]}}]}
                                 }
                                ,{ ?DOCS_URI ++ "/[...]"
                                 , cowboy_static
                                 , {priv_dir, rerest, "docs",[{mimetypes,{<<"text">>,<<"html">>,[]}}]}
                                 }

                                ,{ ?ROOT_URI, root_handler,[]}
                                ,{ ?RECORD_URI, record_handler, [{record_id,int}]}
                                ,{ ?FIELD_URI, field_handler, []}
                               ] }
                       ]).

-define(INIT_STORAGE, rerest_mnesia_store:init).
-define(CREATE, rerest_mnesia_store:create).
-define(READ, rerest_mnesia_store:read).
-define(UPDATE, rerest_mnesia_store:update).
-define(DELETE, rerest_mnesia_store:delete).

-define(RRIF_SCHEMA(X), (fun({{S,_},_}) -> S end)(X)).

-type(json() :: binary()).

-type(rrif() :: {schema_node(),{list(),data_flags()}}).
-type(schema_node() :: {schema(),schema_flags()}).
-type(schema() :: map()).
-type(schema_flags() :: list()).
-type(data_flags() :: list()).

-type(data_id() :: binary()).
-type(schema_id() :: binary()).

-type(table_name() :: atom()).
