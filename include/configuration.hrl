%% [ {URIHost, list({URIPath, Handler, Opts})} | _ ]
-define(ROOT_URI, "/").
-define(STATIC_URI, ?ROOT_URI ++ "static").
-define(DOCS_URI, ?ROOT_URI ++ "docs").

-define(RECORD_URI, ?ROOT_URI ++ ":record_id").
-define(FIELD_URI, ?RECORD_URI ++ "/:field").



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

