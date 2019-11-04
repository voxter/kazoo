%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc Postgresql view related functions
%%% Couch like view results are generated and returned by this module from postgresql view queries
%%% The following should be followed when generating postgresql views:
%%% The view must be named DESIGN_NAME~VIEW_NAME as it would be defined in couchdb
%%% Its assumed that DESIGN_NAME is the plural of the associated Table Name, unless override defined in kz_postgresql_schema:view_name_to_table_name/1
%%% The view must return a column "_view_id" that will be placed in the "id" part of the JSON response
%%% The view must return a column "_view_rev" that will be used by include_docs inner join to return the correct doc rev row from the table
%%% The view must return '_view_db_name' column that is used to filter results for a particular database
%%% The view must return at least one column beginning with _view_key_N  that that will be placed in the "key" part of the JSON response
%%% The view can return columns beginning with _view_value_COLUMN_NAME  that that will be placed in the "value" map part of the JSON response
%%% The view can alternatively return one column _view_value that will be the only value placed in the "value" map part of the JSON response
%%% All other columns will be considered to be part of the JSON doc and will be added to the "doc" key of the response, (include_docs defined)
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_view).
-include("kz_postgresql.hrl").
-export([get_results/4
        ,all_docs/3
        ,is_view/1
        ]).

%%------------------------------------------------------------------------------
%% @doc Load a PG view from the DB and return a couch view like response
%%
%% ConnPool - The PG connection
%% KazooDBName - The couch like DB name
%% DesignDoc - The couch like DesignName/ViewName
%% Options - Couch like query options
%% @end
%%------------------------------------------------------------------------------
-spec get_results(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_data:options()) ->
                         {'ok', kz_json:objects()} | kz_data:data_error().
get_results(ConnPool, KazooDBName, DesignDoc, ViewOptions) ->
    lager:debug("get_results called with args, KazooDBName: ~p, DesignDoc: ~p, ViewOptions: ~p", [KazooDBName, DesignDoc, ViewOptions]),
    do_view_query(ConnPool, KazooDBName, DesignDoc, ViewOptions).

%%------------------------------------------------------------------------------
%% @doc Execute the PG view query
%%
%% ConnPool - The PG DB connection
%% KazooDBName -  Used as a input to the view query to only load data related to this DB / Account
%% DesignDoc - Used to define the view query to load. Eg cdr/crossbar_listing
%% Options - Couch like query options
%% @end
%%------------------------------------------------------------------------------
-spec do_view_query(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:doc(), kz_data:options()) ->
                           {'ok', kz_json:objects()} | kz_data:data_error().
do_view_query(ConnPool, KazooDBName, DesignDoc, ViewOptions) when is_binary(DesignDoc) ->
    [DesignName, ViewName|_] = binary:split(DesignDoc, <<"/">>, ['global']),
    do_view_query(ConnPool, KazooDBName, {DesignName, ViewName}, ViewOptions);
do_view_query(ConnPool, KazooDBName,{DesignName, ViewName}, ViewOptions) ->
    View = <<DesignName/binary,"~",ViewName/binary>>,
    Query = #kz_postgresql_query{'select' = [<<"\"",View/binary,"\".*">>]
                                ,'from' = [<<"\"",View/binary,"\"">>]
                                ,'where' = {<<"=">>, [<<"\"",View/binary,"\"._view_db_name">>
                                                     ,<<"$1">>
                                                     ]
                                           }
                                ,'parameters' = [KazooDBName]
                                },
    PGResp = kz_postgresql_query:execute_query(ConnPool, Query, ViewOptions),
    kz_postgresql_response:parse_response_to_view_doc(PGResp).

%%------------------------------------------------------------------------------
%% @doc Load all docs for a given couch db from the PG DB and return a couch view like response
%% First the lookup table will be used to find the PG tables needed. Then doc ids will
%% be grouped by table names if doc_ids are supplied in the 'keys' ViewOptions
%% Then the docs are loaded table by table and combined and sorted according either
%% ASC or DESC in ViewOptions
%%
%% ConnPool - The PG connection
%% KazooDBName - The couch like DB name
%% Options - Couch Db query parameters
%%
%% NOTE This is a heavy request and should be avoided if possible
%% NOTE Only support for include_doc, ASC, DESC, keys in ViewOptions
%% @end
%%------------------------------------------------------------------------------
-spec all_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:options()) ->
                      {'ok', kz_json:objects()} | kz_data:data_error().
all_docs(ConnPool, KazooDBName, ViewOptions) ->
    lager:debug("loading all_docs view for KazooDBName: ~p with ViewOptions: ~p (ConnPool: ~p)", [KazooDBName, ViewOptions, ConnPool]),
    %% Extract keys from the options if defined so we can work out what PG tables to query
    DocIds = kz_postgresql_options:get_keys(ViewOptions, []),
    %% Get a list of all the PG tables and there corresponding doc ids
    case get_postgresql_table_names(ConnPool, KazooDBName, DocIds, ViewOptions) of
        {'error', Cause} = Error ->
            lager:error("failed to load all docs view, cause: ~p", [Cause]),
            Error;
        {'ok', PgTablesAndDocIds} ->
            lager:debug("postgresql table names and doc ids to query: ~p", [PgTablesAndDocIds]),
            %% Go through each table and load the docs, table by table
            do_all_doc_query(ConnPool, KazooDBName, PgTablesAndDocIds, ViewOptions)
    end.

%%------------------------------------------------------------------------------
%% @doc Calculate the PG table(s) that contain the doc ids for the given KazooDBName supplied
%% or all the PG table(s) for a given KazooDBName when no doc_ids are supplied.
%%
%% If doc_type is defined in the view options it will return the corresponding PG table name
%% else it will call kz_postgresql_db_table_translation:get_table_names() and
%% get the table names from the lookup table
%% @end
%%------------------------------------------------------------------------------
-spec get_postgresql_table_names(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binaries(), kz_data:options()) ->
                                        {'ok', list({kz_postgresql:table_name(), kz_term:ne_binaries()})} | kz_data:data_error().
get_postgresql_table_names(ConnPool, KazooDBName, DocIds, ViewOptions) ->
    lager:debug("calculating tables that contain rows for KazooDBName ~p", [KazooDBName]),
    case kz_postgresql_options:get_doc_type(ViewOptions) of
        'undefined' ->
            lager:debug("doc_type is not defined in the view options, looking up pg table names in pg lookup table"),
            kz_postgresql_db_table_translation:get_table_names(ConnPool, KazooDBName, DocIds);
        DocType when is_list(DocType) ->
            lager:debug("multiple doc_types are defined in the view options, ignoring this and looking up pg table names in pg lookup table"),
            kz_postgresql_db_table_translation:get_table_names(ConnPool, KazooDBName, DocIds);
        DocType ->
            lager:debug("the doc_type is defined in the view options (~p), useing this to calcualte the pg table", [DocType]),
            PgTableName = kz_postgresql_schema:db_and_pvt_type_to_pg_table_name(KazooDBName, DocType),
            {'ok', [{PgTableName, DocIds}]}
    end.

%%------------------------------------------------------------------------------
%% @doc For a given list of PG table names and their corresponding DocIds,
%% Load the _id, _id as _key_0 and _rev as _value_rev of each doc and return a couch
%% like all_docs JObject
%% @end
%%------------------------------------------------------------------------------
-spec do_all_doc_query(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()
                      ,list({kz_postgresql:table_name(), kz_term:ne_binaries()}), kz_data:options()) ->
                              {'ok', kz_json:objects()} |  kz_data:data_error().
do_all_doc_query(ConnPool, KazooDBName, PgTablesAndDocIds, ViewOptions) ->
    do_all_doc_query(ConnPool, KazooDBName, PgTablesAndDocIds, ViewOptions, []).

-spec do_all_doc_query(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()
                      ,list({kz_postgresql:table_name(), kz_term:ne_binaries()}), boolean(), kz_data:options()) ->
                              {'ok', kz_json:objects()} | kz_data:data_error().
do_all_doc_query(_ConnPool, _KazooDBName, [], ViewOptions, JObjs) ->
    case kz_postgresql_options:get_order(ViewOptions, 'undefined') of
        'ascending' ->
            lager:debug("sorting combined all_doc view results to ascending order"),
            {'ok', kz_json:sort(fun(A, B) -> kz_doc:id(A) > kz_doc:id(B) end, JObjs)};
        'descending' ->
            lager:debug("sorting combined all_doc view results to descending order"),
            {'ok', kz_json:sort(fun(A, B) -> kz_doc:id(A) < kz_doc:id(B) end, JObjs)};
        'undefined' ->
            lager:debug("no sort order defined, sorting keys as defined by keys in view options"),
            IndexedKeyMap = lists:foldl(fun(Key, Acc) -> Acc#{Key => maps:size(Acc)} end
                                       ,#{}
                                       ,kz_postgresql_options:get_keys(ViewOptions)),
            SortFun = fun(A, B) ->
                              maps:find(kz_doc:id(A), IndexedKeyMap) < maps:find(kz_doc:id(B), IndexedKeyMap)
                      end,
            {'ok', kz_json:sort(SortFun, JObjs)}
    end;

do_all_doc_query(ConnPool, KazooDBName, [{TableName, DocIds} | OtherTables], ViewOptions, JObjs) ->
    Query = generate_all_docs_table_query(KazooDBName, TableName, DocIds),
    %% We only care about include_doc option for this part of the query
    ReducedOptions = case kz_postgresql_options:get_include_docs(ViewOptions) of
                         'true' -> ['include_docs'];
                         'false' -> []
                     end,
    PGResp = kz_postgresql_query:execute_query(ConnPool, Query, ReducedOptions),
    case kz_postgresql_response:parse_response_to_view_doc(PGResp) of
        {'ok', NewJObjs} when is_list(NewJObjs) ->
            do_all_doc_query(ConnPool, KazooDBName, OtherTables, ViewOptions, JObjs ++ NewJObjs);
        {'ok', NewJObj} ->
            do_all_doc_query(ConnPool, KazooDBName, OtherTables, ViewOptions, JObjs ++ [NewJObj]);
        {'error', _} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Generate a PG query record to load data from a table
%% @end
%%------------------------------------------------------------------------------
-spec generate_all_docs_table_query(kz_postgresql:kazoo_db_name(), kz_postgresql:table_name(), kz_term:ne_binaries()) ->
                                           kz_postgresql:query_record().
generate_all_docs_table_query(KazooDBName, TableName, []) ->
    #kz_postgresql_query{'select' = [<<"\"",TableName/binary,"\"._id AS _view_id">>
                                    ,<<"\"",TableName/binary,"\"._id  AS _view_key_0">>
                                    ,<<"\"",TableName/binary,"\"._rev AS _view_value_rev">>
                                    ]
                        ,'from' = [<<"\"",TableName/binary,"\"">>]
                        ,'where' = {<<"=">>, [<<"\"",TableName/binary,"\".kazoo_db_name">>
                                             ,<<"$1">>
                                             ]
                                   }
                        ,'parameters' = [KazooDBName]
                        };
generate_all_docs_table_query(KazooDBName, TableName, DocIds) ->
    #kz_postgresql_query{'select' = [<<"\"",TableName/binary,"\"._id AS _view_id">>
                                    ,<<"\"",TableName/binary,"\"._id  AS _view_key_0">>
                                    ,<<"\"",TableName/binary,"\"._rev AS _view_value_rev">>
                                    ]
                        ,'from' = [<<"\"",TableName/binary,"\"">>]
                        ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"",TableName/binary,"\".kazoo_db_name">>
                                                          ,<<"$1">>
                                                          ]}
                                               ,{<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                          ,<<"ANY($2)">>
                                                          ]}
                                               ]
                                   }
                        ,'parameters' = [KazooDBName, DocIds]
                        }.

%%------------------------------------------------------------------------------
%% @doc Return true if the view name indicates its a view, else return false
%% @end
%%------------------------------------------------------------------------------
-spec is_view(kz_tern:ne_binary()) -> boolean().
is_view(ViewName) ->
    case binary:split(ViewName, <<"~">>) of
        [_NotAView] -> 'false';
        [_TableNamePlural, _View] -> 'true'
    end.