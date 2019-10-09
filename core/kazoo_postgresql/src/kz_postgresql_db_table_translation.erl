%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc Kazoo / Couch DB to/from PostgreSQL TableName translation
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_db_table_translation).
-include("kz_postgresql.hrl").
-export([get_table_names/2, get_table_names/3
        ]).

%%------------------------------------------------------------------------------
%% @doc Lookup the postgreSQL table name and doc ids for a given KazooDBName
%% Response will be in the form {'ok', [{TableName, [Doc_Ids...]}...} or {'error', Cause}
%% @end
%%------------------------------------------------------------------------------
-spec get_table_names(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()) ->
                             {'ok', list({kz_postgresql:table_name(), kz_term:ne_binaries()})} | kz_data:data_error().
get_table_names(ConnPool, KazooDBName)->
    get_table_names_from_pgsql_db(ConnPool, KazooDBName, []).

%%------------------------------------------------------------------------------
%% @doc Lookup the postgreSQL table name from the pvt_type for a given KazooDBName and Doc.
%% If pvt_type is not defined then lookup the table name from the PG DB
%% If [] is supplied as DocIds then a response containing all doc ids and tables for the DB will be returned
%% Response will be in the form {'ok', [{TableName, [Doc_Ids...]}...} or {'error', Cause}
%% @end
%%------------------------------------------------------------------------------
-spec get_table_names(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary() | kz_term:ne_binaries() | kz_doc:object()) ->
                             {'ok', list({kz_postgresql:table_name(), kz_term:ne_binaries()})} | kz_data:data_error().
%% Doc ID supplied
get_table_names(ConnPool, KazooDBName, DocId) when is_binary(DocId)->
    get_table_names_from_pgsql_db(ConnPool, KazooDBName, [DocId]);

%% List of Doc Ids supplied
get_table_names(ConnPool, KazooDBName, DocIds) when is_list(DocIds) ->
    get_table_names_from_pgsql_db(ConnPool, KazooDBName, DocIds);

%% Doc supplied
get_table_names(ConnPool, KazooDBName, Doc) ->
    case kz_doc:type(Doc) of
        'undefined' -> get_table_names_from_pgsql_db(ConnPool, KazooDBName, [kz_doc:id(Doc, Doc)]);
        DocType -> {'ok', [{kz_postgresql_schema:db_and_pvt_type_to_pg_table_name(KazooDBName, DocType), [kz_doc:id(Doc, Doc)]}]}
    end.

%%------------------------------------------------------------------------------
%% @doc Lookup the postgreSQL table name(s) for a given couch like KazooDBName and DocId(optional)
%% PostgreSQL 'lookup' table is used to define the table for every Doc_id and KazooDBName in the PG database
%% Response will be in the form {'ok', [{TableName, [Doc_Ids...]}...} or {'error', Cause}
%% @end
%%------------------------------------------------------------------------------
-spec get_table_names_from_pgsql_db(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binaries()) ->
                                           {'ok', list({kz_postgresql:table_name(), list()})} |kz_data:data_error().
%% No Doc Ids defined, Return list of PG tabes for all Doc Ids related to the db name
get_table_names_from_pgsql_db(ConnPool, KazooDBName, []) ->
    lager:debug("looking up postgreSQL table name(s) in db for KazooDBName: ~p, No DocIds supplied", [KazooDBName]),
    Query = #kz_postgresql_query{'select' = [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".table_name">>
                                            ,<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".doc_id">>
                                            ]
                                ,'from' = [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\"">>]
                                ,'where' = {<<"=">>, [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".kazoo_db_name">>
                                                     ,<<"$1">>
                                                     ]
                                           }
                                ,'parameters' = [KazooDBName]
                                },
    do_get_table_names_from_pgsql_db(ConnPool, Query);
get_table_names_from_pgsql_db(ConnPool, KazooDBName, DocIds) when is_list(DocIds) ->
    lager:debug("looking up postgreSQL table name(s) in db for KazooDBName: ~p, doc ids: ~p", [KazooDBName, DocIds]),
    Query = #kz_postgresql_query{'select' = [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".table_name">>
                                            ,<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".doc_id">>
                                            ]
                                ,'from' = [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\"">>]
                                ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".kazoo_db_name">>
                                                                  ,<<"$1">>
                                                                  ]}
                                                       ,{<<"=">>, [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".doc_id">>
                                                                  ,<<"ANY($2)">>
                                                                  ]}
                                                       ]
                                           }
                                ,'parameters' = [KazooDBName, DocIds]
                                },
    do_get_table_names_from_pgsql_db(ConnPool, Query).

-spec do_get_table_names_from_pgsql_db(kz_postgresql:connection_pool(), kz_types:ne_binary()) ->
                                              {'ok', list({kz_postgresql:table_name(), list()})} | kz_data:data_error().
do_get_table_names_from_pgsql_db(ConnPool, Query) ->
    case kz_postgresql_query:execute_query(ConnPool, Query) of
        {'ok', _, []} ->
            lager:debug("failed to find doc in postgresql 'lookup' table"),
            {'error', 'not_found'};
        {'ok', _Columns, _Rows} = OkResp ->
            {'ok', group_doc_ids_by_table_name(OkResp)};
        {'error', _}=Error ->
            lager:error("postgresql query (~p) failed, Error: ~p", [Query, Error]),
            {'error', kz_postgresql_response:format_error(Error)}
    end.

%%------------------------------------------------------------------------------
%% @doc Convert epgsql OK respose to a list of tuples grouped by there table name
%% Rows must be in the form [{TableName, DocId}...]
%% @end
%%------------------------------------------------------------------------------
-spec group_doc_ids_by_table_name(epgsql:ok_reply()) -> list({kz_postgresql:table_name(), list()}).
group_doc_ids_by_table_name({'ok', _Columns, Rows}) ->
    group_doc_ids_by_table_name(Rows, []).
group_doc_ids_by_table_name([], DocsByTable) ->
    DocsByTable;
group_doc_ids_by_table_name([{TableName, DocId} | Rows], DocsByTable) ->
    case lists:keytake(TableName, 1, DocsByTable) of
        {'value', {TableName, DocIds}, Acc1} ->
            NewDocsByTable = lists:merge(Acc1, [{TableName, DocIds ++ [DocId]}]),
            group_doc_ids_by_table_name(Rows, NewDocsByTable);
        'false' ->
            NewDocsByTable = lists:merge(DocsByTable, [{TableName, [DocId]}]),
            group_doc_ids_by_table_name(Rows, NewDocsByTable)
    end.
