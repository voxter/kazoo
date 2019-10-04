%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc PostgreSQL Couch like Database operations
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_db).
-include("kz_postgresql.hrl").
-export([db_delete/2
        ,db_exists/2
        ,db_list/2
        ]).

%%------------------------------------------------------------------------------
%% @doc Delete all rows in postgresql db that are assocated with the couch like db name
%% This will use the lookup table to find all rows (docs) with the associated table name and delete those rows (docs)
%% @end
%%------------------------------------------------------------------------------
-spec db_delete(kz_postgresql:connection_pool(), kz_term:ne_binary()) -> boolean().
db_delete(ConnPool, DbName) ->
    lager:debug("deleting all docs for couch like database: ~p in postgresql database", [DbName]),
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, DbName) of
        {'error', Cause} ->
            lager:error("error when deleting couch like database: ~p in postgresql database, Cause: ~p", [DbName, Cause]),
            'false';
        {'ok', []} ->
            lager:debug("couch like db does not exists in lookup table"),
            'false';
        {'ok', PgTablesAndDocIds} ->
            %% Generate the doc in the correct format
            %% Add pvt_type to help reduce the number of requests to the lookup table when deleing docs
            %% NOTE this assumes that table name == pvt_type
            %% Better way would be to pass the table name(s) to kz_postgresql_doc:del_docs but this will do for now
            Fun = fun(TableName, DocIds) -> lists:map(fun(DocId) -> Doc = kz_doc:set_id(kz_json:new(), DocId),
                                                                    kz_json:set_value(<<"pvt_type">>, TableName, Doc)
                                                      end
                                                     ,DocIds)
                  end,
            Docs = lists:foldl(fun({TableName, DocIds}, Docs) -> Fun(TableName, DocIds) ++ Docs end, [], PgTablesAndDocIds),
            case kazoo_postgresql:del_docs(ConnPool, DbName, Docs, []) of
                {'ok', _} ->
                    lager:debug("deleted all docs for the db ~p in postgresql db", [DbName]),
                    'true';
                {'error', Cause} ->
                    lager:error("error when deleting all docs for the db ~p, Cause: ~p", [DbName, Cause]),
                    'false'
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Verify if a couch like DB exists in the PG database
%% Uses the lookup table to verify if the dbname exists
%% @end
%%------------------------------------------------------------------------------
-spec db_exists(kz_postgresql:connection_pool(), kz_term:ne_binary()) -> boolean().
db_exists(ConnPool, DbName) ->
    lager:debug("checking if couch like database (~p) exists in postgresql database", [DbName]),
    Query = #kz_postgresql_query{'select' = [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".doc_id">>]
                                ,'from' = [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\"">>]
                                ,'where' = {<<"=">>, [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\".db_name">>
                                                     ,<<"$1">>
                                                     ]
                                           }
                                ,'parameters' = [DbName]
                                },
    case kz_postgresql_query:execute_query(ConnPool, Query) of
        {'ok', _, []} -> 'false';
        {'ok', _Columns, _Rows} -> 'true';
        {'error', _}=Error ->
            lager:error("postgresql query (~p) failed, Error: ~p", [Query, Error]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc List all the couch like dbs in the postgresql database
%% This will get all the db_names from the lookup table
%% @end
%%------------------------------------------------------------------------------
-spec db_list(kz_postgresql:connection_pool(), kz_data:options()) -> {'ok', kz_term:ne_binaries()} | kz_data:data_error().
db_list(ConnPool, Options) ->
    lager:debug("getting a list of all couch like dbs in postgresql db"),
    Query = #kz_postgresql_query{'select' = [<<"DISTINCT(\"",?PG_LOOKUP_TABLE_NAME/binary,"\".db_name)">>]
                                ,'from' = [<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\"">>]
                                },
    case kz_postgresql_query:execute_query(ConnPool, Query, Options) of
        {'ok', _, []} ->
            lager:debug("could not find any couch like dbs in the lookup table"),
            {'error', 'no_results'};
        {'ok', _Columns, Rows} ->
            {'ok', lists:map(fun({DbName}) -> DbName end, Rows)};
        {'error', _}=Error ->
            lager:error("postgresql query (~p) failed, Error: ~p", [Query, Error]),
            {'error', kz_postgresql_response:format_error(Error)}
    end.
