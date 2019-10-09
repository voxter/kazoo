%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc DB Schema used by PostgreSQL kazoo storage driver
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_schema).
-include("kz_postgresql.hrl").
-export([db_and_pvt_type_to_pg_table_name/2
        ,pg_view_name_to_pg_table_name/1
        ,get_schema/2
        ]).

%%------------------------------------------------------------------------------
%% @doc Define the postgreSQL table to use for an couch like KazooDBName and Doc Type
%% If no table is defined the default table used is "other"
%% @end
%%------------------------------------------------------------------------------
-spec db_and_pvt_type_to_pg_table_name(kz_postgresql:kazoo_db_name(), kz_term:ne_binary()) -> kz_postgresql:table_name().
db_and_pvt_type_to_pg_table_name(_KazooDBName, <<"call_stat">>) -> <<"call_stat">>;
db_and_pvt_type_to_pg_table_name(_KazooDBName, <<"cdr">>) -> <<"cdr">>;
db_and_pvt_type_to_pg_table_name(_KazooDBName, <<"login_attempt">>) -> <<"auth">>;
db_and_pvt_type_to_pg_table_name(_KazooDBName, <<"status_stat">>) -> <<"status_stat">>;
db_and_pvt_type_to_pg_table_name(_KazooDBName, _DocType) -> <<"other">>.

%%------------------------------------------------------------------------------
%% @doc Define the postgreSQL table to use for a given view name
%% If no table is defined the default table will be assumed that the DesignDoc
%% is the plural of the table name.
%% Pg Views are defined as DesignDoc~ViewName
%% @end
%%------------------------------------------------------------------------------
-spec pg_view_name_to_pg_table_name(kz_term:ne_binary()) -> kz_term:ne_binary().
pg_view_name_to_pg_table_name(<<"agent_stats~",_>>) -> <<"status_stat">>;
pg_view_name_to_pg_table_name(ViewName) ->
    case binary:split(ViewName, <<"~">>) of
        [_NotAView] -> ViewName;
        [TableNamePlural, _View] ->
            kz_postgresql_util:depluralize_table_name(TableNamePlural)
    end.

%%------------------------------------------------------------------------------
%% @doc Return a list of column names and data type for a PG Table or View
%% [{ColName, Type}]
%% @end
%%------------------------------------------------------------------------------
-spec get_schema(kz_postgresql:connection_pool(), kz_postgresql:table_name() | kz_postgresql:view_name()) ->
                        kz_postgresql:table_schema().
get_schema(ConnPool, TableOrViewName) ->
    case fetch_cache_table_schema(TableOrViewName) of
        {'error', 'not_found'} ->
            lager:debug("postgres table schema for table '~p' was not found in cache, querying schema", [TableOrViewName]),
            QuerySchama = #kz_postgresql_query{'select' = [<<"COLUMN_NAME">>
                                                          ,<<"DATA_TYPE">>
                                                          ]
                                              ,'from' = [<<"information_schema.COLUMNS">>]
                                              ,'where' = {<<"=">>, [<<"TABLE_NAME">>
                                                                   ,<<"$1">>
                                                                   ]
                                                         }
                                              ,'parameters' = [TableOrViewName]
                                              },
            {'ok', _, Columns} = kz_postgresql_query:execute_query(ConnPool, QuerySchama),
            cache_table_schema(TableOrViewName, Columns);
        {'ok', Columns} ->
            lager:debug("postgres table schema for table '~p' was found in cache", [TableOrViewName]),
            Columns
    end.

%%------------------------------------------------------------------------------
%% @doc Cache a PG Table Schema
%% @end
%%------------------------------------------------------------------------------
-spec cache_table_schema(kz_postgresql:table_name(), list(tuple())) -> list(tuple()).
cache_table_schema(TableName, Schema) ->
    lager:debug("caching table schema for table '~p'", [TableName]),
    CacheProps = [{'expires',?KAZOO_POSTGRESQL_CACHE_TIMEOUT}],
    kz_cache:store_local(?KAZOO_POSTGRESQL_SCHEMA_CACHE, TableName, Schema, CacheProps),
    Schema.

%%------------------------------------------------------------------------------
%% @doc Fetch a PG table schema from cache by Table Name
%% @end
%%------------------------------------------------------------------------------
-spec fetch_cache_table_schema(kz_postgresql:table_name())-> {'ok', list(tuple())} |
                                                             {'error', 'not_found'}.
fetch_cache_table_schema(TableName) ->
    kz_cache:fetch_local(?KAZOO_POSTGRESQL_SCHEMA_CACHE, TableName).
