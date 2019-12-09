%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc Postgresql connection functions
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-ifndef(KZ_POSTGRESQL_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(KAZOO_POSTGRESQL_QUERIES_CACHE, 'kazoo_postgresql_queries').
-define(KAZOO_POSTGRESQL_SCHEMA_CACHE, 'kazoo_postgresql_schema').
-define(KAZOO_POSTGRESQL_CACHE_TIMEOUT, 3600).

-define(PG_NULL, 'null').       %% NULL value used for postgreSQL query's
-define(DOC_NULL, 'null').      %% NULL value used when building JObject
-define(PG_LOOKUP_TABLE_NAME, <<"lookup">>).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 5432).
-define(DEFAULT_TIMEOUT_S, 4000).
-define(DEFAULT_DATABASE, "kazoo").
-define(DEFAULT_CONN_POOL_SIZE, 10).
-define(DEFAULT_CONN_POOL_MAX_OVERFLOW, 3).

-type connection_pool() :: atom().                                                          %% PostgreSQL connection pool id
-type table_schema() :: [{kz_term:ne_binary(), kz_term:ne_binary()}].                       %% List of {ColumnName, ColumnType}
-type table_name() :: kz_term:ne_binary().                                                  %% PostgreSQL table name
-type view_name() :: kz_term:ne_binary().                                                   %% DesignName~ViewName
-type column_name() :: kz_term:ne_binary().                                                 %% PostgreSQL column name
-type column_names() :: [column_name()].
-type kazoo_db_name() :: kz_term:ne_binary().                                               %% Database name assigned by kazoo
-type value() :: kz_term:ne_binary().                                                       %% PostgreSQL query VALUE
-type values() :: [value()].
-type query() :: iolist().                                                                  %% Converted iolist of kz_postgresql:query_record()
-type where_clause() :: {kz_term:ne_binary(), list(where_clause() | kz_term:ne_binary())}.  %% AST rep of a PostgreSQL WHERE clause
-type sort_operator() :: kz_term:ne_binary().                                               %% <<"ASC">> | <<"DESC">> | <<"COLLATE \"C\" ASC">> | <<"COLLATE \"C\" DESC">>
-type order_by() :: {column_name(), sort_operator()}.                                       %% {Column Name, sort_operator()}
-type inner_join() :: {table_name(), nonempty_list(kz_term:ne_binary())}.                   %% {Table Name, JoinString(s)}
-type insert_into() :: {table_name(), column_names()}.                                      %% {Table Name, List of Columns}

-record(kz_postgresql_connection, {id = kz_time:current_tstamp() :: kz_time:gregorian_seconds()
                                  ,host = ?DEFAULT_HOST :: string()
                                  ,port = ?DEFAULT_PORT :: integer()
                                  ,username = "" :: string()
                                  ,password = "" :: string()
                                  ,database = ?DEFAULT_DATABASE :: string()
                                  ,timeout = ?DEFAULT_TIMEOUT_S :: integer()
                                  ,conn_pool_size = ?DEFAULT_CONN_POOL_SIZE :: integer()
                                  ,conn_pool_max_overflow = ?DEFAULT_CONN_POOL_MAX_OVERFLOW :: integer()
                                  }).
-type postgresql_connection() :: #kz_postgresql_connection{}.
-type postgresql_connections() :: [postgresql_connection()].

-record(kz_postgresql_query, {'select' = [] :: column_names()                                       %% SELECT part of PG query
                             ,'from' = [] :: [table_name() | view_name()]                           %% FROM part of SELECT PG query
                             ,'insert_into' = 'undefined' :: 'undefined' | insert_into()            %% INSERT INTO part of PG query
                             ,'values' = [] :: [values()]                                           %% VALUES part of INSERT INTO PG query
                             ,'update' = 'undefined' :: 'undefined' | table_name()                  %% UPDATE part of PG query
                             ,'set' = [] :: [{column_name(), value()}]                              %% SET part of PG query [{ColumnName, Value}]
                             ,'delete_from' = 'undefined' :: 'undefined' | table_name()             %% DELETE FROM part of PG query
                             ,'where' = 'undefined' :: 'undefined' | where_clause()                 %% WHERE part of PG query (AST format)
                             ,'group_by' = [] :: column_names()                                     %% GROUP BY part of SELECT PG query
                             ,'order_by' = [] :: [order_by()]                                       %% ORDER BY part of SELECT PG query
                             ,'returning' = [] :: column_names()                                    %% RETURNING part of SELECT PG query
                             ,'limit' = 'undefined' :: kz_term:api_integer()                        %% LIMIT part of SELECT PG query
                             ,'offset' = 'undefined' :: kz_term:api_integer()                       %% OFFSET part of SELECT PG query
                             ,'inner_join' = 'undefined' :: 'undefined' | inner_join()              %% INNER JOIN ON part of PG query
                             ,'parameters' = [] :: values()                                         %% List of Parameters to fill $1..$n
                             }).
-type query_record() :: #kz_postgresql_query{}.
-endif.
