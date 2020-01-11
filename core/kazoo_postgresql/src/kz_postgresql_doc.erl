%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc Postgresql document related functions
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_doc).
-include("kz_postgresql.hrl").
-export([lookup_doc_rev/3
        ,open_doc/3, open_doc/4
        ,save_doc/3, save_doc/4
        ,save_docs/3, save_docs/4
        ,del_doc/3, del_doc/4
        ,del_docs/3, del_docs/4
        ]).

%%------------------------------------------------------------------------------
%% @doc Lookup couch like doc revision with couch like KazooDBName and Doc or DocId
%% Get the highest revision number for a given doc id.
%% Note, preference should be given to calling this with a kz_doc object
%% Note, rev will be returned if the doc _deleted = true or false
%% @end
%%------------------------------------------------------------------------------
-spec lookup_doc_rev(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary() | kz_data:document()) ->
          {'ok', kz_term:ne_binary()} | kz_data:data_error().
lookup_doc_rev(ConnPool, KazooDBName, DocOrId) ->
    %% Get the relevant postgresql Table name
    lager:debug("looking up doc rev for doc ~p", [DocOrId]),
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, KazooDBName, DocOrId) of
        {'ok', [{TableName, [DocId]}]} ->
            lookup_doc_rev_by_pg_table_name(ConnPool, TableName, DocId);
        {'error', Cause} = Error ->
            lager:debug("failed to lookup the postgresql table name for KazooDBName: ~p, DocOrId: ~p, Cause: ~p", [KazooDBName, DocOrId, Cause]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Lookup couch like doc revision by postgresql table name and doc id
%% @end
%%------------------------------------------------------------------------------
-spec lookup_doc_rev_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:table_name(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} | kz_data:data_error().
lookup_doc_rev_by_pg_table_name(ConnPool, TableName, DocId) ->
    lager:debug("looking up doc revision for DocId ~p in pg table ~p", [DocId, TableName]),
    Query = #kz_postgresql_query{'select' = [<<"\"",TableName/binary,"\"._id">>
                                            ,<<" \"",TableName/binary,"\"._rev">>
                                            ]
                                ,'from' = [<<"\"",TableName/binary,"\"">>]
                                ,'where' = {<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                     ,<<"$1">>
                                                     ]
                                           }
                                ,'parameters' = [DocId]
                                },
    PGResp = kz_postgresql_query:execute_query(ConnPool, Query),
    case kz_postgresql_response:parse_response_to_doc(TableName, PGResp) of
        {'ok', JObj} -> {'ok', kz_doc:revision(JObj)};
        {'error', _} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Open and return row as a JSON doc when privided with a couch like Db name and Doc id
%% @end
%%------------------------------------------------------------------------------
-spec open_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} | kz_data:data_error().
open_doc(ConnPool, KazooDBName, DocId) ->
    open_doc(ConnPool, KazooDBName, DocId, []).

-spec open_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_data:options()) ->
          {'ok', kz_json:object()} | kz_data:data_error().
open_doc(ConnPool, KazooDBName, DocId, Options) ->
    %% Get the relevant postgresql Table name
    %% TODO Maybe use Options (expected_doc_type) to calculate table name?
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, KazooDBName, DocId) of
        {'ok', [{TableName, [DocId]}]} ->
            open_doc_by_pg_table_name(ConnPool, KazooDBName, TableName, DocId, Options);
        {'error', Cause} = Error ->
            lager:error("failed to lookup the postgresql table name for KazooDBName: ~p, DocId: ~p, Cause: ~p", [KazooDBName, DocId, Cause]),
            {'error', kz_postgresql_response:format_error(Error)}
    end.

%%------------------------------------------------------------------------------
%% @doc Open postgresql row and return json doc when privided with a postgres table name and Doc id
%% @end
%%------------------------------------------------------------------------------
-spec open_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_postgresql:table_name()
                               ,kz_term:ne_binary(), kz_data:options()) ->
          {'ok', kz_json:object()} | kz_data:data_error().
open_doc_by_pg_table_name(ConnPool, KazooDBName, TableName, DocId, Options) ->
    Query = #kz_postgresql_query{'select' = [<<"*">>]
                                ,'from' = [<<"\"",TableName/binary,"\"">>]
                                ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                                  ,<<"$1">>
                                                                  ]}
                                                       ,{<<"=">>, [<<"\"",TableName/binary,"\".kazoo_db_name">>
                                                                  ,<<"$2">>
                                                                  ]}
                                                       ]}
                                ,'parameters' = [DocId, KazooDBName]
                                },
    PGResp = kz_postgresql_query:execute_query(ConnPool, Query, Options),
    kz_postgresql_response:parse_response_to_doc(TableName, PGResp).

%%------------------------------------------------------------------------------
%% @doc Save a JSON doc to postgresql table
%% @end
%%------------------------------------------------------------------------------
-spec save_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document()) ->
          {'ok', kz_json:object()} | kz_data:data_error().
save_doc(ConnPool, KazooDBName, Doc) ->
    save_doc(ConnPool, KazooDBName, Doc, []).

-spec save_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document(), kz_data:options()) ->
          {'ok', kz_json:object()} | kz_data:data_error().
save_doc(ConnPool, KazooDBName, Doc, Options) ->
    lager:debug("saving doc ~p (KazooDBName: ~p)", [kz_doc:id(Doc), KazooDBName]),
    {PGResp, TableName} = do_save_doc(ConnPool, KazooDBName, Doc, Options),
    kz_postgresql_response:parse_response_to_doc(TableName, PGResp).

-spec do_save_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document(), kz_data:options()) ->
          {epgsql:reply(), kz_postgresql:table_name()}.
do_save_doc(ConnPool, KazooDBName, Doc, Options) ->
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, KazooDBName, Doc) of
        {'error', Cause} = Error ->
            lager:error("failed to find postgresql table for doc, Doc: ~p, Cause: ~p", [Doc, Cause]),
            {Error, 'undefined'};
        {'ok', [{TableName, _}]} ->
            {insert_or_update_doc_by_pg_table_name(ConnPool, KazooDBName, TableName, Doc, Options), TableName}
    end.

%%------------------------------------------------------------------------------
%% @doc Save multiple JSON docs to postgresql table
%% Expected bulk response:
%% {ok,[{[{ok,true}
%%       ,{id,abcdefgh...}
%%       ,{rev,1-12879c2017cb8f00a16a865dc6f92fe1}]}
%%     ,{[{id,xyx...}
%%       ,{error,conflict}
%%       ,{reason,Document update conflict.}]}]}
%% @end
%%------------------------------------------------------------------------------
-spec save_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:documents()) ->
          {'ok', kz_json:objects()} | kz_data:data_error().
save_docs(ConnPool, KazooDBName, Docs) ->
    save_docs(ConnPool, KazooDBName, Docs, []).

-spec save_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:documents(), kz_data:options()) ->
          {'ok', kz_json:objects()} | kz_data:data_error().
save_docs(ConnPool, KazooDBName, Docs, Options) ->
    lager:debug("saving multiple docs (one by one) (KazooDBName: ~p)", [KazooDBName]),
    save_docs_fold(ConnPool, KazooDBName, Docs, Options, []).

-spec save_docs_fold(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:documents(), kz_data:options(), kz_json:objects()) ->
          {'ok', kz_json:objects()}.
save_docs_fold(_ConnPool, _KazooDBName, [], _Options, DocAcc) ->
    {'ok', DocAcc};
save_docs_fold(ConnPool, KazooDBName, [Doc | Docs], Options, DocAcc) ->
    PgResp = do_save_doc(ConnPool, KazooDBName, Doc, Options),
    DocResp = kz_postgresql_response:parse_response_to_bulk_response_doc(kz_doc:id(Doc), PgResp),
    save_docs_fold(ConnPool, KazooDBName, Docs, Options, [DocResp | DocAcc]).

%%------------------------------------------------------------------------------
%% @doc Delete JSON Doc from the PostgreSQL table
%% @end
%%------------------------------------------------------------------------------
-spec del_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document()) ->
          {'ok', kz_json:object()} | kz_data:data_error().
del_doc(ConnPool, KazooDBName, Doc) ->
    del_doc(ConnPool, KazooDBName, Doc, []).

-spec del_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document(), kz_data:options()) ->
          {'ok', kz_json:object()} | kz_data:data_error().
del_doc(ConnPool, KazooDBName, Doc, Options) ->
    lager:debug("deleting doc ~p (KazooDBName: ~p)", [kz_doc:id(Doc), KazooDBName]),
    {PGResp, TableName} = do_del_doc(ConnPool, KazooDBName, Doc, Options),
    kz_postgresql_response:parse_response_to_doc(TableName, PGResp).

-spec do_del_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document(), kz_data:options()) ->
          {epgsql:reply(), kz_postgresql:table_name()}.
do_del_doc(ConnPool, KazooDBName,  Doc, Options) ->
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, KazooDBName, Doc) of
        {'error', Cause} = Error ->
            lager:error("failed to find postgresql table for doc, KazooDBName: ~p, Doc: ~p, Cause: ~p", [KazooDBName, Doc, Cause]),
            {Error, 'undefined'};
        {'ok', [{TableName, _}]} ->
            {delete_doc_by_pg_table_name(ConnPool, KazooDBName, TableName, Doc, Options), TableName}
    end.

%%------------------------------------------------------------------------------
%% @doc Delete multiple JSON docs in the postgresql table
%% Expected bulk response:
%% {ok,[{[{ok,true}
%%       ,{id,abcdefgh...}
%%       ,{rev,1-12879c2017cb8f00a16a865dc6f92fe1}]}
%%     ,{[{id,xyx...}
%%       ,{error,conflict}
%%       ,{reason,Document update conflict.}]}]}
%% @end
%%------------------------------------------------------------------------------
-spec del_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:documents()) ->
          {'ok', kz_json:objects()}.
del_docs(ConnPool, KazooDBName, Docs) ->
    del_docs(ConnPool, KazooDBName, Docs, []).

-spec del_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:documents(), kz_data:options()) ->
          {'ok', kz_json:objects()}.
del_docs(ConnPool, KazooDBName, Docs, Options) ->
    lager:debug("deleting multiple docs (one by one) (KazooDBName: ~p)", [KazooDBName]),
    del_docs_fold(ConnPool, KazooDBName, Docs, Options, []).

-spec del_docs_fold(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:documents(), kz_data:options(), kz_json:objects()) ->
          {'ok', kz_json:objects()}.
del_docs_fold(_ConnPool, _KazooDBName, [], _Options, DocAcc) ->
    {'ok', DocAcc};
del_docs_fold(ConnPool, KazooDBName, [Doc | Docs], Options, DocAcc) ->
    PgResp = do_del_doc(ConnPool, KazooDBName, Doc, Options),
    DocResp = kz_postgresql_response:parse_response_to_bulk_response_doc(kz_doc:id(Doc), PgResp),
    del_docs_fold(ConnPool, KazooDBName, Docs, Options, [DocResp | DocAcc]).

%%------------------------------------------------------------------------------
%% @doc INSERT or UPDATE Query
%% For a given PG table name and doc, either INSERT or UPDATE the row in the PG DB
%% If the doc exists in the lookup table then UPDATE else INSERT
%% @end
%%------------------------------------------------------------------------------
-spec insert_or_update_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_postgresql:table_name()
                                           ,kz_data:document(), kz_data:options()) ->
          epgsql:reply().
insert_or_update_doc_by_pg_table_name(ConnPool, KazooDBName, TableName, Doc, Options) ->
    case doc_exists(ConnPool, Doc) of
        'false' ->
            lager:debug("doc ~p does not exists in pg, selecting INSERT query", [kz_doc:id(Doc)]),
            do_insert_or_update_doc_by_pg_table_name(ConnPool, <<"INSERT">>, KazooDBName, TableName, Doc, Options);
        'true' ->
            lager:debug("doc ~p exists in pg, selecting UPDATE query", [kz_doc:id(Doc)]),
            do_insert_or_update_doc_by_pg_table_name(ConnPool, <<"UPDATE">>, KazooDBName, TableName, Doc, Options);
        {'error', _} = E -> E
    end.

-spec do_insert_or_update_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:query_operator(), kz_postgresql:kazoo_db_name()
                                              ,kz_postgresql:table_name(), kz_data:document(), kz_data:options()) ->
          epgsql:reply().
do_insert_or_update_doc_by_pg_table_name(ConnPool, QueryType, KazooDBName, TableName, Doc, Options) ->
    {Query, ColumnAndTypeList} = kz_postgresql_query:generate_query(ConnPool, TableName, QueryType),
    QueryWithParametersDoc = Query#kz_postgresql_query{'parameters' = doc_to_query_parameters(KazooDBName
                                                                                             ,Doc
                                                                                             ,TableName
                                                                                             ,ColumnAndTypeList)},
    kz_postgresql_query:execute_query(ConnPool, QueryWithParametersDoc, Options).

%%------------------------------------------------------------------------------
%% @doc DELETE PG QUERY
%% DELETE a JSON doc from a postgresql table
%% Note, This assumes table layout to contain columns '_id' and 'data'
%% @end
%%------------------------------------------------------------------------------
-spec delete_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_postgresql:table_name()
                                 ,kz_data:document(), kz_data:options()) ->
          epgsql:reply().
delete_doc_by_pg_table_name(ConnPool, KazooDBName, TableName, Doc, Options) ->
    lager:debug("delete doc ~p from pg table: ~p", [kz_doc:id(Doc), TableName]),
    Query = #kz_postgresql_query{'delete_from' = <<"\"",TableName/binary,"\"">>
                                ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                                  ,<<"$1">>
                                                                  ]
                                                        }
                                                       ,{<<"=">>, [<<"\"",TableName/binary,"\".kazoo_db_name">>
                                                                  ,<<"$2">>
                                                                  ]
                                                        }]
                                           }
                                ,'returning' = [<<"*">>]
                                ,'parameters' = [kz_doc:id(Doc), KazooDBName]
                                },
    kz_postgresql_query:execute_query(ConnPool, Query, Options).

%%------------------------------------------------------------------------------
%% @doc For a given couch like db name and doc id
%% Return true if the doc exists in the lookup table, else false
%% NOTE This does not check the contents of the archived table
%% @end
%%------------------------------------------------------------------------------
-spec doc_exists(kz_postgresql:connection_pool(), kz_doc:object()) -> boolean() | epgsql:error_reply().
doc_exists(ConnPool, Doc) ->
    Query = #kz_postgresql_query{'select' = [<<"1">>]
                                ,'from' = [<<"\"lookup\"">>]
                                ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"lookup\".doc_id">>
                                                                  ,<<"$1">>
                                                                  ]}
                                                       ,{<<"=">>, [<<"\"lookup\".kazoo_db_name">>
                                                                  ,<<"$2">>
                                                                  ]}
                                                       ]
                                           }
                                ,'parameters' = [kz_doc:id(Doc), kz_doc:account_db(Doc)]
                                },
    case kz_postgresql_query:execute_query(ConnPool, Query) of
        {'ok', _, []} -> 'false';
        {'ok', _, [_]} -> 'true';
        {'error', _} = Error ->
            lager:error("postgresql error when verifing if doc (ID: ~p, KazooDBName: ~p) exists in lookup table, Error: ~p", [kz_doc:id(Doc)
                                                                                                                             ,kz_doc:account_db(Doc)
                                                                                                                             ,Error]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Generate an order defined list of parameters from a kz_data doc
%% Columns is a list of PG columns
%% Order of the parameters list returned is defined by the Columns order
%% Any doc elements not defined in the Columns list will be converted to JSON and
%% added to the list in place of the other_json column
%% @end
%%------------------------------------------------------------------------------
-spec doc_to_query_parameters(kz_postgresql:kazoo_db_name(), kz_data:document(), kz_postgresql:table_name(), list()) -> kz_term:ne_binaries().
doc_to_query_parameters(KazooDBName, Doc, TableName, Columns) ->
    lager:debug("extracting the postgresql query values from the JSON doc for table: ~p", [TableName]),
    doc_to_query_parameters(KazooDBName, Doc, TableName, Columns, []).

-spec doc_to_query_parameters(kz_postgresql:kazoo_db_name(), kz_data:document(), kz_postgresql:table_name(), list(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
doc_to_query_parameters(_KazooDBName, _Doc, _TableName, [], Acc) ->
    lists:reverse(Acc);

%% Put the remainder of the JObj in the "data" column, this must be last
doc_to_query_parameters(KazooDBName, Doc, TableName, [{<<"data">>, ColType}| []], Acc) ->
    Value = kz_postgresql_util:encode_query_value(ColType, Doc),
    doc_to_query_parameters(KazooDBName, kz_json:from_list([]), TableName, [], [Value | Acc]);

doc_to_query_parameters(KazooDBName, Doc, TableName, [{<<"kazoo_db_name">>, ColType}|Columns], Acc) ->
    Acc1 = [kz_postgresql_util:encode_query_value(ColType, KazooDBName) | Acc],
    doc_to_query_parameters(KazooDBName, Doc, TableName, Columns, Acc1);

%% Go through the db columns and find the json element for each and add it to a list
doc_to_query_parameters(KazooDBName, Doc, TableName, [{ColName, ColType}|Columns], Acc) ->
    KeyPath = kz_postgresql_schema:pg_table_and_column_to_doc_key_path(TableName, ColName),
    %% Get the value at the key path from the JSON obj
    case kz_json:get_value(KeyPath, Doc) of
        'undefined' ->
            %% Set the value to null as its not defined in the JSON
            doc_to_query_parameters(KazooDBName, Doc, TableName, Columns, [?PG_NULL | Acc]);
        Value ->
            %% Add the binary string value to the list in the reverse order and
            %% remove the item from the jobj so we know what is left at the end to add to other_json
            Acc1 = [kz_postgresql_util:encode_query_value(ColType, Value) | Acc],
            Doc1 = kz_json:delete_key(KeyPath, Doc),
            doc_to_query_parameters(KazooDBName, Doc1, TableName, Columns, Acc1)
    end.
