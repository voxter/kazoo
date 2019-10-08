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
%% @doc Lookup couch like doc revision with couch like DbName and Doc or DocId
%% Get the highest revision number for a given doc id.
%% Note, preference should be given to calling this with a kz_doc object
%% Note, rev will be returned if the doc _deleted = true or false
%% @end
%%------------------------------------------------------------------------------
-spec lookup_doc_rev(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_term:ne_binary() | kz_data:document()) ->
                            {'ok', kz_term:ne_binary()} | kz_data:data_error().
lookup_doc_rev(ConnPool, DbName, DocOrId) ->
    %% Get the relevant postgresql Table name
    lager:debug("looking up doc rev for doc ~p", [DocOrId]),
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, DbName, DocOrId) of
        {'ok', [{TableName, [DocId]}]} ->
            lookup_doc_rev_by_pg_table_name(ConnPool, TableName, DocId);
        {'error', Cause} = Error ->
            lager:debug("failed to lookup the postgresql table name for DbName: ~p, DocOrId: ~p, Cause: ~p", [DbName, DocOrId, Cause]),
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
    case kz_postgresql_response:parse_response_to_doc(PGResp) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', JObj} -> {'ok', kz_doc:revision(JObj)};
        {'error', _} = Error -> {'error', kz_postgresql_response:format_error(Error)}
    end.

%%------------------------------------------------------------------------------
%% @doc Open and return row as a JSON doc when privided with a couch like Db name and Doc id
%% @end
%%------------------------------------------------------------------------------
-spec open_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                      {'ok', kz_json:object()} | kz_data:data_error().
open_doc(ConnPool, DbName, DocId) ->
    open_doc(ConnPool, DbName, DocId, []).

-spec open_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
                      {'ok', kz_json:object()} | kz_data:data_error().
open_doc(ConnPool, DbName, DocId, Options) ->
    %% Get the relevant postgresql Table name
    %% TODO Maybe use Options (expected_doc_type) to calculate table name?
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, DbName, DocId) of
        {'ok', [{TableName, [DocId]}]} ->
            open_doc_by_pg_table_name(ConnPool, TableName, DbName, DocId, Options);
        {'error', Cause} = Error ->
            lager:error("failed to lookup the postgresql table name for DbName: ~p, DocId: ~p, Cause: ~p", [DbName, DocId, Cause]),
            {'error', kz_postgresql_response:format_error(Error)}
    end.

%%------------------------------------------------------------------------------
%% @doc Open postgresql row and return json doc when privided with a postgres table name and Doc id
%% @end
%%------------------------------------------------------------------------------
-spec open_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:table_name(), kz_term:ne_binary()
                               ,kz_term:ne_binary(), kz_data:options()) ->
                                       {'ok', kz_json:object()} | kz_data:data_error().
open_doc_by_pg_table_name(ConnPool, TableName, DbName, DocId, Options) ->
    Query = #kz_postgresql_query{'select' = [<<"*">>]
                                ,'from' = [<<"\"",TableName/binary,"\"">>]
                                ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                                  ,<<"$1">>
                                                                  ]}
                                                       ,{<<"=">>, [<<"\"",TableName/binary,"\".data->>'pvt_account_db'">>
                                                                  ,<<"$2">>
                                                                  ]}
                                                       ]}
                                ,'parameters' = [DocId, DbName]
                                },
    PGResp = kz_postgresql_query:execute_query(ConnPool, Query, Options),
    kz_postgresql_response:parse_response_to_doc(PGResp).

%%------------------------------------------------------------------------------
%% @doc Save a JSON doc to postgresql table
%% @end
%%------------------------------------------------------------------------------
-spec save_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document()) ->
                      {'ok', kz_json:object()} | kz_data:data_error().
save_doc(ConnPool, DbName, Doc) ->
    save_doc(ConnPool, DbName, Doc, []).

-spec save_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
                      {'ok', kz_json:object()} | kz_data:data_error().
save_doc(ConnPool, DbName, Doc, Options) ->
    lager:debug("saving doc ~p (DbName: ~p)", [kz_doc:id(Doc), DbName]),
    PGResp = do_save_doc(ConnPool, DbName, Doc, Options),
    kz_postgresql_response:parse_response_to_doc(PGResp).

-spec do_save_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> epgsql:reply().
do_save_doc(ConnPool, DbName, Doc, Options) ->
    DocWithDbName = kz_doc:set_account_db(Doc, DbName),
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, DbName, DocWithDbName) of
        {'error', Cause} = Error ->
            lager:error("failed to find postgresql table for doc, Doc: ~p, Cause: ~p", [DocWithDbName, Cause]),
            {Error, 'undefined'};
        {'ok', [{TableName, _}]} ->
            insert_or_update_doc_by_pg_table_name(ConnPool, TableName, DocWithDbName, Options)
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
-spec save_docs(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents()) ->
                       {'ok', kz_json:objects()} | kz_data:data_error().
save_docs(ConnPool, DbName, Docs) ->
    save_docs(ConnPool, DbName, Docs, []).

-spec save_docs(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) ->
                       {'ok', kz_json:objects()} | kz_data:data_error().
save_docs(ConnPool, DbName, Docs, Options) ->
    lager:debug("saving multiple docs (one by one) (DbName: ~p)", [DbName]),
    save_docs_fold(ConnPool, DbName, Docs, Options, []).

-spec save_docs_fold(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents(), kz_data:options(), kz_json:objects()) ->
                            {'ok', kz_json:objects()}.
save_docs_fold(_ConnPool, _DbName, [], _Options, DocAcc) ->
    {'ok', DocAcc};
save_docs_fold(ConnPool, DbName, [Doc | Docs], Options, DocAcc) ->
    PgResp = do_save_doc(ConnPool, DbName, Doc, Options),
    DocResp = kz_postgresql_response:parse_response_to_bulk_response_doc(kz_doc:id(Doc), PgResp),
    save_docs_fold(ConnPool, DbName, Docs, Options, [DocResp | DocAcc]).

%%------------------------------------------------------------------------------
%% @doc Delete JSON Doc from the PostgreSQL table
%% @end
%%------------------------------------------------------------------------------
-spec del_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document()) ->
                     {'ok', kz_json:object()} | kz_data:data_error().
del_doc(ConnPool, DbName, Doc) ->
    del_doc(ConnPool, DbName, Doc, []).

-spec del_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
                     {'ok', kz_json:object()} | kz_data:data_error().
del_doc(ConnPool, DbName, Doc, Options) ->
    lager:debug("deleting doc ~p (DbName: ~p)", [kz_doc:id(Doc), DbName]),
    PGResp = do_del_doc(ConnPool, DbName, Doc, Options),
    kz_postgresql_response:parse_response_to_doc(PGResp).

-spec do_del_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> epgsql:reply().
do_del_doc(ConnPool, DbName,  Doc, Options) ->
    DocWithDbName = kz_doc:set_account_db(Doc, DbName),
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, DbName, DocWithDbName) of
        {'error', Cause} = Error ->
            lager:error("failed to find postgresql table for doc, DbName: ~p, Doc: ~p, Cause: ~p", [DbName, DocWithDbName, Cause]),
            {Error, 'undefined'};
        {'ok', [{TableName, _}]} ->
            delete_doc_by_pg_table_name(ConnPool, TableName, DocWithDbName, Options)
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
-spec del_docs(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents()) ->
                      {'ok', kz_json:objects()}.
del_docs(ConnPool, DbName, Docs) ->
    del_docs(ConnPool, DbName, Docs, []).

-spec del_docs(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) ->
                      {'ok', kz_json:objects()}.
del_docs(ConnPool, DbName, Docs, Options) ->
    lager:debug("deleting multiple docs (one by one) (DbName: ~p)", [DbName]),
    del_docs_fold(ConnPool, DbName, Docs, Options, []).

-spec del_docs_fold(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents(), kz_data:options(), kz_json:objects()) ->
                           {'ok', kz_json:objects()}.
del_docs_fold(_ConnPool, _DbName, [], _Options, DocAcc) ->
    {'ok', DocAcc};
del_docs_fold(ConnPool, DbName, [Doc | Docs], Options, DocAcc) ->
    PgResp = do_del_doc(ConnPool, DbName, Doc, Options),
    DocResp = kz_postgresql_response:parse_response_to_bulk_response_doc(kz_doc:id(Doc), PgResp),
    del_docs_fold(ConnPool, DbName, Docs, Options, [DocResp | DocAcc]).

%%------------------------------------------------------------------------------
%% @doc INSERT or UPDATE Query
%% For a given PG table name and doc, either INSERT or UPDATE the row in the PG DB
%% If the doc exists in the lookup table then UPDATE else INSERT
%% @end
%%------------------------------------------------------------------------------
-spec insert_or_update_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:table_name(), kz_data:document(), kz_data:options()) ->
                                                   epgsql:reply().
insert_or_update_doc_by_pg_table_name(ConnPool, TableName, Doc, Options) ->
    case doc_exists(ConnPool, Doc) of
        'false' ->
            lager:debug("doc ~p does not exists in pg, selecting INSERT query", [kz_doc:id(Doc)]),
            do_insert_doc_by_pg_table_name(ConnPool, TableName, Doc, Options);
        'true' ->
            lager:debug("doc ~p exists in pg, selecting UPDATE query", [kz_doc:id(Doc)]),
            do_update_doc_by_pg_table_name(ConnPool, TableName, Doc, Options);
        {'error', _} = E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc INSERT PG QUERY
%% INSERT a JSON doc into a postgresql table
%% Note, This assumes table layout to contain columns '_id' and 'data'
%% @end
%%------------------------------------------------------------------------------
-spec do_insert_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:table_name(), kz_data:document(), kz_data:options()) ->
                                            epgsql:reply().
do_insert_doc_by_pg_table_name(ConnPool, TableName, Doc, Options) ->
    lager:debug("inserting doc ~p into pg table: ~p", [kz_doc:id(Doc), TableName]),
    %% Strip out the id and rev from the doc as they are seperate pg columns
    %% Remove the rev if its set as this will be calculated by PG trigger
    ReducedDoc = kz_doc:delete_revision(kz_doc:delete_id(Doc)),
    Query = #kz_postgresql_query{'insert_into' = {TableName, [<<"_id">>, <<"data">>]}
                                ,'values' = [[<<"$1">>, <<"$2">>]]
                                ,'returning' = [<<"*">>]
                                ,'parameters' = [kz_postgresql_util:encode_query_value(<<"character varying">>, kz_doc:id(Doc))
                                                ,kz_postgresql_util:encode_query_value(<<"jsonb">>, ReducedDoc)
                                                ]
                                },
    kz_postgresql_query:execute_query(ConnPool, Query, Options).


%%------------------------------------------------------------------------------
%% @doc UPDATE PG QUERY
%% UPDATE a JSON doc in a postgresql table
%% Note, This assumes table layout to contain columns '_id' and 'data'
%% @end
%%------------------------------------------------------------------------------
-spec do_update_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:table_name(), kz_data:document(), kz_data:options()) ->
                                            epgsql:reply().
do_update_doc_by_pg_table_name(ConnPool, TableName, Doc, Options) ->
    lager:debug("updating doc ~p in pg table: ~p", [kz_doc:id(Doc), TableName]),
    %% Strip out the id and rev from the doc as they are seperate pg columns
    %% Remove the rev if its set as this will be calculated by PG trigger
    ReducedDoc = kz_doc:delete_revision(kz_doc:delete_id(Doc)),
    Query = #kz_postgresql_query{'update' = <<"\"",TableName/binary,"\"">>
                                ,'set' = [{<<"_id">>, <<"$1">>}
                                         ,{<<"data">>, <<"$2">>}
                                         ]
                                ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                                  ,<<"$1">>
                                                                  ]
                                                        }
                                                       ,{<<"=">>, [<<"\"",TableName/binary,"\".data->>'pvt_account_db'">>
                                                                  ,<<"$3">>
                                                                  ]
                                                        }
                                                       ]
                                           }
                                ,'returning' = [<<"*">>]
                                ,'parameters' = [kz_postgresql_util:encode_query_value(<<"character varying">>, kz_doc:id(Doc))
                                                ,kz_postgresql_util:encode_query_value(<<"jsonb">>, ReducedDoc)
                                                ,kz_postgresql_util:encode_query_value(<<"character varying">>, kz_doc:account_db(Doc))
                                                ]
                                },
    kz_postgresql_query:execute_query(ConnPool, Query, Options).

%%------------------------------------------------------------------------------
%% @doc DELETE PG QUERY
%% DELETE a JSON doc from a postgresql table
%% Note, This assumes table layout to contain columns '_id' and 'data'
%% @end
%%------------------------------------------------------------------------------
-spec delete_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:table_name() ,kz_data:document(), kz_data:options()) ->
                                         epgsql:reply().
delete_doc_by_pg_table_name(ConnPool, TableName, Doc, Options) ->
    lager:debug("delete doc ~p from pg table: ~p", [kz_doc:id(Doc), TableName]),
    Query = #kz_postgresql_query{'delete_from' = <<"\"",TableName/binary,"\"">>
                                ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                                  ,<<"$1">>
                                                                  ]
                                                        }
                                                       ,{<<"=">>, [<<"\"",TableName/binary,"\".data->>'pvt_account_db'">>
                                                                  ,<<"$2">>
                                                                  ]
                                                        }]
                                           }
                                ,'returning' = [<<"*">>]
                                ,'parameters' = [kz_doc:id(Doc), kz_doc:account_db(Doc)]
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
                                                       ,{<<"=">>, [<<"\"lookup\".db_name">>
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
            lager:error("postgresql error when verifing if doc (ID: ~p, DbName: ~p) exists in lookup table, Error: ~p", [kz_doc:id(Doc)
                                                                                                                        ,kz_doc:account_db(Doc)
                                                                                                                        ,Error]),
            Error
    end.
