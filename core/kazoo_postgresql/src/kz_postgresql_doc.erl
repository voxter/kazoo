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
        ,doc_exists/2
        ]).

%%------------------------------------------------------------------------------
%% @doc Lookup couch like doc revision with couch like DbName and Doc or DocId
%% Get the highest revision number for a given doc id.
%% Note, preference should be given to calling this with a kz_doc object
%% Note, rev will be returned if the doc _deleted = true or false
%% @end
%%------------------------------------------------------------------------------
-spec lookup_doc_rev(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_term:ne_binary() | kz_data:document()) ->
                            {'ok', kz_term:ne_binary()} |
                            epgsql:error_reply().
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
%% @doc Lookup  couch like doc revision by postgresql table name and doc id
%% Postgresql query SELECT max(_rev) from doc by TableName and ID
%% Get the highest revision number for a given doc id.
%% This will return a rev number even if the doc _deleted = true
%% @end
%%------------------------------------------------------------------------------
-spec lookup_doc_rev_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:table_name(), kz_term:ne_binary()) ->
                                             {'ok', kz_term:ne_binary()} |
                                             epgsql:error_reply().
lookup_doc_rev_by_pg_table_name(ConnPool, TableName, DocId) ->
    lager:debug("looking up doc revision for DocId ~p in pg table ~p", [DocId, TableName]),
    Query = #kz_postgresql_query{'select' = [<<"\"",TableName/binary,"\"._id">>
                                            ,<<"MAX(\"",TableName/binary,"\"._rev) AS _rev">>
                                            ]
                                ,'from' = [<<"\"",TableName/binary,"\"">>]
                                ,'where' = {<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                     ,<<"$1">>
                                                     ]
                                           }
                                ,'group_by' = [<<"\"",TableName/binary,"\"._id">>]
                                ,'parameters' = [DocId]
                                },
    Response = kz_postgresql_query:execute_query(ConnPool, Query),
    case kz_postgresql_response:parse_response_to_doc(ConnPool, TableName, Response) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', JObj} -> {'ok', kz_doc:revision(JObj)};
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Open and return row as a JSON doc when privided with a couch like Db name and Doc id
%% @end
%%------------------------------------------------------------------------------
-spec open_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                      {'ok', kz_json:object()} |
                      epgsql:error_reply().
open_doc(ConnPool, DbName, DocId) ->
    open_doc(ConnPool, DbName, DocId, []).

-spec open_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
                      {'ok', kz_json:object()} |
                      epgsql:error_reply().
open_doc(ConnPool, DbName, DocId, Options) ->
    %% Get the relevant postgresql Table name
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, DbName, DocId) of
        {'ok', [{TableName, [DocId]}]} ->
            open_doc_by_pg_table_name(ConnPool, TableName, DocId, Options);
        {'error', Cause} = Error ->
            lager:error("failed to lookup the postgresql table name for DbName: ~p, DocId: ~p, Cause: ~p", [DbName, DocId, Cause]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Open postgresql row and return json doc when privided with a postgres table name and Doc id
%% @end
%%------------------------------------------------------------------------------
-spec open_doc_by_pg_table_name(kz_postgresql:connection_pool(), kz_postgresql:table_name(), kz_term:ne_binary(), kz_data:options()) ->
                                       {'ok', kz_json:object()} | epgsql:error_reply().
open_doc_by_pg_table_name(ConnPool, TableName, DocId, Options) ->
    Query = #kz_postgresql_query{'select' = [<<"DISTINCT ON (\"",TableName/binary,"\"._id) *">>]
                                ,'from' = [<<"\"",TableName/binary,"\"">>]
                                ,'where' = {<<"AND">>, [{<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                                  ,<<"$1">>
                                                                  ]}
                                                       ,{<<"=">>, [<<"\"",TableName/binary,"\"._deleted">>
                                                                  ,<<"false">>
                                                                  ]}
                                                       ,{<<"=">>, [<<"\"",TableName/binary,"\"._rev">>
                                                                  ,#kz_postgresql_query{'select' = [<<"MAX(\"",TableName/binary,"\"._rev)">>]
                                                                                       ,'from' = [<<"\"",TableName/binary,"\"">>]
                                                                                       ,'where' = {<<"=">>, [<<"\"",TableName/binary,"\"._id">>
                                                                                                            ,<<"$1">>
                                                                                                            ]}
                                                                                       }
                                                                  ]}
                                                       ]
                                           }
                                ,'order_by' = [{<<"\"",TableName/binary,"\"._id">>, <<"DESC">>}
                                              ,{<<"\"",TableName/binary,"\"._rev">>, <<"DESC">>}
                                              ]
                                ,'parameters' = [DocId]
                                },
    Response = kz_postgresql_query:execute_query(ConnPool, Query, Options),
    kz_postgresql_response:parse_response_to_doc(ConnPool, TableName, Response).

%%------------------------------------------------------------------------------
%% @doc Save a JSON doc to postgresql table
%% For doc revision reasons, we house to always INSERT to the Table and
%% increment the revision number.
%% For save we ensure the _deleted field of the JObj is set to false
%% @end
%%------------------------------------------------------------------------------
-spec save_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document()) ->
                      {'ok', kz_json:object()} | epgsql:error_reply().
save_doc(ConnPool, DbName, Doc) ->
    save_doc(ConnPool, DbName, Doc, []).

-spec save_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
                      {'ok', kz_json:object()} | epgsql:error_reply().
save_doc(ConnPool, DbName, Doc, Options) ->
    lager:debug("saving doc ~p (DbName: ~p)", [kz_doc:id(Doc), DbName]),
    do_save_doc(ConnPool, DbName, Doc, Options, <<"INSERT">>, 'true').

-spec do_save_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document(), kz_data:options(), kz_term:ne_binary(), boolean()) ->
                         {'ok', kz_json:object()} | epgsql:error_reply().
do_save_doc(ConnPool, DbName, Doc, Options, QueryType, DoWithTransactions) ->
    UpdatedDoc = kz_postgresql_util:ensure_deleted_is_set(Doc, 'false'),
    case kz_postgresql_db_table_translation:get_table_names(ConnPool, DbName, UpdatedDoc) of
        {'error', Cause} = Error ->
            lager:error("failed to find postgresql table for doc, DbName: ~p, Doc: ~p, Cause: ~p", [DbName, UpdatedDoc, Cause]),
            Error;
        {'ok', [{TableName, _}]} ->
            Response = insert_doc_to_db_by_pg_table_name(ConnPool, DbName, TableName, UpdatedDoc, Options, QueryType, DoWithTransactions),
            kz_postgresql_response:parse_response_to_doc(ConnPool, TableName, Response)
    end.

%%------------------------------------------------------------------------------
%% @doc Save multiple JSON docs to postgresql table
%% @end
%%------------------------------------------------------------------------------
-spec save_docs(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents()) ->
                       {'ok', kz_json:objects()} | epgsql:error_reply().
save_docs(ConnPool, DbName, Docs) ->
    save_docs(ConnPool, DbName, Docs, []).

%%------------------------------------------------------------------------------
%% @doc Save multiple JSON docs to postgresql table
%% @end
%%------------------------------------------------------------------------------
-spec save_docs(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) ->
                       {'ok', kz_json:objects()} | epgsql:error_reply().
save_docs(ConnPool, DbName, Docs, Options) ->
    lager:debug("saving multiple docs (DbName: ~p)", [DbName]),
    %% Do all insert querys as a pg transaction
    TransactionFun = fun() ->
                             save_docs_fold(ConnPool, DbName, Docs, Options, [])
                     end,
    case kz_postgresql_query:execute_query_with_transaction(ConnPool, TransactionFun) of
        {'error', Cause} = Error ->
            lager:error("failed to save multiple docs to postgresql db, Cause: ~p", [Cause]),
            Error;
        {'ok', DocsResp} ->
            lager:debug("saved multiple docs successfully to postgresql db"),
            {'ok', DocsResp}
    end.

%%------------------------------------------------------------------------------
%% @doc Save each doc in a list of docs, if there is a error then throw a erlang error.
%% This function is designed to be run as a postgresql transaction
%% @end
%%------------------------------------------------------------------------------
-spec save_docs_fold(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents(), kz_data:options(), kz_json:objects()) ->
                            {'ok', kz_json:objects()}.
save_docs_fold(_ConnPool, _DbName, [], _Options, DocAcc) ->
    {'ok', lists:reverse(DocAcc)};
save_docs_fold(ConnPool, DbName, [Doc | Docs], Options, DocAcc) ->
    case do_save_doc(ConnPool, DbName, Doc, Options, <<"INSERT-BULK">>, 'false') of
        {'error', Cause} ->
            erlang:error(Cause);
        {'ok', DocResp} ->
            save_docs_fold(ConnPool, DbName, Docs, Options, [DocResp | DocAcc])
    end.

%%------------------------------------------------------------------------------
%% @doc For a given doc id and / or table name, check if the doc exisis and is not set to deleted
%% @end
%%------------------------------------------------------------------------------
-spec doc_exists(kz_postgresql:connection_pool(), {kz_term:ne_binary(), kz_postgresql:table_name()} | kz_term:ne_binary()) -> boolean().
doc_exists(ConnPool, {DocId, TableName}) ->
    case open_doc_by_pg_table_name(ConnPool, TableName, DocId, []) of
        {'error', 'not_found'} -> 'false';
        {'ok', _Doc} -> 'true';
        {'error', _}=Error ->
            lager:error("failed to check if doc (~p) in PG table ~p exists Error: ~p", [DocId, TableName, Error]),
            'false'
    end;
doc_exists(ConnPool, DocId) ->
    case open_doc(ConnPool, DocId, []) of
        {'error', 'not_found'} -> 'false';
        {'ok', _Doc} -> 'true';
        {'error', _}=Error ->
            lager:error("failed to check if doc (~p) exists Error: ~p", [DocId, Error]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Delete JSON Doc from the PostgreSQL table
%% For doc revision reasons, we choose to always INSERT to the Table and
%% increment the revision number.
%% For delete we ensure the _deleted field of the JObj is set to true
%% @end
%%------------------------------------------------------------------------------
-spec del_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document()) ->
                     {'ok', kz_json:object()} | epgsql:error_reply().
del_doc(ConnPool, DbName, Doc) ->
    del_doc(ConnPool, DbName, Doc, []).

-spec del_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
                     {'ok', kz_json:object()} | epgsql:error_reply().
del_doc(ConnPool, DbName, Doc, Options) ->
    lager:debug("deleting doc ~p (DbName: ~p)", [kz_doc:id(Doc), DbName]),
    do_del_doc(ConnPool, DbName, Doc, Options, <<"INSERT">>, 'true').

-spec do_del_doc(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document(), kz_data:options(), kz_tern:ne_binay(), boolean()) ->
                        {'ok', kz_json:object()} | epgsql:error_reply().
do_del_doc(ConnPool, DbName, Doc, Options, QueryType, DoWithTransactions) ->
    %% We have the overhaed of checking if the doc exists before deleting it
    case open_doc(ConnPool, DbName, kz_doc:id(Doc), Options) of
        {'error', _}=Error ->
            lager:error("failed to delete doc, Doc not found in DB"),
            Error;
        {'ok', OpenedDoc} ->
            {'ok', [{TableName, _}]} = kz_postgresql_db_table_translation:get_table_names(ConnPool, DbName, OpenedDoc),
            UpdatedDoc = kz_postgresql_util:ensure_deleted_is_set(Doc, 'true'),
            Response = insert_doc_to_db_by_pg_table_name(ConnPool, DbName, TableName, UpdatedDoc, Options, QueryType, DoWithTransactions),
            kz_postgresql_response:parse_response_to_doc(ConnPool, TableName, Response)
    end.

%%------------------------------------------------------------------------------
%% @doc Delete multiple JSON docs in the postgresql table
%% Note, this will insert a row with _deleted = true, No row will be deleted
%% Expected response {'ok', JObjs}
%% @end
%%------------------------------------------------------------------------------
-spec del_docs(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents()) ->
                      {'ok', kz_json:objects()} |
                      epgsql:error_reply().
del_docs(ConnPool, TableName, Docs) ->
    del_doc(ConnPool, TableName, Docs, []).

-spec del_docs(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) ->
                      {'ok', kz_json:objects()} |
                      epgsql:error_reply().
del_docs(ConnPool, DbName, Docs, Options) ->
    lager:debug("deleting multiple docs (DbName: ~p)", [DbName]),
    %% Do all querys as a pg transaction
    TransactionFun = fun() ->
                             del_docs_fold(ConnPool, DbName, Docs, Options, [])
                     end,
    case kz_postgresql_query:execute_query_with_transaction(ConnPool, TransactionFun) of
        {'error', Reason} = Error ->
            lager:error("failed to delete multiple docs in the postgresql db, Reason: ~p", [Reason]),
            Error;
        {'ok', _DeletedDocs} = OkResp -> OkResp
    end.

%%------------------------------------------------------------------------------
%% @doc Delete each doc in a list of docs
%% This function is designed to be run as a postgresql transaction
%% @end
%%------------------------------------------------------------------------------
-spec del_docs_fold(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:documents(), kz_data:options(), kz_json:objects())->
                           {'ok', kz_json:objects()}.
del_docs_fold(_ConnPool, _DbName, [], _Options, DocAcc) ->
    {'ok', lists:reverse(DocAcc)};
del_docs_fold(ConnPool, DbName, [Doc | Docs], Options, DocAcc) ->
    case do_del_doc(ConnPool, DbName, Doc, Options, <<"INSERT-BULK">>, 'false') of
        {'error', _Cause} = Error ->
            erlang:error(Error);
        {'ok', DocResp} ->
            del_docs_fold(ConnPool, DbName, Docs, Options, [DocResp | DocAcc])
    end.

%%------------------------------------------------------------------------------
%% @doc Insert a JSON doc to postgresql table with a postgresql TableName and Doc
%% For doc revision reasons, we chouse to always INSERT to the Table and
%% increment the revision number.
%% For save we ensure the _deleted field of the JObj is set to false
%% For delete we ensure the _deleted field of the JObj is set to true
%% DoWithTransactions should be set to true unless you are wrapping this function in a transaction
%% @end
%%------------------------------------------------------------------------------
-spec insert_doc_to_db_by_pg_table_name(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_postgresql:table_name()
                                       ,kz_data:document(), kz_data:options(), kz_term:ne_binary(), boolean()) ->
                                               epgsql:reply().
insert_doc_to_db_by_pg_table_name(ConnPool, DbName, TableName, Doc, Options, QueryType, DoWithTransactions) ->
    lager:debug("inserting doc ~p into pg table: ~p", [kz_doc:id(Doc), TableName]),
    %% Set / Update the doc revision
    case get_next_postgresql_doc_revision(ConnPool, DbName, Doc) of
        {'error', Cause} = Error ->
            lager:error("failed to calcualte the doc revision, Cause: ~p", [Cause]),
            Error;
        {'ok', Rev} ->
            insert_doc_to_db_by_pg_table_name(ConnPool, DbName, TableName, Doc, Options, QueryType, DoWithTransactions, Rev)
    end.

-spec insert_doc_to_db_by_pg_table_name(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_postgresql:table_name()
                                       ,kz_data:document(), kz_data:options(), kz_term:ne_binary(), boolean(), integer()) ->
                                               epgsql:reply().
insert_doc_to_db_by_pg_table_name(ConnPool, DbName, TableName, Doc, Options, QueryType, DoWithTransactions, 1) ->
    {Query, ColumnAndTypeList} = kz_postgresql_query:generate_query(ConnPool, TableName, QueryType),
    DocWithRev = kz_doc:set_revision(Doc, 1),
    QueryWithParametersDoc = Query#kz_postgresql_query{'parameters' = doc_to_query_parameters(DocWithRev
                                                                                             ,TableName
                                                                                             ,ColumnAndTypeList)},
    case DoWithTransactions of
        'false' ->
            %% Run the 2 queries separately
            insert_entry_into_lookup_table(ConnPool, DbName, TableName, kz_doc:id(Doc)),
            kz_postgresql_query:execute_query(ConnPool, QueryWithParametersDoc, Options);
        'true' ->
            %% Exec both querys in a transaction
            TransactionFun = fun() ->
                                     insert_entry_into_lookup_table(ConnPool, DbName, TableName, kz_doc:id(Doc)),
                                     kz_postgresql_query:execute_query(ConnPool, QueryWithParametersDoc, Options)
                             end,
            kz_postgresql_query:execute_query_with_transaction(ConnPool, TransactionFun)
    end;
insert_doc_to_db_by_pg_table_name(ConnPool, _DbName, TableName, Doc, Options, QueryType, _DoWithTransactions, Rev) ->
    {Query, ColumnAndTypeList} = kz_postgresql_query:generate_query(ConnPool, TableName, QueryType),
    DocWithRev = kz_doc:set_revision(Doc, Rev),
    QueryWithParametersDoc = Query#kz_postgresql_query{'parameters' = doc_to_query_parameters(DocWithRev
                                                                                             ,TableName
                                                                                             ,ColumnAndTypeList)},
    kz_postgresql_query:execute_query(ConnPool, QueryWithParametersDoc, Options).

%%------------------------------------------------------------------------------
%% @doc Generate an order defined list of parameters from a kz_data doc
%% Columns is a list of PG columns
%% Order of the parameters list returned is defined by the Columns order
%% Any doc elements not defined in the Columns list will be converted to JSON and
%% added to the list in place of the other_json column
%% @end
%%------------------------------------------------------------------------------
-spec doc_to_query_parameters(kz_data:document(), kz_postgresql:table_name(), list()) -> kz_term:ne_binaries().
doc_to_query_parameters(Doc, TableName, Columns) ->
    lager:debug("extracting the postgresql query values from the JSON doc for table: ~p", [TableName]),
    doc_to_query_parameters(Doc, TableName, Columns, []).

-spec doc_to_query_parameters(kz_data:document(), kz_postgresql:table_name(), list(), kz_term:ne_binaries()) ->
                                     kz_term:ne_binaries().
doc_to_query_parameters(_Doc, _TableName, [], Acc) ->
    lists:reverse(Acc);

%% Put the remainder of the JObj in the other_json column, this must be last
doc_to_query_parameters(Doc, TableName, [{<<"other_json">>, ColType}| []], Acc) ->
    Value = kz_postgresql_util:encode_query_value(ColType, Doc),
    doc_to_query_parameters(kz_json:from_list([]), TableName, [], [Value | Acc]);

%% Go through the db columns and find the json element for each and add it to a list
doc_to_query_parameters(Doc, TableName, [{ColName, ColType}|Columns], Acc) ->
    KeyPath = kz_postgresql_schema:pg_table_and_column_to_doc_key_path(TableName, ColName),
    %% Get the value at the key path from the JSON obj
    case kz_json:get_value(KeyPath, Doc) of
        'undefined' ->
            %% Set the value to null as its not defined in the JSON
            doc_to_query_parameters(Doc, TableName, Columns, [?PG_NULL | Acc]);
        Value ->
            %% Add the binary string value to the list in the reverse order and
            %% remove the item from the jobj so we know what is left at the end to add to other_json
            Acc1 = [kz_postgresql_util:encode_query_value(ColType, Value) | Acc],
            Doc1 = kz_json:delete_key(KeyPath, Doc),
            doc_to_query_parameters(Doc1, TableName, Columns, Acc1)
    end.

%%------------------------------------------------------------------------------
%% @doc For every doc there must be a reference to what table it can be found in.
%% This record is kept in the table PG_LOOKUP_TABLE_NAME
%% Kazoo referenced docs by DbName (couch like) and doc id, so the lookup table cantains the
%% couch like DbName and Doc id and the coresponding postgresql table to find the doc by id.
%% @end
%%------------------------------------------------------------------------------
-spec insert_entry_into_lookup_table(kz_postgresql:connection_pool(), kz_term:ne_binary()
                                    ,kz_postgresql:table_name(), kz_term:ne_binary()) ->
                                            epgsql:reply().
insert_entry_into_lookup_table(ConnPool, CouchDbName, PgTableName, DocId) ->
    lager:debug("inserting entery into pg lookup table, DbName: ~p, PgTableName: ~p, DocId: ~p", [CouchDbName
                                                                                                 ,PgTableName
                                                                                                 ,DocId]),
    Query = #kz_postgresql_query{'insert_into' = {<<"\"",?PG_LOOKUP_TABLE_NAME/binary,"\"">>, [<<"doc_id">>
                                                                                              ,<<"db_name">>
                                                                                              ,<<"table_name">>]}
                                ,'values' = [[<<"$1">>, <<"$2">>, <<"$3">>]]
                                ,'parameters' = [DocId, CouchDbName, PgTableName]
                                },
    kz_postgresql_query:execute_query(ConnPool, Query).

%%------------------------------------------------------------------------------
%% @doc For a Doc, first check if the current rev is set on itself, If not then
%% look up the doc in PG DB by ID. Then increment the rev by 1
%% Note, this returns the revision as an int for postgresql and not the full couch like revision
%% @end
%%------------------------------------------------------------------------------
-spec get_next_postgresql_doc_revision(kz_postgresql:connection_pool(), kz_term:ne_binary(), kz_data:document()) ->
                                              {'ok', integer()} | epgsql:error_reply().
get_next_postgresql_doc_revision(ConnPool, DbName, Doc) ->
    lager:debug("calculating doc revision for doc id: ~p, DbName: ~p", [kz_doc:id(Doc), DbName]),
    CurrentRev = case kz_doc:revision(Doc) of
                     'undefined' -> lookup_doc_rev(ConnPool, DbName, Doc);
                     Rev -> {'ok', Rev}
                 end,
    increment_rev(CurrentRev).

%%------------------------------------------------------------------------------
%% @doc Increment a Couch like revision or PG revision by 1
%% Return PG like integer revision
%% @end
%%------------------------------------------------------------------------------
-spec increment_rev({'ok', kz_term:ne_binary()} | {'ok', integer()} | epgsql:error_reply()) ->
                           {'ok', integer()} | epgsql:error_reply().
increment_rev({'error', 'not_found'}) -> {'ok', 1};
increment_rev({'error', _}=Error) -> Error;
increment_rev({'ok', Rev}) when is_integer(Rev) -> {'ok', Rev+1};
increment_rev({'ok', Rev}) ->
    RevInt = kz_postgresql_util:couch_rev_to_postgresql_rev(Rev),
    {'ok', binary_to_integer(RevInt)+1}.
