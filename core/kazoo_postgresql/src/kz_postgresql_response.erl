%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc Postgresql query response utilities
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_response).
-export([parse_response_to_doc/3
        ,parse_response_to_bulk_response_doc/2
        ,parse_response_to_view_doc/3
        ,format_error/1
        ]).
-include("kz_postgresql.hrl").


%%------------------------------------------------------------------------------
%% @doc Convert epgsql error messages to kz_data formatted errors
%% @end
%%------------------------------------------------------------------------------
-spec format_error(epgsql:error_reply()) -> kz_data:data_errors().
format_error({'error', {'error', 'error', _, 'unique_violation', _, _}}) -> 'conflict';
format_error({'error', Error}) -> Error.

%%------------------------------------------------------------------------------
%% @doc Parse the epgsql postgresql response to the expected couch like JSON doc response
%% @end
%%------------------------------------------------------------------------------
-spec parse_response_to_doc(kz_postgresql:connection_pool(), kz_postgresql:table_name(), epgsql:reply()) ->
                                   {'ok', kz_doc:object() | kz_doc:objects()} | kz_data:data_error().
%% Error response
parse_response_to_doc(_ConnPool, _TableName, {'error', Cause}=Error) ->
    lager:debug("postgresql query returned error, ~p", [Cause]),
    {'error', format_error(Error)};

%% SELECT with an empty response
parse_response_to_doc(_ConnPool, _TableName, {'ok', _Columns, []}) ->
    lager:debug("postgresql query to table '~p' returned an ok response with 0 rows", [_TableName]),
    {'error', 'not_found'};

%% SELECT response with 1 or more rows
parse_response_to_doc(_ConnPool, TableName, {'ok', Columns, Rows}) ->
    lager:debug("postgresql query to table '~p' returned an ok result with ~p rows", [TableName, length(Rows)]),
    Doc = col_and_rows_to_jobj(TableName, Columns, Rows),
    DocsWithRev = kz_postgresql_util:simulate_couch_doc_revision(Doc),
    {'ok', DocsWithRev};

%% INSERT, UPDATE or DELETE response with 0 rows
parse_response_to_doc(_ConnPool, _TableName, {'ok', 0}) ->
    lager:debug("postgresql query to table '~p' returned an ok result with 0 rows", [_TableName]),
    {'error', 'not_found'};

%% INSERT, UPDATE or DELETE response affecting 1 or more rows
parse_response_to_doc(_ConnPool, _TableName, {'ok', RowCount}) ->
    lager:debug("postgresql query to table '~p' returned an ok result affecting ~p rows", [RowCount]),
    {'ok', kz_json:new()};

%% INSERT, UPDATE or DELETE with RETURNING response with 0 rows
parse_response_to_doc(_ConnPool, _TableName, {'ok', 0, _, _}) ->
    lager:debug("postgresql query to table '~p' returned an ok result with 0 rows", [_TableName]),
    {'error', 'not_found'};

%% INSERT, UPDATE or DELETE with RETURNING response with 1 or more rows
parse_response_to_doc(_ConnPool, TableName, {'ok', _, Columns, Rows}) ->
    lager:debug("postgresql query to table '~p' returned an ok result with ~p rows", [TableName, length(Rows)]),
    Docs = col_and_rows_to_jobj(TableName, Columns, Rows),
    DocsWithRev = kz_postgresql_util:simulate_couch_doc_revision(Docs),
    {'ok', DocsWithRev}.

%%------------------------------------------------------------------------------
%% @doc Parse the epgsql postgresql response to the expected couch like JSON BULK doc response
%% Expected bulk response:
%% {[{ok, true}
%%  ,{id, abcdefgh...}
%%  ,{rev, 1-12879c2017cb8f00a16a865dc6f92fe1}]}
%% OR
%% {[{id, xyx...}
%%  ,{error, conflict}
%%  ,{reason, Document update conflict.}]}
%% @end
%%------------------------------------------------------------------------------
-spec parse_response_to_bulk_response_doc(kz_term:ne_binary(), epgsql:reply()) -> kz_doc:object() | kz_doc:objects().
%% ERROR response
parse_response_to_bulk_response_doc(DocId, {'error', Cause}=Error) ->
    lager:debug("postgresql returned error, ~p", [Cause]),
    kz_json:from_list([{<<"id">>, DocId}
                      ,{<<"error">>, format_error(Error)}
                      ,{<<"reason">>, <<"">>}
                      ]);

%% INSERT, UPDATE or DELETE response with 0 rows
parse_response_to_bulk_response_doc(DocId, {'ok', 0}) ->
    lager:debug("postgresql returned empty response"),
    kz_json:from_list([{<<"id">>, DocId}
                      ,{<<"error">>, <<"not_found">>}
                      ,{<<"reason">>, <<"Document not found">>}
                      ]);

%% INSERT, UPDATE or DELETE response affecting 1 or more rows
parse_response_to_bulk_response_doc(DocId, {'ok', RowCount}) ->
    lager:debug("postgresql returned ok result affecting ~p rows", [RowCount]),
    kz_json:from_list([{<<"ok">>, 'true'}
                      ,{<<"id">>, DocId}
                      ]);

%% INSERT, UPDATE or DELETE with RETURNING response with 0 rows
parse_response_to_bulk_response_doc(DocId, {'ok', 0, _, _}) ->
    lager:debug("postgresql returned empty response"),
    kz_json:from_list([{<<"id">>, DocId}
                      ,{<<"error">>, <<"not_found">>}
                      ,{<<"reason">>, <<"Document not found">>}
                      ]);

%% INSERT, UPDATE or DELETE with RETURNING response with 1 or more rows
parse_response_to_bulk_response_doc(DocId, {'ok', _, Columns, Rows}) ->
    lager:debug("postgresql query returned an ok result with ~p rows", [length(Rows)]),
    col_and_rows_to_bulk_jobj(DocId, Columns, Rows).

%%------------------------------------------------------------------------------
%% @doc Parse the epgsql postgresql response to the expected couch like doc view response
%% @end
%%------------------------------------------------------------------------------
-spec parse_response_to_view_doc(kz_postgresql:connection_pool(), kz_postgresql:view_name(), epgsql:reply()) ->
                                        {'ok', kz_doc:objects()} | kz_data:data_error().
parse_response_to_view_doc(_ConnPool, _TableName, {'error', _Cause}=Error) ->
    lager:error("postgresql view query returned an error: ~p", [Error]),
    {'error', format_error(Error)};
parse_response_to_view_doc(_ConnPool, _TableName, {'ok', _Columns, []}) ->
    lager:debug("postgresql view query returned an ok result with 0 rows"),
    {'ok', []};
parse_response_to_view_doc(_ConnPool, TableName, {'ok', Columns, Rows}) ->
    lager:debug("postgresql query returned an ok result with ~p rows", [length(Rows)]),
    Docs = col_and_rows_to_view_jobj(TableName, Columns, Rows),
    DocsUpdated = kz_postgresql_util:simulate_couch_doc_revision(Docs, [<<"value">>, <<"rev">>]),
    DocsUpdated1 = kz_postgresql_util:simulate_couch_doc_revision(DocsUpdated, [<<"doc">>, <<"_rev">>]),
    {'ok', DocsUpdated1}.

%%------------------------------------------------------------------------------
%% @doc Convert the epgsql postgresql column and row(s) to json doc
%% This will take in to account any custom column to doc key mappings defined in
%% kz_postgresql_schema:get_key_path/2
%% @end
%%------------------------------------------------------------------------------
-spec col_and_rows_to_jobj(kz_postgresql:table_name(), list(), list()) -> kz_doc:object() | kz_doc:objects().
col_and_rows_to_jobj(TableName, Columns, [Row]) ->
    lager:debug("converting postgresql response row to doc"),
    col_and_row_to_jobj(TableName, Columns, tuple_to_list(Row), kz_json:new());
col_and_rows_to_jobj(TableName, Columns, Rows) ->
    lager:debug("converting postgresql response rows to docs"),
    [col_and_row_to_jobj(TableName, Columns, tuple_to_list(Row), kz_json:new()) || Row <- Rows].

-spec col_and_row_to_jobj(kz_postgresql:table_name(), list(), kz_term:ne_binaries(), kz_json:object()) -> kz_doc:object().
col_and_row_to_jobj(_, [], [], JObj) ->
    JObj;
col_and_row_to_jobj(TableName, [Column | OtherColumns], [Value | OtherValues], JObj) ->
    {'column', ColName, Type, _, _, _, _} = Column,
    UpdatedJObj = add_postgresql_value_to_jobj(TableName, ColName, Value, Type, JObj),
    col_and_row_to_jobj(TableName, OtherColumns, OtherValues, UpdatedJObj).

%%------------------------------------------------------------------------------
%% @doc Convert the epgsql postgresql column and row(s) to json bulk opertion response doc
%% The BULK response returns a limited doc
%% @end
%%------------------------------------------------------------------------------
-spec col_and_rows_to_bulk_jobj(kz_term:ne_binary(), list(), list()) -> kz_doc:object() | kz_doc:objects().
col_and_rows_to_bulk_jobj(DocId, Columns, [Row]) ->
    lager:debug("converting postgresql response row to bulk response doc"),
    BaseJObj = kz_json:from_list([{<<"ok">>, 'true'}
                                 ,{<<"id">>, DocId}
                                 ]),
    col_and_row_to_bulk_jobj(Columns, tuple_to_list(Row), BaseJObj);
col_and_rows_to_bulk_jobj(DocId, Columns, Rows) ->
    lager:debug("converting postgresql response rows to bulk response docs"),
    BaseJObj = kz_json:from_list([{<<"ok">>, 'true'}
                                 ,{<<"id">>, DocId}
                                 ]),
    [col_and_row_to_bulk_jobj(Columns, tuple_to_list(Row), BaseJObj) || Row <- Rows].

-spec col_and_row_to_bulk_jobj(list(), kz_term:ne_binaries(), kz_json:object()) -> kz_doc:object().
col_and_row_to_bulk_jobj([], [], JObj) -> JObj;
col_and_row_to_bulk_jobj([{'column', <<"_rev">>, _Type, _, _, _, _} | _OtherColumns], [RevValue | _OtherValues], JObj) ->
    kz_json:set_value(<<"rev">>, RevValue, JObj);
col_and_row_to_bulk_jobj([_Column | OtherColumns], [_Value | OtherValues], JObj) ->
    col_and_row_to_bulk_jobj(OtherColumns, OtherValues, JObj).

%%------------------------------------------------------------------------------
%% @doc Convert the epgsql postgresql column and row(s) to the json doc view
%% with "id", "key", "value" keys
%% This will take in to account any custom column to doc key mappings defined in
%% kz_postgresql_schema:get_key_path/2
%% @end
%%------------------------------------------------------------------------------
-spec col_and_rows_to_view_jobj(kz_postgresql:table_name() | kz_postgresql:view_name(), list(), list()) -> kz_json:objects().
col_and_rows_to_view_jobj(TableOrViewName, Columns, Rows) ->
    lager:debug("converting postgresql view response row(s) to view doc(s)"),
    [col_and_rows_to_view_jobj(TableOrViewName, Columns, tuple_to_list(Row), kz_json:new()) || Row <- Rows].

-spec col_and_rows_to_view_jobj(kz_postgresql:table_name() | kz_postgresql:view_name()
                               ,list(), kz_term:ne_binaries(), kz_json:object()) ->
                                       kz_json:object().
col_and_rows_to_view_jobj(_TableOrViewName, [], [], JObj) ->
    JObj;
col_and_rows_to_view_jobj(TableOrViewName, [Column | OtherColumns], [Value | OtherValues], JObj) ->
    {'column', ColName, Type, _, _, _, _} = Column,
    UpdatedJObj = add_postgresql_value_to_jobj(TableOrViewName, ColName, Value, Type, JObj, [<<"doc">>]),
    col_and_rows_to_view_jobj(TableOrViewName, OtherColumns, OtherValues, UpdatedJObj).

%%------------------------------------------------------------------------------
%% @doc Add a PG value to a doc like map structure.
%% The map value key will be calculated from the PG column name
%% This will take in to account any custom column to doc key mappings defined in
%% kz_postgresql_schema:get_key_path/2
%% @end
%%------------------------------------------------------------------------------
-spec add_postgresql_value_to_jobj(kz_postgresql:table_name() | kz_postgresql:view_name()
                                  ,kz_term:ne_binary(), any(), atom(), kz_json:object()) ->
                                          map().
add_postgresql_value_to_jobj(TableOrViewName, ColumnName, Value, Type, Map) ->
    add_postgresql_value_to_jobj(TableOrViewName, ColumnName, Value, Type, Map, []).

-spec add_postgresql_value_to_jobj(kz_postgresql:table_name() | kz_postgresql:view_name()
                                  ,kz_term:ne_binary(), any(), atom(), kz_json:object(), list()) ->
                                          map().
%% Handle other_json
add_postgresql_value_to_jobj(_TableOrViewName, <<"other_json">>, Value, 'jsonb'=Type, JObj, PrePath) ->
    DecodedValue = kz_postgresql_util:decode_query_value(Type, Value),
    add_value_to_jobj(PrePath, DecodedValue,JObj);

%% Ignore _deleted
add_postgresql_value_to_jobj(_TableOrViewName, <<"_deleted">>, _Value, _Type, JObj, _PrePath) ->
    JObj;

%% Ignore any column with the value set to null
add_postgresql_value_to_jobj(_TableOrViewName, _ColumnName, ?PG_NULL, _Type, JObj, _PrePath) ->
    JObj;

%% Handle view_id
%% View Related cases should ignore PrePath (_view_****)
add_postgresql_value_to_jobj(_TableOrViewName, <<"_view_id">>, Value, Type, JObj, _PrePath) ->
    ValueKeyPath = [<<"id">>],
    DecodedValue = kz_postgresql_util:decode_query_value(Type, Value),
    add_value_to_jobj(ValueKeyPath, DecodedValue, JObj);

%% Handle all view columns that begin with '_view_key_'
%% View Related cases should ignore PrePath (_view_****)
add_postgresql_value_to_jobj(_TableOrViewName, <<"_view_key_", _KeyInt/binary>>, Value, Type, JObj, _PrePath) ->
    ValueKeyPath = [<<"key">>],
    DecodedValue = kz_postgresql_util:decode_query_value(Type, Value),
    add_value_to_jobj(ValueKeyPath, DecodedValue, JObj);

%% Handle '_view_value' column
%% View Related cases should ignore PrePath (_view_****)
add_postgresql_value_to_jobj(_TableOrViewName, <<"_view_value">>, Value, Type, JObj, _PrePath) ->
    ValueKeyPath = [<<"value">>],
    DecodedValue = kz_postgresql_util:decode_query_value(Type, Value),
    add_value_to_jobj(ValueKeyPath, DecodedValue, JObj);

%% Handle all view columns that begin with '_view_value_'
%% View Related cases should ignore PrePath (_view_****)
add_postgresql_value_to_jobj(_TableOrViewName, <<"_view_value_", ColumnName/binary>>, Value, Type, JObj, _PrePath) ->
    ValueKeyPath = [<<"value">> | [ColumnName]],
    DecodedValue = kz_postgresql_util:decode_query_value(Type, Value),
    add_value_to_jobj(ValueKeyPath, DecodedValue, JObj);

%% Ignore any other _view_****** columns
add_postgresql_value_to_jobj(_TableOrViewName, <<"_view_",_Ignore/binary>>, _Value, _Type, JObj, _PrePath) ->
    JObj;

%% Handle all other columns
add_postgresql_value_to_jobj(TableOrViewName, ColumnName, Value, Type, JObj, PrePath) ->
    TableName = kz_postgresql_schema:pg_view_name_to_pg_table_name(TableOrViewName),
    KeyPath = PrePath ++ kz_postgresql_schema:pg_table_and_column_to_doc_key_path(TableName, ColumnName),
    DecodedValue = kz_postgresql_util:decode_query_value(Type, Value),
    add_value_to_jobj(KeyPath, DecodedValue, JObj).

%%------------------------------------------------------------------------------
%% @doc Add a value by key path to a kz_json object ensuring to merge values
%% if key paths are already defined
%% @end
%%------------------------------------------------------------------------------
-spec add_value_to_jobj(kz_term:ne_binary(), kz_json:json_term(), kz_json:object()) ->
                               kz_json:object().
add_value_to_jobj(KeyPath, Value, JObj) ->
    case kz_json:get_value(KeyPath, JObj) of
        'undefined' ->
            %% No existing value at keypath
            kz_json:set_value(KeyPath, Value, JObj);
        ExistingValue ->
            %% There is an existing value at keypath, Merge values
            MergedValues = merge_values(ExistingValue, Value),
            kz_json:set_value(KeyPath, MergedValues, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc Merge 2 values
%% When lists are involved, always add to the end of the list
%% @end
%%------------------------------------------------------------------------------
-spec merge_values(kz_json:object() | kz_term:ne_binary() | integer() | list()
                  ,kz_json:object() | kz_term:ne_binary() | integer() | list()) ->
                          kz_json:object() | list().
merge_values({_}=JObj1, {_}=JObj2) -> kz_json:merge(JObj1, JObj2);
merge_values(List, NotList) when is_list(List), not is_list(NotList) -> lists:append(List, [NotList]);
merge_values(List1, List2) when is_list(List1), is_list(List2)-> lists:append(List1, List2);
merge_values(Any1, Any2) -> [Any1, Any2].
