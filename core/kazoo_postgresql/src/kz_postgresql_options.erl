%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc Postgresql Options
%%% Extract couch options for use in PG queries
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_options).
-include("kz_postgresql.hrl").
-export([get_keys/1, get_keys/2
        ,get_limit/1, get_limit/2
        ,get_startkey/1, get_startkey/2
        ,get_endkey/1, get_endkey/2
        ,get_order/1, get_order/2
        ,get_doc_type/1, get_doc_type/2
        ,get_include_docs/1, get_include_docs/2
        ,add_options_to_query/3
        ]).

-spec get_keys(kz_data:options()) -> list().
get_keys(CouchOptions) ->
    get_keys(CouchOptions, []).

-spec get_keys(kz_data:options(), any()) -> list().
get_keys(CouchOptions, Default) ->
    props:get_value('keys', CouchOptions, Default).

-spec get_limit(kz_data:options()) -> integer().
get_limit(CouchOptions) ->
    get_limit(CouchOptions, 51).

-spec get_limit(kz_data:options(), any()) -> integer().
get_limit(CouchOptions, Default) ->
    props:get_value('limit', CouchOptions, Default).

-spec get_startkey(kz_data:options()) -> list() | 'undefined'.
get_startkey(CouchOptions) ->
    get_startkey(CouchOptions, 'undefined').

-spec get_startkey(kz_data:options(), any()) ->  list() | 'undefined'.
get_startkey(CouchOptions, Default) ->
    StartKeys = props:get_value('startkey', CouchOptions, Default),
    ensure_list(StartKeys).

-spec get_endkey(kz_data:options()) -> list() | 'undefined'.
get_endkey(CouchOptions) ->
    get_endkey(CouchOptions, 'undefined').

-spec get_endkey(kz_data:options(), any()) -> list() | 'undefined'.
get_endkey(CouchOptions, Default) ->
    EndKeys = props:get_value('endkey', CouchOptions, Default),
    ensure_list(EndKeys).

-spec get_order(kz_data:options()) -> 'ascending' | 'descending'.
get_order(CouchOptions) ->
    get_order(CouchOptions, 'ascending').

-spec get_order(kz_data:options(), atom()) -> 'ascending' | 'descending'.
get_order(CouchOptions, Default) ->
    case lists:filter(fun(E) -> lists:member(E, ['ascending', 'descending']) end, CouchOptions) of
        [Order] -> Order;
        _ -> Default
    end.

-spec get_include_docs(kz_data:options()) -> boolean().
get_include_docs(CouchOptions) ->
    get_include_docs(CouchOptions, 'false').

-spec get_include_docs(kz_data:options(), boolean()) -> boolean().
get_include_docs(CouchOptions, Default) ->
    props:get_value('include_docs', CouchOptions, Default).

-spec get_doc_type(kz_data:options()) -> kz_term:ne_binary() | 'undefined'.
get_doc_type(CouchOptions) ->
    get_doc_type(CouchOptions, 'undefined').

-spec get_doc_type(kz_data:options(), any()) -> kz_term:ne_binary() | any().
get_doc_type(CouchOptions, Default) ->
    props:get_value('doc_type', CouchOptions, Default).

-spec del_startkey(kz_data:options()) -> kz_data:options().
del_startkey(CouchOptions) ->
    props:delete('startkey', CouchOptions).

-spec del_endkey(kz_data:options()) -> kz_data:options().
del_endkey(CouchOptions) ->
    props:delete('endkey', CouchOptions).

-spec ensure_list(list() | any()) -> list().
ensure_list(List) when is_list(List) -> List;
ensure_list(Any) -> [Any].

%%------------------------------------------------------------------------------
%% @doc Convert and add kz_data options to a postgresql query
%% @end
%%------------------------------------------------------------------------------
-spec add_options_to_query(kz_postgresql:connection_pool(), kz_postgresql:query_record(), kz_data:options()) ->
                                  kz_postgresql:query_record().
add_options_to_query(_ConnPool, Query, []) ->
    Query;

%% limit kz_data:option()
%% Add LIMIT Limit
add_options_to_query(ConnPool, Query, [{'limit', Limit} | OtherOptions]) ->
    UpdatedQuery = Query#kz_postgresql_query{'limit' = Limit},
    add_options_to_query(ConnPool, UpdatedQuery, OtherOptions);

%% skip kz_data:option()
%% Add OFFSET Skip
add_options_to_query(ConnPool, Query, [{'skip', Skip} | OtherOptions]) ->
    UpdatedQuery = Query#kz_postgresql_query{'offset' = Skip},
    add_options_to_query(ConnPool, UpdatedQuery, OtherOptions);

%% ascending kz_data:option()
%% View query only
%% Add ORDER BY _view_key_0 ASC, _view_key_N ASC
%% Add 'COLLATE "C"' on character varying and varchar columns to match couch sorting
add_options_to_query(ConnPool
                    ,#kz_postgresql_query{'order_by'=OrderByList, 'from'=[View|[]]}=Query
                    ,['ascending' | OtherOptions]) ->
    case generate_order_by_list(ConnPool, View, <<"ASC">>) of
        [] ->
            lager:error("failed to parse view keys from view for 'ascending' view option"),
            erlang:error("failed_to_add_sort_order");
        OrderByListAdditions ->
            UpdatedQuery = Query#kz_postgresql_query{'order_by' = lists:append(OrderByList, OrderByListAdditions)},
            add_options_to_query(ConnPool, UpdatedQuery, OtherOptions)
    end;

%% descending kz_data:option()
%% View query only
%% Add ORDER BY _view_key_0 DESC, _view_key_N DESC
%% Add 'COLLATE "C"' on character varying and varchar columns to match couch sorting
add_options_to_query(ConnPool
                    ,#kz_postgresql_query{'order_by'=OrderByList, 'from'=[View|[]]}=Query
                    ,['descending' | OtherOptions]) ->
    case generate_order_by_list(ConnPool, View, <<"DESC">>) of
        [] ->
            lager:error("failed to parse view keys from view for 'descending' view option"),
            erlang:error("failed_to_add_sort_order");
        OrderByListAdditions ->
            UpdatedQuery = Query#kz_postgresql_query{'order_by' = lists:append(OrderByList, OrderByListAdditions)},
            add_options_to_query(ConnPool, UpdatedQuery, OtherOptions)
    end;

%% startkey kz_data:option(), {'startkey', [0..N]}
%% View query only
%% Need the Startkeys, EndKeys and Sort order to calculate the 'where; clause to simulate couch start and end keys
add_options_to_query(ConnPool
                    ,#kz_postgresql_query{'where'=Where}=Query
                    ,[{'startkey', StartKeys} | OtherOptions]) ->
    UpdatedQuery = case key_options_to_where_clause(ensure_list(StartKeys)
                                                   ,get_endkey(OtherOptions)
                                                   ,get_order(OtherOptions)) of
                       {} ->
                           Query;
                       KeyWhereClause when Where =:= 'undefined' ->
                           Query#kz_postgresql_query{'where' = KeyWhereClause};
                       KeyWhereClause ->
                           UpdatedWhere = {<<"AND">>, [Where, KeyWhereClause]},
                           Query#kz_postgresql_query{'where' = UpdatedWhere}
                   end,
    add_options_to_query(ConnPool, UpdatedQuery, del_endkey(OtherOptions));

%% endkey kz_data:option(), {'endkey', [0..N]}
%% View query only
%% Need the Startkeys, EndKeys and Sort order to calculate the 'where; clause to simulate couch start and end keys
add_options_to_query(ConnPool
                    ,#kz_postgresql_query{'where'=Where}=Query
                    ,[{'endkey', EndKeys} | OtherOptions]) ->
    UpdatedQuery = case key_options_to_where_clause(get_startkey(OtherOptions, [])
                                                   ,ensure_list(EndKeys)
                                                   ,get_order(OtherOptions)) of
                       {} ->
                           Query;
                       KeyWhereClause when Where =:= 'undefined' ->
                           Query#kz_postgresql_query{'where' = KeyWhereClause};
                       KeyWhereClause ->
                           UpdatedWhere = {<<"AND">>, [Where, KeyWhereClause]},
                           Query#kz_postgresql_query{'where' = UpdatedWhere}
                   end,
    add_options_to_query(ConnPool, UpdatedQuery, del_startkey(OtherOptions));

%% doc_type kz_data:option()
%% Add WHERE pvt_type=DocType, Dependent on pvt_type column being defined for every table
add_options_to_query(ConnPool
                    ,#kz_postgresql_query{'where'='undefined', 'parameters'=ParametersList}=Query
                    ,[{'doc_type', DocType} | OtherOptions]) ->
    NextParamCount = integer_to_binary(length(ParametersList)+1),
    NewWhereClause = {<<"=">>, [<<"data->>'pvt_type'">>
                               ,<<"$",NextParamCount/binary>>
                               ]
                     },
    UpdatedQuery = Query#kz_postgresql_query{'where' = NewWhereClause
                                            ,'parameters' = lists:append(ParametersList, [DocType])},
    add_options_to_query(ConnPool, UpdatedQuery, OtherOptions);
add_options_to_query(ConnPool
                    ,#kz_postgresql_query{'where'=WhereClause, 'parameters'=ParametersList}=Query
                    ,[{'doc_type', DocType} | OtherOptions]) ->
    NextParamCount = integer_to_binary(length(ParametersList)+1),
    UpdatedWhereClause = {<<"AND">>, [WhereClause
                                     ,{<<"=">>, [<<"data->>'pvt_type'">>
                                               ,<<"$",NextParamCount/binary>>
                                               ]}
                                     ]
                         },
    UpdatedQuery = Query#kz_postgresql_query{'where' = UpdatedWhereClause
                                            ,'parameters' = lists:append(ParametersList, [DocType])},
    add_options_to_query(ConnPool, UpdatedQuery, OtherOptions);

%% include_docs kz_data:option()
%% View and non view queries
%% Only supports queries with 1 FROM table/view
add_options_to_query(ConnPool
                    ,#kz_postgresql_query{'from'= [From | []], 'select' = SelectList}=Query
                    ,['include_docs' | OtherOptions]) ->
    %% Parse the table from the view
    FromStripped = binary:replace(From, <<"\"">>, <<"">>, [global]),
    UpdatedQuery = case kz_postgresql_view:is_view(FromStripped) of
                       'false' ->
                           %% The table is not a PG view
                           %% Add table.* to select
                           Select = <<"\"",FromStripped/binary,"\".*">>,
                           Query#kz_postgresql_query{'select' = lists:append(SelectList, [Select])};
                       'true' ->
                           %% The table is a PG view
                           %% Add TABLE.* to select list
                           %% Add INNER JOIN on VIEW._view_id = TABLE._id and VIEW._view_db_name = TABLE.kazoo_db_name
                           TableName = kz_postgresql_schema:pg_view_name_to_pg_table_name(FromStripped),
                           Select = <<"\"",TableName/binary,"\".*">>,
                           InnerJoin = {TableName, [<<From/binary,"._view_id = ",TableName/binary,"._id">>
                                                   ,<<From/binary,"._view_db_name = ",TableName/binary,".kazoo_db_name">>]},
                           Query#kz_postgresql_query{'select' = lists:append(SelectList, [Select])
                                                    ,'inner_join' = InnerJoin}
                   end,
    add_options_to_query(ConnPool, UpdatedQuery, OtherOptions);
add_options_to_query(ConnPool, Query, ['include_docs' | OtherOptions]) ->
    lager:error("failed to convert couch option include_docs to postgresql query, Query must only contain 1 FROM table, ignoring option"),
    add_options_to_query(ConnPool, Query, OtherOptions);

%% Any unhandled options
add_options_to_query(ConnPool, Query, [UnknownOption | OtherOptions]) ->
    lager:error("failed to convert unknown kz_data option '~p' to postgresql query, ignoring option", [UnknownOption]),
    add_options_to_query(ConnPool, Query, OtherOptions).

%%------------------------------------------------------------------------------
%% @doc For a given PG View, generate a list of tuples where each tuple contains
%% the view key N and and the operator defined
%% Add 'COLLATE "C"' on character varying and varchar columns to match couch sorting
%% NOTE, This will only work on PG views
%% @end
%%------------------------------------------------------------------------------
-spec generate_order_by_list(kz_postgresql:connection_pool(), kz_postgresql:view_name(), kz_postgresql:sort_operator()) ->
                                list(kz_postgresql:order_by()).
generate_order_by_list(ConnPool, View, Operator) ->
    ViewStripped = binary:replace(View, <<"\"">>, <<"">>, ['global']),
    Columns = kz_postgresql_schema:get_schema(ConnPool, ViewStripped),
    lists:foldr(fun({<<"_view_key_",_>> = Key, <<"character varying">>}, OrderByAcc) -> [{Key, <<"COLLATE \"C\" ",Operator/binary>>} | OrderByAcc];
                   ({<<"_view_key_",_>> = Key, <<"varchar">>}, OrderByAcc) -> [{Key, <<"COLLATE \"C\" ",Operator/binary>>} | OrderByAcc];
                   ({<<"_view_key_",_>> = Key, _Type}, OrderByAcc) -> [{Key, Operator} | OrderByAcc];
                   (_, OrderByAcc) -> OrderByAcc
                end
               ,[]
               ,Columns).

%%------------------------------------------------------------------------------
%% @doc For a given sort order and lists of startkeys and endkeys, build the equivalent PG where
%% clause to simulate the same response as couch
%% NOTE StartKeys and EndKeys are swaped when descending order is set to replicate couch boundaries
%% http://guide.couchdb.org/draft/views.html#reversed
%% @end
%%------------------------------------------------------------------------------
-spec key_options_to_where_clause(list(), list(), 'ascending' | 'descending') -> kz_postgresql:where_clause().
key_options_to_where_clause(StartKeys, EndKeys, 'ascending') ->
    generate_key_bounds_exp(filter_out_wildcard_keys(StartKeys)
                           ,filter_out_wildcard_keys(EndKeys)
                           ,0);
key_options_to_where_clause(StartKeys, EndKeys, 'descending') ->
    generate_key_bounds_exp(filter_out_wildcard_keys(EndKeys)
                           ,filter_out_wildcard_keys(StartKeys)
                           ,0).

%%------------------------------------------------------------------------------
%% @doc Remove empty kz_json from a list of keys
%% {[]} is used as a wildcard key so we can ignore this
%% @end
%%------------------------------------------------------------------------------
-spec filter_out_wildcard_keys(list()) -> list().
filter_out_wildcard_keys(Keys) ->
    lists:filter(fun(Option) -> not kz_json:is_empty(Option) end, Keys).

%%------------------------------------------------------------------------------
%% @doc For a given lists of startkeys and endkeys, build the equivalent PG where
%% clause to simulate the same response as couch
%% NOTE StartKeys should be greater than or equal to EndKeys as PG applies ordering after where clause
%% @end
%%------------------------------------------------------------------------------
-spec generate_key_bounds_exp(list(), list(), integer()) -> kz_postgresql:where_clause().
generate_key_bounds_exp(StartKeys, EndKeys, KeyCounter) ->
    Inbetween = generate_key_inbetween_bounds(StartKeys, EndKeys, KeyCounter),
    StartBoundry = generate_key_start_boundary(StartKeys, EndKeys, KeyCounter),
    EndBoundry = generate_end_boundary(StartKeys, EndKeys, KeyCounter),
    Equal = generate_equal_check(StartKeys, EndKeys, KeyCounter),
    %% Drop any {}
    case lists:filter(fun({}) -> 'false'; (_) -> 'true' end, [Inbetween, StartBoundry, EndBoundry, Equal]) of
        [] ->
            {};
        OrList ->
            {<<"OR">>, OrList}
    end.

%%------------------------------------------------------------------------------
%% @doc Generate the PG key column name (_view_key_N) where N is supplied
%% @end
%%------------------------------------------------------------------------------
-spec generate_key_column_name(integer()) -> kz_term:ne_binary().
generate_key_column_name(KeyCounter) ->
    KeyCountBin = integer_to_binary(KeyCounter),
    <<"_view_key_", KeyCountBin/binary>>.

%%------------------------------------------------------------------------------
%% @doc Generate the first part of the key bounds query
%% (_view_key_N greater than start_key_N AND _view_key_N less than end_key_N)
%% @end
%%------------------------------------------------------------------------------
-spec generate_key_inbetween_bounds(any(), any(), integer()) -> kz_postgresql:where_clause().
generate_key_inbetween_bounds([], [], _KeyCounter) ->
    {};
generate_key_inbetween_bounds([StartKey | _], [], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    {<<">">>, [ViewKeyName, StartKey]};
generate_key_inbetween_bounds([], [EndKey | _], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    {<<"<">>, [ViewKeyName, EndKey]};
generate_key_inbetween_bounds([StartKey | _], [EndKey | _], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    {<<"AND">>, [{<<">">>, [ViewKeyName, StartKey]}
                ,{<<"<">>, [ViewKeyName, EndKey]}
                ]}.

%%------------------------------------------------------------------------------
%% @doc Generate the second part of the key bounds query
%% (_view_key_N equal StartN AND _view_key_N less than EndN AND
%%    // recurse start
%%    (_view_key_N+1 greater than StartN+1 OR
%%        (_view_key_N+1 equal StartN+1 AND recurse with N+2)
%%    )
%%    // recurse end
%% )
%% @end
%%------------------------------------------------------------------------------
-spec generate_key_start_boundary(list(), list(), integer()) -> kz_postgresql:where_clause().
generate_key_start_boundary([], _EndKeys, _KeyCounter) ->
    {};
generate_key_start_boundary([StartKey | StartKeysTail], [], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    case generate_key_start_boundary_recursive(StartKeysTail, KeyCounter+1) of
        {} ->
            {<<"=">>, [ViewKeyName, StartKey]};
        RecursionPart ->
            {<<"AND">>, [{<<"=">>, [ViewKeyName, StartKey]}
                        ,RecursionPart
                        ]}
    end;
generate_key_start_boundary([StartKey | StartKeysTail], [EndKey | _], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    case generate_key_start_boundary_recursive(StartKeysTail, KeyCounter+1) of
        {} ->
            {<<"AND">>, [{<<"=">>, [ViewKeyName, StartKey]}
                        ,{<<"<">>, [ViewKeyName, EndKey]}
                        ]};
        RecursionPart ->
            {<<"AND">>, [{<<"=">>, [ViewKeyName, StartKey]}
                        ,{<<"<">>, [ViewKeyName, EndKey]}
                        ,RecursionPart
                        ]}
    end.

%%------------------------------------------------------------------------------
%% @doc Generate the recursive part of the second part of the key bounds query
%%    (_view_key_N greater than StartN OR
%%        (_view_key_N equal StartN AND recurse self with N+1)
%%    )
%% @end
%%------------------------------------------------------------------------------
-spec generate_key_start_boundary_recursive(list(), integer()) -> kz_postgresql:where_clause().
generate_key_start_boundary_recursive([], _KeyCounter) ->
    {};
generate_key_start_boundary_recursive([StartKey | StartKeysTail], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    case generate_key_start_boundary_recursive(StartKeysTail, KeyCounter+1) of
        {} ->
            {<<">=">>, [ViewKeyName, StartKey]};
        RecursionPart ->
            {<<"OR">>, [{<<">">>, [ViewKeyName, StartKey]}
                       ,{<<"AND">>, [{<<"=">>, [ViewKeyName, StartKey]}
                                    ,RecursionPart
                                    ]}
                       ]}
    end.

%%------------------------------------------------------------------------------
%% @doc Generate the end boundary (third part) of the key bounds query
%% (_view_key_N greater than StartN AND _view_key_N equal EndN AND
%%    // recurse start
%%    (_view_key_N+1 less than EndN+1 OR
%%        (_view_key_N+1 equal EndN+1 AND recurse with N+2)
%%    )
%%    // recurse end
%% )
%% @end
%%------------------------------------------------------------------------------
-spec generate_end_boundary(list(), list(), integer()) -> kz_postgresql:where_clause().
generate_end_boundary(_StartKeys, [], _KeyCounter) ->
    {};
generate_end_boundary([], [EndKey | EndKeysTail], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    case generate_end_boundary_recursive(EndKeysTail, KeyCounter+1) of
        {} ->
            {<<"=">>, [ViewKeyName, EndKey]};
        RecursionPart ->
            {<<"AND">>, [{<<"=">>, [ViewKeyName, EndKey]}
                        ,RecursionPart
                        ]}
    end;
generate_end_boundary([StartKey | _], [EndKey | EndKeysTail], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    case generate_end_boundary_recursive(EndKeysTail, KeyCounter+1) of
        {} ->
            {<<"AND">>, [{<<">">>, [ViewKeyName, StartKey]}
                        ,{<<"=">>, [ViewKeyName, EndKey]}
                        ]};
        RecursionPart ->
            {<<"AND">>, [{<<">">>, [ViewKeyName, StartKey]}
                        ,{<<"=">>, [ViewKeyName, EndKey]}
                        ,RecursionPart
                        ]}
    end.

%%------------------------------------------------------------------------------
%% @doc Generate the recursive part of the end boundary (third part) of the key bounds query
%%    (_view_key_N less than EndN OR
%%        (_view_key_N equal EndN AND recurse with N+1)
%%    )
%% @end
%%------------------------------------------------------------------------------
-spec generate_end_boundary_recursive(list(), integer()) -> kz_postgresql:where_clause().
generate_end_boundary_recursive([], _KeyCounter) ->
    {};
generate_end_boundary_recursive([EndKey | EndKeysTail], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    case generate_end_boundary_recursive(EndKeysTail, KeyCounter+1) of
        {} ->
            {<<"<=">>, [ViewKeyName, EndKey]};
        RecursionPart ->
            {<<"OR">>, [{<<"<">>, [ViewKeyName, EndKey]}
                       ,{<<"AND">>, [{<<"=">>, [ViewKeyName, EndKey]}
                                    ,RecursionPart
                                    ]}
                       ]}
    end.

%%------------------------------------------------------------------------------
%% @doc Generate the where clause to catch when the startKey[N] equal endKey[N]
%%      (_view_key_N = startN AND _view_key_N = endN AND recurse everything with
%%      N+1)
%% @end
%%------------------------------------------------------------------------------
-spec generate_equal_check(list(), list(), integer()) -> kz_postgresql:where_clause().
generate_equal_check([StartKey | StartKeysTail], [EndKey | EndKeysTail], KeyCounter) ->
    ViewKeyName = generate_key_column_name(KeyCounter),
    case generate_key_bounds_exp(StartKeysTail, EndKeysTail, KeyCounter+1) of
        {} -> {};
        RecursionPart ->
            {<<"AND">>, [{<<"=">>, [ViewKeyName, StartKey]}
                        ,{<<"=">>, [ViewKeyName, EndKey]}
                        ,RecursionPart
                        ]}
    end;
generate_equal_check(_StartKeys, _EndKeys, _KeyCounter) ->
    {}.
