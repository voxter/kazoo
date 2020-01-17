%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc Postgresql db query operation functions
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_query).
-include("kz_postgresql.hrl").

-export([execute_query/2, execute_query/3
        ,execute_query_with_transaction/2
        ]).

-define(COMMON_BUILD_QUERY_ROUTINES, [fun maybe_add_insert_into/2
                                     ,fun maybe_add_values/2
                                     ,fun maybe_add_select/2
                                     ,fun maybe_add_update/2
                                     ,fun maybe_add_set/2
                                     ,fun maybe_add_delete_from/2
                                     ,fun maybe_add_from/2
                                     ,fun maybe_add_inner_join/2
                                     ,fun maybe_add_where/2
                                     ,fun maybe_add_group_by/2
                                     ,fun maybe_add_order_by/2
                                     ,fun maybe_add_limit/2
                                     ,fun maybe_add_offset/2
                                     ,fun maybe_add_returning/2
                                     ]).

%%------------------------------------------------------------------------------
%% @doc Run postgresql query with or without kz_data options
%% @end
%%------------------------------------------------------------------------------
-spec execute_query(kz_postgresql:connection_pool(), kz_postgresql:query_record()) -> epgsql:reply().
execute_query(ConnPool, QueryRecord) ->
    execute_query(ConnPool, QueryRecord, []).

-spec execute_query(kz_postgresql:connection_pool(), kz_postgresql:query_record(), kz_data:options()) -> epgsql:reply().
execute_query(ConnPool, QueryRecord, Options) ->
    QueryRecordWithOpt = kz_postgresql_options:add_options_to_query(ConnPool, QueryRecord, Options),
    QueryIolist = build_query_iolist(QueryRecordWithOpt),
    case get_query_parameters(QueryRecordWithOpt) of
        [] ->
            do_simple_query(ConnPool, QueryIolist);
        QueryParameters ->
            do_extended_query(ConnPool, QueryIolist, QueryParameters)
    end.

%%------------------------------------------------------------------------------
%% @doc Return the list of query parameters from a query record
%% @end
%%------------------------------------------------------------------------------
-spec get_query_parameters(kz_postgresql:query_record()) -> kz_term:ne_binaries().
get_query_parameters(#kz_postgresql_query{'parameters'=Parameters}) ->
    Parameters.

%%------------------------------------------------------------------------------
%% @doc Build iolist query from postgresql_query record
%% @end
%%------------------------------------------------------------------------------
-spec build_query_iolist(kz_postgresql:query_record()) -> kz_postgresql:query().
build_query_iolist(QueryRecord) ->
    Routines = ?COMMON_BUILD_QUERY_ROUTINES ++ [fun add_end_of_query/2],
    build_query_iolist(QueryRecord, Routines).

%%------------------------------------------------------------------------------
%% @doc Build iolist subquery from postgresql_query record
%% @end
%%------------------------------------------------------------------------------
-spec build_sub_query_iolist(kz_postgresql:query_record()) -> kz_postgresql:query().
build_sub_query_iolist(QueryRecord) ->
    build_query_iolist(QueryRecord, ?COMMON_BUILD_QUERY_ROUTINES).

%%------------------------------------------------------------------------------
%% @doc Build binary string query from postgresql_query record and list of defined
%% functions to build the binary string
%% @end
%%------------------------------------------------------------------------------
-spec build_query_iolist(kz_postgresql:query_record(), list()) -> kz_postgresql:query().
build_query_iolist(QueryRecord, Routines) ->
    lists:foldl(fun(Fun, QueryBinary) -> Fun(QueryBinary, QueryRecord) end
               ,[]
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc Add INSERT_INTO part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_insert_into(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_insert_into(Query, #kz_postgresql_query{'insert_into' = 'undefined'}) ->
    Query;
maybe_add_insert_into(Query, #kz_postgresql_query{'insert_into' = {TableName, ColumnsList}}) ->
    ColumnsIolist = lists:join(<<", ">>, ColumnsList),
    [Query, <<"INSERT INTO ">>, TableName, <<"(">> ,ColumnsIolist, <<")">>].

%%------------------------------------------------------------------------------
%% @doc Add VALUES part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_values(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_values(Query, #kz_postgresql_query{'values' = []}) ->
    Query;
maybe_add_values(Query, #kz_postgresql_query{'values' = ValuesList}) ->
    ValuesIolist = generate_values_iolist(ValuesList),
    [Query, <<" VALUES ">>, ValuesIolist].

%%------------------------------------------------------------------------------
%% @doc Add UPDATE part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_update(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_update(Query, #kz_postgresql_query{'update' = 'undefined'}) ->
    Query;
maybe_add_update(Query, #kz_postgresql_query{'update' = TableName}) ->
    [Query, <<"UPDATE ">>, TableName].

%%------------------------------------------------------------------------------
%% @doc Add SET part to PG query binary string if its present in the query record
%% set list will be  list of tuples where each tuple is {ColumnName, Value}
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_set(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_set(Query, #kz_postgresql_query{'set' = []}) ->
    Query;
maybe_add_set(Query, #kz_postgresql_query{'set' = ColumnAndValuesList}) ->
    SetIolist = generate_set_iolist(ColumnAndValuesList),
    [Query, <<" SET ">>, SetIolist].

%%------------------------------------------------------------------------------
%% @doc Add SELECT part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_select(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_select(Query, #kz_postgresql_query{'select' = []}) ->
    Query;
maybe_add_select(Query, #kz_postgresql_query{'select' = SelectList}) ->
    SelectIolist = lists:join(<<", ">>, SelectList),
    [Query, <<"SELECT ">>, SelectIolist].

%%------------------------------------------------------------------------------
%% @doc Add FROM part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_from(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_from(Query, #kz_postgresql_query{'from' = []}) ->
    Query;
maybe_add_from(Query, #kz_postgresql_query{'from' = FromList}) ->
    FromIolist = lists:join(<<", ">>, FromList),
    [Query, <<" FROM ">>, FromIolist].

%%------------------------------------------------------------------------------
%% @doc Add DELETE part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_delete_from(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_delete_from(Query, #kz_postgresql_query{'delete_from' = 'undefined'}) ->
    Query;
maybe_add_delete_from(Query, #kz_postgresql_query{'delete_from' = DeleteTableName}) ->
    [Query, <<"DELETE FROM  ">>, DeleteTableName].

%%------------------------------------------------------------------------------
%% @doc Add WHERE part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_where(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_where(Query, #kz_postgresql_query{'where' = 'undefined'}) ->
    Query;
maybe_add_where(Query, #kz_postgresql_query{'where' = WhereClause}) ->
    WhereIolist = where_clause_to_iolist(WhereClause),
    [Query, <<" WHERE ">>, WhereIolist].

%%------------------------------------------------------------------------------
%% @doc Add INNER JOIN part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_inner_join(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_inner_join(Query, #kz_postgresql_query{'inner_join' = 'undefined'}) ->
    Query;
maybe_add_inner_join(Query, #kz_postgresql_query{'inner_join' = {TableName, JoinList}}) ->
    JoinsIolist = lists:join(<<" AND ">>, JoinList),
    [Query, <<" INNER JOIN \"">>, TableName, <<"\" ON ">>,JoinsIolist].

%%------------------------------------------------------------------------------
%% @doc Add ORDER BY part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_order_by(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_order_by(Query, #kz_postgresql_query{'order_by' = []}) ->
    Query;
maybe_add_order_by(Query, #kz_postgresql_query{'order_by' = OrderByList}) ->
    OrderByFlat = lists:map(fun({Column, Order}) -> [Column, <<" ">> , Order] end ,OrderByList),
    OrderByIolist = lists:join(<<", ">>, OrderByFlat),
    [Query, <<" ORDER BY ">>, OrderByIolist].

%%------------------------------------------------------------------------------
%% @doc Add GROUP BY part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_group_by(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_group_by(Query, #kz_postgresql_query{'group_by' = []}) ->
    Query;
maybe_add_group_by(Query, #kz_postgresql_query{'group_by' = GroupByList}) ->
    GroupByIolist = lists:join(<<", ">>, GroupByList),
    [Query, <<" GROUP BY ">>, GroupByIolist].

%%------------------------------------------------------------------------------
%% @doc Add LIMIT part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_limit(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_limit(Query, #kz_postgresql_query{'limit' = 'undefined'}) ->
    Query;
maybe_add_limit(Query, #kz_postgresql_query{'limit' = Limit}) ->
    [Query, <<" LIMIT ">>, kz_term:to_binary(Limit)].

%%------------------------------------------------------------------------------
%% @doc Add OFFSET part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_offset(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_offset(Query, #kz_postgresql_query{'offset' = 'undefined'}) ->
    Query;
maybe_add_offset(Query, #kz_postgresql_query{'offset' = Offset}) ->
    [Query, <<" OFFSET ">>, kz_term:to_binary(Offset)].

%%------------------------------------------------------------------------------
%% @doc Add RETURNING part to PG query binary string if its present in the query record
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_returning(kz_postgresql:query(), kz_postgresql:query_record()) -> kz_postgresql:query().
maybe_add_returning(Query, #kz_postgresql_query{'returning' = []}) ->
    Query;
maybe_add_returning(Query, #kz_postgresql_query{'returning' = ReturningList}) ->
    ReturningIolist = lists:join(<<", ">>, ReturningList),
    [Query, <<" RETURNING ">>, ReturningIolist].
%%------------------------------------------------------------------------------
%% @doc Add ';' part to PG query
%% @end
%%------------------------------------------------------------------------------
-spec add_end_of_query(kz_term:ne_binary(), kz_postgresql:query_record()) -> kz_term:ne_binary().
add_end_of_query(Query, _QueryRecord) ->
    [Query, <<";">>].

%%------------------------------------------------------------------------------
%% @doc Generate the VALUES string from the nested 'values' list
%% eg [[value1, value2, value3], [valueX, valueY]] would result in (value1, value2, value3), (valueX, valueY)
%% @end
%%------------------------------------------------------------------------------
-spec generate_values_iolist(list(kz_postgresql:values())) -> iolist().
generate_values_iolist(ValueLists) ->
    Values = lists:map(fun(ValueList) -> [<<"(">>, lists:join(<<", ">>, ValueList), <<")">>] end
                      ,ValueLists),
    lists:join(<<", ">>, Values).

%%------------------------------------------------------------------------------
%% @doc Generate the SET string from the list of tuples (ColumnName, Value}
%% eg [{ColumnA, ValueA}, {ColumnB, ValueB}] would result in ColumnA = ValueA, ColumnB = ValueB
%% @end
%%------------------------------------------------------------------------------
-spec generate_set_iolist(list(kz_term:ne_binaries())) -> iolist().
generate_set_iolist(ColumnAndValuesList) ->
    SetIoList = lists:map(fun({ColumnName, Value}) -> [ColumnName, <<"=">>, Value] end
                         ,ColumnAndValuesList),
    lists:join(<<", ">>, SetIoList).

%%------------------------------------------------------------------------------
%% @doc Generate a where clause iolist from a kz_postgresql:where_clause()
%% @end
%%------------------------------------------------------------------------------
-spec where_clause_to_iolist(kz_postgresql:where_clause()) -> iolist().
where_clause_to_iolist(Expr) ->
    where_clause_to_iolist(Expr, [], []).

-spec where_clause_to_iolist(kz_postgresql:where_clause(), list(), iolist()) -> iolist().
where_clause_to_iolist({_, []}, [], Acc) ->
    %% end of an operation, when there are no more operations remaining
    [Acc, <<")">>];
where_clause_to_iolist({_, []}, [{_, []}=Operation|Stack], Acc) ->
    %% end of an operation, when there are no more operands in the parent to process
    Acc1 = [Acc, <<")">>],
    where_clause_to_iolist(Operation, Stack, Acc1);
where_clause_to_iolist({_, []}, [{Operator, [Operand|Operands]}|Stack], Acc) ->
    %% end of an operation, when there are still operands in the parent to process
    Acc1 = [Acc, <<") ">>, Operator, <<" ">>],
    where_clause_to_iolist(Operand, [{Operator, Operands}|Stack], Acc1);
where_clause_to_iolist({Operator, [Operand|Operands]}, Stack, Acc) ->
    %% start of an operation
    Acc1 = [Acc, <<"(">>],
    where_clause_to_iolist(Operand, [{Operator, Operands}|Stack], Acc1);
where_clause_to_iolist(B, [], Acc) ->
    %% lonely expression iolist
    [Acc, B];
where_clause_to_iolist(#kz_postgresql_query{} = Value, [{_, []}=Operation|Stack], Acc) ->
    %% last operand in an operation is a sub query
    SubQueryIolist = build_sub_query_iolist(Value),
    Acc1 = [Acc, <<"(">>, SubQueryIolist, <<")">>],
    where_clause_to_iolist(Operation, Stack, Acc1);
where_clause_to_iolist(Value, [{_, []}=Operation|Stack], Acc) ->
    %% last operand in an operation
    ValueIolist = where_value_to_iolist(Value),
    Acc1 = [Acc, ValueIolist],
    where_clause_to_iolist(Operation, Stack, Acc1);
where_clause_to_iolist(ColumnName, [{Operator, [Operand|Operands]}|Stack], Acc) ->
    %% operands within an operation
    Acc1 = [Acc, ColumnName, <<" ">>, Operator, <<" ">>],
    where_clause_to_iolist(Operand, [{Operator, Operands}|Stack], Acc1).

%%------------------------------------------------------------------------------
%% @doc Convert the value of a where clause to a binary string by doing the following.
%% Convert integers to binary and do not surround in quotes
%% Add single quotes if the value is a binary string and not a PG value/column/parameter placeholder (eg $1 or tablename.column)
%% Convert kz_postgresql_query record to a binary and sorund in brackets
%% @end
%%------------------------------------------------------------------------------
-spec where_value_to_iolist(kz_term:ne_binary() | integer()) -> iolist().
where_value_to_iolist(Int) when is_integer(Int) ->
    [kz_term:to_binary(Int)];
where_value_to_iolist(Bin) when is_binary(Bin) ->
    case binary:match(Bin, [<<"\$">>, <<".">>, <<"\"">>]) of
        'nomatch' -> [<<"'">>, Bin, <<"'">>];
        _Match -> [Bin]
    end.

%%------------------------------------------------------------------------------
%% @doc Run a simple postgresql query
%% Do not log SELECT version kazoo data health check queries to save logs!
%% @end
%%------------------------------------------------------------------------------
-spec do_simple_query(kz_postgresql:connection_pool(), kz_postgresql:query()) -> epgsql:reply().
do_simple_query(ConnPool, [[[],<<"SELECT ">>,[<<"version()">>]],<<";">>] = Query) ->
    pgapp:squery(ConnPool, Query);
do_simple_query(ConnPool, Query) ->
    %% TODO Consider removing debug line in future for performance
    lager:debug("executing postgresql (ConnPool: ~p) simple query: ~p ", [ConnPool, iolist_to_binary(Query)]),
    pgapp:squery(ConnPool, Query).

%%------------------------------------------------------------------------------
%% @doc Run a extended postgresql query
%% @end
%%------------------------------------------------------------------------------
-spec do_extended_query(kz_postgresql:connection_pool(), kz_postgresql:query(), kz_term:ne_binaries()) -> epgsql:reply().
do_extended_query(ConnPool, Query, QueryValues) ->
    %% TODO Consider removing debug line in future for performance
    lager:debug("executing postgresql (ConnPool: ~p) extended query: ~p with values ~p", [ConnPool, iolist_to_binary(Query), QueryValues]),
    pgapp:equery(ConnPool, Query, QueryValues).

%%------------------------------------------------------------------------------
%% @doc Execute a multiple postgresql querys as a transaction with rollback if any query fails
%% @end
%%------------------------------------------------------------------------------
-spec execute_query_with_transaction(kz_postgresql:connection_pool(), fun(() -> epgsql:reply())) -> epgsql:reply().
execute_query_with_transaction(ConnPool, EpgsqlFunctions) ->
    lager:debug("starting postgresql transaction (ConnPool: ~p)", [ConnPool]),
    case pgapp:with_transaction(ConnPool, EpgsqlFunctions) of
        {'rollback', Reason} ->
            lager:error("postgreSQL (ConnPool: ~p) transaction failed, Reason: ~p", [ConnPool, Reason]),
            Reason;
        OkResponse ->
            lager:debug("successfully completed postgresql transaction (ConnPool: ~p)", [ConnPool]),
            OkResponse
    end.