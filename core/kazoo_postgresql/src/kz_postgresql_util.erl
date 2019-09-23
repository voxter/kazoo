%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc Util functions used by kazoo_postgresql
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_util).
-include("kz_postgresql.hrl").

-export([server_info/1
        ,decode_query_value/2
        ,encode_query_value/2
        ,simulate_couch_doc_revision/1, simulate_couch_doc_revision/2
        ,postgresql_rev_to_couch_rev/2
        ,couch_rev_to_postgresql_rev/1
        ,depluralize_table_name/1
        ]).

%%------------------------------------------------------------------------------
%% @doc Query the PostgreSQL server for version info
%% @end
%%------------------------------------------------------------------------------
-spec server_info(kz_postgresql:connection_pool()) -> {'ok', any()} | {'error', any()}.
server_info(ConnPool) ->
    lager:debug("getting postgresql server info"),
    Query = #kz_postgresql_query{'select' = [<<"version()">>]},
    case kz_postgresql_query:execute_query(ConnPool, Query) of
        {'ok', _, Info} -> {'ok', Info};
        Error ->
            lager:error("postgresql query (~p) failed, Error: ~p", [Query, Error]),
            {'error', [Error]}
    end.

%%------------------------------------------------------------------------------
%% @doc Convert Hex list to binary string
%% @end
%%------------------------------------------------------------------------------
-spec hex_to_binary(list()) -> kz_term:ne_binary().
hex_to_binary(HexList) ->
    list_to_binary([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= HexList]).

%%------------------------------------------------------------------------------
%% @doc Decode postgresql query values to the type defined
%% @end
%%------------------------------------------------------------------------------
-spec decode_query_value(kz_term:ne_binary(), any()) -> any().
decode_query_value(_Type, ?PG_NULL) ->
    ?DOC_NULL;
decode_query_value(Type, Value) ->
    case Type of
        'json' -> kz_json:decode(Value);
        'jsonb' -> kz_json:decode(Value);
        'bool' -> Value;
        'int4' when is_integer(Value) -> Value;
        'int4' when is_binary(Value) -> binary_to_integer(Value);
        'int8' when is_integer(Value) -> Value;
        'int8' when is_binary(Value) -> binary_to_integer(Value);
        'varchar' when is_integer(Value) -> list_to_binary(integer_to_list(Value));
        'varchar' when is_binary(Value) -> Value;
        'text' when is_integer(Value) -> list_to_binary(integer_to_list(Value));
        'text' when is_binary(Value) -> Value;
        Unknown ->
            lager:error("error, unknown type ~p to decode, returning value untouched: ~p", [Unknown, Value]),
            Value
    end.

%%------------------------------------------------------------------------------
%% @doc Encode a JSON value to Postgresql value
%% Type is parsed from PG table column type
%% @end
%%------------------------------------------------------------------------------
-spec encode_query_value(kz_term:ne_binary(), any()) -> any().
encode_query_value(<<"json">>, Value) -> kz_json:encode(Value);
encode_query_value(<<"jsonb">>, []) -> ?PG_NULL;
encode_query_value(<<"jsonb">>, Value) -> kz_json:encode(Value);
encode_query_value(<<"boolean">>, Value) -> Value;
encode_query_value(<<"integer">>, Value) when is_integer(Value) -> Value;
encode_query_value(<<"integer">>, Value) when is_binary(Value) -> binary_to_integer(Value);
encode_query_value(<<"bigint">>, Value) when is_integer(Value) -> Value;
encode_query_value(<<"bigint">>, Value) when is_binary(Value) -> binary_to_integer(Value);
encode_query_value(<<"character varying">>, Value) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
encode_query_value(<<"character varying">>, Value) when is_binary(Value) -> Value;
encode_query_value(<<"character varying">>, Value) when is_atom(Value) -> Value;
encode_query_value(Unknown, Value) ->
    lager:error("error, unknown type ~p to encode, returning value untouched: ~p", [Unknown, Value]),
    Value.


%%------------------------------------------------------------------------------
%% @doc For a JObj/Doc or list of JObjs/Docs, build the doc revision to simulate a couchDB doc revision
%% If the rev is not defined then just return the JObject
%% @end
%%------------------------------------------------------------------------------
-spec simulate_couch_doc_revision(kz_json:object() | kz_json:objects()) -> kz_json:object() | kz_json:objects().
simulate_couch_doc_revision(JObjs) when is_list(JObjs) ->
    simulate_couch_doc_revision(JObjs, [<<"_rev">>], []);
simulate_couch_doc_revision(JObj) ->
    simulate_couch_doc_revision(JObj, [<<"_rev">>]).

-spec simulate_couch_doc_revision(kz_json:object() | kz_json:objects(), kz_type:ne_binaries()) -> kz_json:object() | kz_json:objects().
simulate_couch_doc_revision(JObjs, RevPath) when is_list(JObjs) ->
    simulate_couch_doc_revision(JObjs, RevPath, []);
simulate_couch_doc_revision(JObj, RevPath) ->
    try {kz_json:get_value(RevPath, JObj), kz_doc:id(JObj)} of
        {'undefined', _} -> JObj;
        {_, 'undefined'} -> JObj;
        {PgRev, DocId} ->
            CouchRev = postgresql_rev_to_couch_rev(PgRev, DocId),
            kz_json:set_value(RevPath, CouchRev, JObj)
    catch
        'error':'badarg' -> JObj
    end.

-spec simulate_couch_doc_revision(kz_json:objects(), list(), list()) -> kz_json:objects().
simulate_couch_doc_revision([], _RevPath, JObjsAcc) ->
    lists:reverse(JObjsAcc);
simulate_couch_doc_revision([JObj | JObjs], RevPath, JObjsAcc) ->
    simulate_couch_doc_revision(JObjs, RevPath, [simulate_couch_doc_revision(JObj, RevPath) | JObjsAcc]).

%%------------------------------------------------------------------------------
%% @doc Generate a couchdb like revision for a doc
%% @end
%%------------------------------------------------------------------------------
-spec postgresql_rev_to_couch_rev(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
postgresql_rev_to_couch_rev(PgRev, DocId) when is_integer(PgRev) ->
    postgresql_rev_to_couch_rev(list_to_binary(integer_to_list(PgRev)), DocId);
postgresql_rev_to_couch_rev(PgRev, DocId) ->
    Md5 = erlang:md5(DocId),
    RevSuffix = hex_to_binary(Md5),
    <<PgRev/binary,"-",RevSuffix/binary>>.

%%------------------------------------------------------------------------------
%% @doc Parse the PG revision from a Couch like revision
%% @end
%%------------------------------------------------------------------------------
-spec couch_rev_to_postgresql_rev(kz_term:ne_binary()) -> kz_term:ne_binary().
couch_rev_to_postgresql_rev(PGRev) when is_integer(PGRev) -> PGRev;
couch_rev_to_postgresql_rev(CouchRev) ->
    [PgRev, _] = binary:split(CouchRev, <<"-">>),
    PgRev.

%%------------------------------------------------------------------------------
%% @doc Return the singular table name
%% @end
%%------------------------------------------------------------------------------
-spec depluralize_table_name(kz_postgresql:table_name()) -> kz_term:ne_binary().
depluralize_table_name(Name) ->
    Size = byte_size(Name) - 1,
    case Name of
        <<Bin:Size/binary, "s">> -> Bin;
        Bin -> Bin
    end.
