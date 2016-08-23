%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(cf_kvs_set).

-export([handle/2]).
-export([get_kv/2, get_kv/3]).
-export([add_kvs_to_props/2]).
-export([format_json/1]).

-include("../callflow.hrl").

-define(KVS_DB, <<"kvs_collections">>).
-define(COLLECTION_KVS, <<"Custom-KVS">>).
-define(COLLECTION_MODE, <<"KVS-Mode">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Call2 = set_kvs(kz_json:delete_key(<<"kvs_mode">>, Data)
                    ,set_kvs_mode(kz_json:get_value(<<"kvs_mode">>, Data), Call)
                   ),
    cf_exe:set_call(Call2),
    cf_exe:continue(Call2).

get_kv(Key, Call) ->
    get_kv(?COLLECTION_KVS, Key, Call).

get_kv(Collection, Key, Call) ->
    kz_json:get_value(Key, get_collection(Collection, Call)).

set_kvs(Data, Call) ->
    lists:foldl(fun(Key, Call1) ->
                        Value = evaluate(kz_json:get_value(Key, Data), Call1),
                        set_kvs_collection(Key, Value, Call1)
                end
                ,Call
                ,kz_json:get_keys(Data)).

set_kvs_mode(Mode, Call) -> set_collection(?COLLECTION_MODE, <<"kvs_mode">>, Mode, Call).

evaluate(Key, Call) ->
    case digits_key(Key) of
        {'error', JObj} -> JObj;
        'false' -> evaluate2(Key, Call);
        CollectionName -> kapps_call:get_dtmf_collection(CollectionName, Call)
    end.

evaluate2(<<"$", Key/binary>>, Call) ->
    get_kv(Key, Call);
evaluate2(Value, Call) ->
    evaluate_ui(Value, Call).

-spec digits_key(ne_binary()) -> ne_binary() | {'error', kz_json:object()} | 'false'.
digits_key(<<"$_digits">>) ->
    <<"default">>;
digits_key(<<"$_digits[", CollectionName/binary>> = Key) when byte_size(CollectionName) > 0 ->
    case binary:part(CollectionName, byte_size(CollectionName), -1) of
        <<"]">> -> binary:part(CollectionName, 0, byte_size(CollectionName) - 1);
        _ -> digit_evaluation_error(Key)
    end;
digits_key(<<"$_digits[">> = Key) ->
    digit_evaluation_error(Key);
digits_key(_) -> 'false'.

-spec digit_evaluation_error(ne_binary()) -> {'error', kz_json:json_term()}.
digit_evaluation_error(Key) ->
    Msg = "invalid kv lookup key used",
    lager:info(Msg ++ ": ~s", [Key]),
    {'error', kz_json:from_list([{<<"error">>, kz_util:to_binary(Msg)}
                                 ,{<<"key">>, Key}
                                ])}.

-spec evaluate_ui(kz_json:json_term() | 'undefined', kapps_call:call()) -> kz_json:json_term() | 'undefined'.
evaluate_ui(Value, Call) ->
    case kz_json:is_json_object(Value) of
        'true' -> evaluate_ui(kz_json:get_keys(Value), Value, Call);
        'false' -> Value
    end.

-spec evaluate_ui(kz_json:keys(), kz_json:object(), kapps_call:call()) -> kz_json:json_term() | 'undefined'.
evaluate_ui([<<"type">>, <<"value">>], Value, Call) ->
    KeyToCheck = kz_json:get_value(<<"value">>, Value),
    kz_json:set_value(<<"value">>, evaluate(KeyToCheck, Call), Value);
evaluate_ui(_, Value, _) ->
    Value.

-spec get_kvs_collection(kapps_call:call()) -> api_binary().
get_kvs_collection(Call) ->
    get_collection(?COLLECTION_KVS, Call).

get_collection(Collection, Call) ->
    kz_json:get_value(Collection, kapps_call:kvs_fetch(?KVS_DB, kz_json:new(), Call), kz_json:new()).

-spec set_kvs_collection(ne_binary(), ne_binary(), kapps_call:call()) -> kapps_call:call().
set_kvs_collection(Key, Value, Call) ->
    set_collection(?COLLECTION_KVS, Key, Value, Call).
    
set_collection(Collection, Key, Value, Call) ->
    Collections = kapps_call:kvs_fetch(?KVS_DB, kz_json:new(), Call),
    OldCollection = get_collection(Collection, Call),
    NewCollection = kz_json:set_value(Key, Value, OldCollection),
    kapps_call:kvs_store(
    	?KVS_DB,
        kz_json:set_value(Collection, NewCollection, Collections),
        Call
    ).
    
add_kvs_to_props(Props, Call) ->
    case kz_json:get_value(<<"kvs_mode">>, get_collection(?COLLECTION_MODE, Call), undefined) of
        <<"json">> ->
            Collection = get_kvs_collection(Call),
            Keys = kz_json:get_keys(Collection),
            
            lists:foldl(fun(Key, Props2) ->
                [{Key, list_to_binary(format_json(kz_json:get_value(Key, Collection)))}] ++ Props2
                end, Props, Keys);
        _ ->
            [{<<"Custom-KVS">>, get_kvs_collection(Call)}] ++ Props
    end.
    
format_json(Data) ->
    Proplist = case kz_json:is_json_object(Data) of
        true ->
            kz_json:to_proplist(Data);
        _ ->
            Data
    end,
    
    format_json_rec(Proplist).
    
format_json_rec(Proplist) ->
    format_json_rec(Proplist, []).
    
format_json_rec([{K,V}], []) ->
    "{" ++ format_json_rec({K, V}) ++ "}";
    
format_json_rec([{K,V}], _) ->
    format_json_rec({K, V}) ++ "}";
    
format_json_rec([{K,V}=KV|Others], []) ->
    "{" ++ format_json_rec({K, V}) ++ "," ++ format_json_rec(Others, [KV]);
    
format_json_rec([{K,V}=KV|Others], Done) ->
    format_json_rec({K, V}) ++ "," ++ format_json_rec(Others, [KV] ++ Done);
    
format_json_rec({K, V}, _) ->
    "\"" ++ binary_to_list(K) ++ "\":" ++ format_json_rec(V, []);
    
format_json_rec([Prim], []) ->
    "[" ++ format_type(Prim) ++ "]";
    
format_json_rec([Prim], _) ->
    format_type(Prim) ++ "]";
    
format_json_rec([Prim|Others], []) ->
    "[" ++ format_type(Prim) ++ "," ++ format_json_rec(Others, [Prim]);
    
format_json_rec([Prim|Others], _) ->
    format_type(Prim) ++ "," ++ format_json_rec(Others, [Prim]);

format_json_rec(V, _) ->
    "\"" ++ kz_util:to_list(V) ++ "\"".
    
format_type(Data) when not is_binary(Data) ->
    kz_util:to_list(Data);
    
format_type(<<Data/binary>>) ->
    "\"" ++ binary_to_list(Data) ++ "\"".
 
