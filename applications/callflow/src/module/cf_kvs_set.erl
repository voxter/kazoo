%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(cf_kvs_set).

-export([handle/2]).
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
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Call2 = set_kvs(Data, Call),
    cf_exe:set_call(Call2),
    cf_exe:continue(Call2).
    
set_kvs(Data, Call) ->
    Call2 = set_kvs_mode(wh_json:get_value(<<"kvs_mode">>, Data, undefined), Call),
    Data2 = wh_json:delete_key(<<"kvs_mode">>, Data),
    Keys = wh_json:get_keys(Data2),
    lists:foldl(fun(Key, Call3) ->
        set_kvs_collection(Key, wh_json:get_value(Key, Data2), Call3) end,
        Call2,
        Keys
    ).
    
set_kvs_mode(undefined, Call) -> Call;
set_kvs_mode(Mode, Call) -> set_collection(?COLLECTION_MODE, <<"kvs_mode">>, Mode, Call).

-spec get_kvs_collection(whapps_call:call()) -> api_binary().
get_kvs_collection(Call) ->
    get_collection(?COLLECTION_KVS, Call).

get_collection(Collection, Call) ->
    wh_json:get_value(Collection, whapps_call:kvs_fetch(?KVS_DB, wh_json:new(), Call), wh_json:new()).

-spec set_kvs_collection(ne_binary(), ne_binary(), whapps_call:call()) -> whapps_call:call().
set_kvs_collection(Key, Value, Call) ->
    set_collection(?COLLECTION_KVS, Key, Value, Call).
    
set_collection(Collection, Key, Value, Call) ->
    Collections = whapps_call:kvs_fetch(?KVS_DB, wh_json:new(), Call),
    OldCollection = get_collection(Collection, Call),
    NewCollection = wh_json:set_value(Key, Value, OldCollection),
    whapps_call:kvs_store(
    	?KVS_DB,
        wh_json:set_value(Collection, NewCollection, Collections),
        Call
    ).
    
add_kvs_to_props(Props, Call) ->
    case wh_json:get_value(<<"kvs_mode">>, get_collection(?COLLECTION_MODE, Call), undefined) of
        <<"json">> ->
            Collection = get_kvs_collection(Call),
            Keys = wh_json:get_keys(Collection),
            
            lists:foldl(fun(Key, Props2) ->
                [{Key, list_to_binary(format_json(wh_json:get_value(Key, Collection)))}] ++ Props2
                end, Props, Keys);
        _ ->
            [{<<"Custom-KVS">>, get_kvs_collection(Call)}] ++ Props
    end.
    
format_json(Data) ->
    Proplist = case wh_json:is_json_object(Data) of
        true ->
            wh_json:to_proplist(Data);
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
    format_json_rec({K, V}) ++ "," ++ format_json_rec(Others, Done ++ KV);
    
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
    "\"" ++ binary_to_list(V) ++ "\"".
    
format_type(Data) when not is_binary(Data) ->
    wh_util:to_list(Data);
    
format_type(<<Data/binary>>) ->
    "\"" ++ binary_to_list(Data) ++ "\"".
    