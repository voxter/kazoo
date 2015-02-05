%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(ecallmgr_config).

-export([flush/0, flush/1, flush/2
         ,flush_default/0, flush_default/1
        ]).
-export([get/1, get/2, get/3
         ,get_integer/1, get_integer/2, get_integer/3
         ,get_boolean/1, get_boolean/2, get_boolean/3
         ,is_true/1, is_true/2, is_true/3
         ,get_default/1, get_default/2
        ]).
-export([fetch/1, fetch/2, fetch/3]).
-export([set/2
         ,set_default/2
         ,set_node/2
        ]).

-compile([{'no_auto_import', [get/1]}]).

-include("ecallmgr.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-spec flush() -> 'ok'.
-spec flush(wh_json:key()) -> 'ok' | {'error', _}.

flush() ->
    wh_cache:flush_local(?ECALLMGR_UTIL_CACHE),
    flush('undefined').

flush(Key) ->
    flush(Key, '_').

-spec flush(api_binary(), atom() | ne_binary()) -> 'ok'.
flush(Key, 'undefined') ->
    flush(Key);
flush(Key, Node) when not is_binary(Key), Key =/= 'undefined' ->
    flush(wh_util:to_binary(Key), Node);
flush(Key, Node) ->
    CacheKey = cache_key(Key, Node),
    wh_cache:erase_local(?ECALLMGR_UTIL_CACHE, CacheKey),
    Req = [{<<"Category">>, <<"ecallmgr">>}
           ,{<<"Key">>, Key}
           ,{<<"Node">>, Node}
           ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("flushing ~s from sysconf", [Key]),
    wh_amqp_worker:cast(props:filter_undefined(Req)
                        ,fun wapi_sysconf:publish_flush_req/1
                       ).

-spec flush_default() -> 'ok'.
-spec flush_default(api_binary()) -> 'ok'.
flush_default() ->
    flush('undefined', <<"default">>).
flush_default(Key) ->
    flush(Key, <<"default">>).

-spec get(wh_json:key()) -> wh_json:json_term() | 'undefined'.
-spec get(wh_json:key(), Default) -> wh_json:json_term() | Default.
-spec get(wh_json:key(), Default, wh_json:key() | atom()) -> wh_json:json_term() | Default.

get(Key) ->
    get(Key, 'undefined').

get(Key, Default) ->
    get(Key, Default, wh_util:to_binary(node())).

get(Key, Default, 'undefined') ->
    get(Key, Default);
get(Key, Default, Node) when not is_binary(Key) ->
    get(wh_util:to_binary(Key), Default, Node);
get(Key, Default, Node) when not is_binary(Node) ->
    get(Key, Default, wh_util:to_binary(Node));
get(Key, Default, Node) ->
    case wh_cache:fetch_local(?ECALLMGR_UTIL_CACHE, cache_key(Key, Node)) of
        {'ok', V} -> V;
        {'error', E} when E =:= 'not_found' orelse E =:= 'undefined' ->
            fetch(Key, Default, Node)
    end.

-spec get_default(wh_json:key()) -> wh_json:json_term() | 'undefined'.
-spec get_default(wh_json:key(), Default) -> wh_json:json_term() | Default.
get_default(Key) ->
    get(Key, 'undefined', <<"default">>).
get_default(Key, Default) ->
    get(Key, Default, <<"default">>).

-spec get_integer(wh_json:key()) -> api_integer().
-spec get_integer(wh_json:key(), Default) -> integer() | Default.
-spec get_integer(wh_json:key(), Default, wh_json:key() | atom()) -> integer() | Default.
get_integer(Key) ->
    get_integer(Key, 'undefined').

get_integer(Key, Default) ->
    case get(Key, Default) of
        Default -> Default;
        N -> wh_util:to_integer(N)
    end.

get_integer(Key, Default, Node) ->
    case get(Key, Default, Node) of
        Default -> Default;
        N -> wh_util:to_integer(N)
    end.

-spec get_boolean(wh_json:key()) -> api_boolean().
-spec get_boolean(wh_json:key(), Default) -> boolean() | Default.
-spec get_boolean(wh_json:key(), Default, wh_json:key() | atom()) -> boolean() | Default.
get_boolean(Key) ->
    get_boolean(Key, 'undefined').

get_boolean(Key, Default) ->
    case get(Key, Default) of
        Default -> Default;
        N -> wh_util:to_boolean(N)
    end.

get_boolean(Key, Default, Node) ->
    case get(Key, Default, Node) of
        Default -> Default;
        N -> wh_util:to_boolean(N)
    end.

-spec is_true(wh_json:key()) -> boolean().
-spec is_true(wh_json:key(), Default) -> boolean() | Default.
-spec is_true(wh_json:key(), Default, wh_json:key() | atom()) -> boolean() | Default.
is_true(Key) ->
    wh_util:is_true(?MODULE:get(Key)).

is_true(Key, Default) ->
    case ?MODULE:get(Key, Default) of
        Default -> Default;
        N -> wh_util:is_true(N)
    end.

is_true(Key, Default, Node) ->
    case ?MODULE:get(Key, Default, Node) of
        Default -> Default;
        N -> wh_util:is_true(N)
    end.

-spec fetch(wh_json:key()) -> wh_json:json_term() | 'undefined'.
-spec fetch(wh_json:key(), Default) -> wh_json:json_term() | Default.
-spec fetch(wh_json:key(), Default, wh_json:key() | atom()) -> wh_json:json_term() | Default.

fetch(Key) ->
    fetch(Key, 'undefined').

fetch(Key, Default) ->
    fetch(Key, Default, wh_util:to_binary(node())).

fetch(Key, Default, 'undefined') ->
    fetch(Key, Default);
fetch(Key, Default, Node) when not is_binary(Key) ->
    fetch(wh_util:to_binary(Key), Default, Node);
fetch(Key, Default, Node) when not is_binary(Node) ->
    fetch(Key, Default, wh_util:to_binary(Node));
fetch(Key, Default, Node) ->
    Req = [{<<"Category">>, <<"ecallmgr">>}
           ,{<<"Key">>, Key}
           ,{<<"Default">>, Default}
           ,{<<"Node">>, Node}
           ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("looking up '~s' from sysconf", [Key]),
    ReqResp = wh_amqp_worker:call(props:filter_undefined(Req)
                                  ,fun wapi_sysconf:publish_get_req/1
                                  ,fun wapi_sysconf:get_resp_v/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:debug("unable to get config for key '~s' failed: ~p", [Key, _R]),
            Default;
        {'ok', JObj} ->
            Value = get_response_value(JObj, Default),
            _ = maybe_cache_resp(Key, Node, Value),
            Value
    end.

-spec maybe_cache_resp(ne_binary(), ne_binary(), term()) -> 'ok'.
maybe_cache_resp(_, _ , 'undefined') -> 'ok';
maybe_cache_resp(_, _ , 'null') -> 'ok';
maybe_cache_resp(_, _ , <<"undefined">>) -> 'ok';
maybe_cache_resp(_, _ , <<"null">>) -> 'ok';
maybe_cache_resp(Key, Node, Value) ->
    CacheProps = [{'origin', {'db', ?WH_CONFIG_DB, <<"ecallmgr">>}}],
    wh_cache:store_local(?ECALLMGR_UTIL_CACHE
                         ,cache_key(Key, Node)
                         ,Value
                         ,CacheProps).

-spec set(wh_json:key(), wh_json:json_term()) -> 'ok'.
set(Key, Value) ->
    set(Key, Value, wh_util:to_binary(node()), []).

-spec set_default(wh_json:key(), wh_json:json_term()) -> 'ok'.
set_default(Key, Value) ->
    set(Key, Value, <<"default">>, []).

-spec set_node(wh_json:key(), wh_json:json_term()) -> 'ok'.
set_node(Key, Value) ->
    set(Key, Value, wh_util:to_binary(node()), [{'node_specific', 'true'}]).

-spec set(wh_json:key(), wh_json:json_term(), wh_json:key(), wh_proplist()) -> 'ok'.
set(Key, Value, Node, Opt) when not is_binary(Key) ->
    set(wh_util:to_binary(Key), Value, Node, Opt);
set(Key, Value, Node, Opt) ->
    Props = [{<<"Category">>, <<"ecallmgr">>}
             ,{<<"Key">>, Key}
             ,{<<"Value">>, Value}
             ,{<<"Node-Specific">>, props:is_true('node_specific', Opt, 'false')}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    Req = [{<<"Node">>, Node}
           | lists:keydelete(<<"Node">>, 1, Props)
          ],
    ReqResp = wh_amqp_worker:call(props:filter_undefined(Req)
                                  ,fun wapi_sysconf:publish_set_req/1
                                  ,fun wh_util:always_true/1
                                 ),
    case ReqResp of
        {'ok', _} -> maybe_cache_resp(Key, Node, Value);
        {'error', _R} ->
            lager:debug("set config for key '~s' failed: ~p", [Key, _R])
    end.

-spec get_response_value(wh_json:object(), term()) -> term().
get_response_value(JObj, Default) ->
    case wh_json:get_value(<<"Value">>, JObj) of
        'undefined' -> Default;
        'null' -> Default;
        <<"undefined">> -> Default;
        <<"null">> -> Default;
        Value -> Value
    end.

-spec cache_key(term(), atom() | ne_binary()) ->
                       {?MODULE, term(), atom() | ne_binary()}.
cache_key(K, Node) -> {?MODULE, K, Node}.
