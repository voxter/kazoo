%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(cf_kvs_set).

-export([handle/2]).
-export([get_kvs_collection/1]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Keys = wh_json:get_keys(Data),
    Call3 = lists:foldl(
        fun(Key, Call2) -> set_kvs_collection(Key, wh_json:get_value(Key, Data), Call2) end,
            Call,
            Keys
    ),
    lager:debug("The KVS are now: ~p", [get_kvs_collection(Call3)]),
    cf_exe:set_call(Call3),
    cf_exe:continue(Call3).

-spec collection_name() -> ne_binary().
collection_name() ->
    <<"Custom-KVS">>.

-spec get_kvs_collection(whapps_call:call()) -> api_binary().
get_kvs_collection(Call) ->
    wh_json:get_value(collection_name(), whapps_call:kvs_fetch(<<"kvs_collections">>, wh_json:new(), Call), wh_json:new()).

-spec set_kvs_collection(ne_binary(), ne_binary(), whapps_call:call()) -> whapps_call:call().
set_kvs_collection(Key, Value, Call) ->
    Collections = whapps_call:kvs_fetch(<<"kvs_collections">>, wh_json:new(), Call),
    OldCollection = get_kvs_collection(Call),
    lager:debug("Key: ~p, Value: ~p, The OldCollection is: ~p", [Key, Value, OldCollection]),
    NewCollection = wh_json:set_value(Key, Value, OldCollection),
    whapps_call:kvs_store(
    	<<"kvs_collections">>,
        wh_json:set_value(collection_name(), NewCollection, Collections),
        Call
    ).
