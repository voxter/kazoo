%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_callflow).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Id = maybe_use_variable(Data, Call),
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call), Id) of
        {'error', R} ->
            lager:info("could not branch to callflow ~s, ~p", [Id, R]),
            cf_exe:continue(Call);
        {'ok', JObj} ->
            lager:info("branching to new callflow ~s", [wh_doc:id(JObj)]),
            Flow = wh_json:get_value(<<"flow">>, JObj, wh_json:new()),
            cf_exe:set_call(whapps_call:kvs_store('cf_flow_id', wh_doc:id(JObj), Call)),
            cf_exe:branch(Flow, Call)
    end.

-spec maybe_use_variable(wh_json:object(), whapps_call:call()) -> api_binary().
maybe_use_variable(Data, Call) ->
    case wh_json:get_value(<<"var">>, Data) of
        'undefined' ->
            wh_doc:id(Data);
        Variable ->
            Value = wh_json:get_value(<<"value">>, cf_kvs_set:get_kv(Variable, Call)),
            case couch_mgr:open_cache_doc(whapps_call:account_db(Call), Value) of
                {'ok', _} -> Value;
                _ -> wh_doc:id(Data)
            end
    end.
