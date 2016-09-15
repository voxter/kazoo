%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_callflow).

-behaviour(gen_cf_action).

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
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), Id) of
        {'error', R} ->
            lager:info("could not branch to callflow ~s, ~p", [Id, R]),
            cf_exe:continue(Call);
        {'ok', JObj} ->
            lager:info("branching to new callflow ~s", [kz_doc:id(JObj)]),
            Flow = kz_json:get_value(<<"flow">>, JObj, kz_json:new()),
            cf_exe:set_call(kapps_call:kvs_store('cf_flow_id', kz_doc:id(JObj), Call)),
            cf_exe:branch(Flow, Call)
    end.

-spec maybe_use_variable(kz_json:object(), kapps_call:call()) -> api_binary().
maybe_use_variable(Data, Call) ->
    case kz_json:get_value(<<"var">>, Data) of
        'undefined' ->
            kz_doc:id(Data);
        Variable ->
            Value = kz_json:get_value(<<"value">>, cf_kvs_set:get_kv(Variable, Call)),
            case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), Value) of
                {'ok', _} -> Value;
                _ -> kz_doc:id(Data)
            end
    end.
