-module(cf_load_route_vars).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    AccountId = whapps_call:account_id(Call),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    FlowId = whapps_call:kvs_fetch('cf_flow_id', Call),

    Call2 = maybe_load_route_vars(AccountDb, FlowId, Call),
    cf_exe:set_call(Call2),
    cf_exe:continue(Call2).

-spec maybe_load_route_vars(api_binary(), api_binary(), whapps_call:call()) -> whapps_call:call().
maybe_load_route_vars(AccountDb, FlowId, Call) ->
    case couch_mgr:open_doc(AccountDb, <<"route_vars_mapping">>) of
        {'ok', MappingDoc} ->
            %% Can safely assume callflow doc exists since we got here
            {'ok', FlowDoc} = couch_mgr:open_doc(AccountDb, FlowId),
            load_route_vars(MappingDoc, FlowDoc, Call);
        {'error', E} ->
            lager:debug("could not open route vars mapping doc (~p)", [E]),
            Call
    end.

-spec load_route_vars(wh_json:object(), wh_json:object(), whapps_call:call()) -> whapps_call:call().
load_route_vars(MappingDoc, FlowDoc, Call) ->
    load_route_vars_fold(MappingDoc, wh_json:get_value(<<"numbers">>, FlowDoc, []), Call).

-spec load_route_vars_fold(wh_json:object(), list(), whapps_call:call()) -> whapps_call:call().
load_route_vars_fold(_, [], Call) ->
    Call;
load_route_vars_fold(MappingDoc, [Number|Numbers], Call) ->
    NumberCCVs = wh_json:get_value(Number, MappingDoc, wh_json:new()),
    Call3 = lists:foldl(
              fun(Key, Call2) ->
                                Value = wh_json:get_value(Key, NumberCCVs),
                                lager:debug("setting route var ~p to ~p", [Key, Value]),
                                whapps_call:set_custom_channel_var(Key, Value, Call2)
              end
              ,Call
              ,wh_json:get_keys(NumberCCVs)),
    load_route_vars_fold(MappingDoc, Numbers, Call3).
