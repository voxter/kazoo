%%%-------------------------------------------------------------------
%%% @copyright (C) 2016 Voxter Communications Inc.
%%% @doc
%%%
%%% Data: {
%%%   "id":"queue id"
%%% }
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(cf_acdc_agent_availability).

-export([handle/2]).

-include("../callflow.hrl").

-define(AVAILABLE_BRANCH_KEY, <<"available">>).
-define(UNAVAILABLE_BRANCH_KEY, <<"unavailable">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    QueueId = maybe_use_variable(Data, Call),
    Req = props:filter_undefined([{<<"Account-ID">>, whapps_call:account_id(Call)}
                                  ,{<<"Queue-ID">>, QueueId}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_queue:publish_agents_available_req/1
                                       ,fun wapi_acdc_queue:agents_available_resp_v/1
                                      ) of
        {'error', E} ->
            lager:debug("error ~p when getting agents availability in queue ~s", [E, QueueId]),
            cf_exe:attempt(?AVAILABLE_BRANCH_KEY, Call);
        {'ok', Resp} -> branch_on_availability(wh_json:get_integer_value(<<"Agent-Count">>, Resp), Call)
    end,
    'ok'.

-spec branch_on_availability(non_neg_integer(), whapps_call:call()) -> {'attempt_resp', 'ok' | {'error', 'empty'}}.
branch_on_availability(0, Call) -> cf_exe:attempt(?UNAVAILABLE_BRANCH_KEY, Call);
branch_on_availability(_, Call) -> cf_exe:attempt(?AVAILABLE_BRANCH_KEY, Call).

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
