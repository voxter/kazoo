%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @author Daniel Finke <danielfinke2011@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
-module(amimulator_originate_resp).

-export([init/1
        ,bindings/1
        ,responders/1
        ,handle_event/2
        ]).

-include("../amimulator.hrl").

%%------------------------------------------------------------------------------
%% @doc Initialize the module
%% @end
%%------------------------------------------------------------------------------
-spec init(kz_term:ne_binary()) -> 'ok'.
init(_) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Return bindings required by the module
%% @end
%%------------------------------------------------------------------------------
-spec bindings(kz_term:proplist()) -> kz_term:proplist().
bindings(_) -> [].

%%------------------------------------------------------------------------------
%% @doc Return responders handled by the module's handle_event/2
%% @end
%%------------------------------------------------------------------------------
-spec responders(kz_term:proplist()) -> kz_term:proplist().
responders(_) ->
    [{<<"resource">>, <<"*">>}].

%%------------------------------------------------------------------------------
%% @doc Handle events supported by the module
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_event(JObj, Props) ->
    handle_specific_event(kz_util:get_event_type(JObj)
                         ,JObj
                         ,Props).

%%------------------------------------------------------------------------------
%% @doc Handle events supported by the module, with specific clauses for
%% each unique Event-Category/Event-Name pair
%% @end
%%------------------------------------------------------------------------------
-spec handle_specific_event({kz_term:api_binary(), kz_term:api_binary()}, kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_specific_event({<<"resource">>, <<"originate_resp">>}, JObj, _) ->
    CallId = kz_json:get_ne_binary_value(<<"Call-ID">>, JObj),
    Call = ami_sm:call(CallId),
    Exten = case kz_json:is_true([<<"Custom-Channel-Vars">>, <<"Flip-Direction-On-Bridge">>], JObj) of
                'true' ->
                    %% From user won't be set yet (not bridged) - use other_id_number
                    amimulator_call:other_id_number(Call);
                'false' -> amimulator_call:to_user(Call)
            end,
    %% TODO: dynamic response/reason
    Response = <<"Success">>,
    Reason = 4,

    Payload = [{<<"Event">>, <<"OriginateResponse">>}
              ,{<<"Privilege">>, <<"call,all">>}
              ,{<<"ActionID">>, kz_json:get_ne_binary_value(<<"Msg-ID">>, JObj)}
              ,{<<"Response">>, Response}
              ,{<<"Channel">>, amimulator_call:channel(Call)}
              ,{<<"Context">>, <<"from-internal">>}
              ,{<<"Exten">>, Exten}
              ,{<<"Reason">>, Reason}
              ,{<<"Uniqueid">>, CallId}
              ,{<<"CallerIDNum">>, amimulator_call:other_id_number(Call)}
              ,{<<"CallerIDName">>, amimulator_call:other_id_name(Call)}
              ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call));
handle_specific_event(_, _, _) -> 'ok'.
