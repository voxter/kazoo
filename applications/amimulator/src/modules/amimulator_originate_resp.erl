%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voxter Communications Inc.
%%% @doc
%%%
%%% @end
%%% @contributors
%%%    Daniel Finke <danielfinke2011@gmail.com>
%%%-------------------------------------------------------------------
-module(amimulator_originate_resp).

-export([init/1
        ,bindings/1
        ,responders/1
        ,handle_event/2
        ]).

-include("../amimulator.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc Initialize the module
%%--------------------------------------------------------------------
-spec init(ne_binary()) -> 'ok'.
init(_) -> 'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc Return bindings required by the module
%%--------------------------------------------------------------------
-spec bindings(kz_proplist()) -> kz_proplist().
bindings(_) -> [].

%%--------------------------------------------------------------------
%% @public
%% @doc Return responders handled by the module's handle_event/2
%%--------------------------------------------------------------------
-spec responders(kz_proplist()) -> kz_proplist().
responders(_) ->
    [{<<"resource">>, <<"*">>}].

%%--------------------------------------------------------------------
%% @public
%% @doc Handle events supported by the module
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_event(JObj, Props) ->
    handle_specific_event(kz_util:get_event_type(JObj)
                         ,JObj
                         ,Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle events supported by the module, with specific clauses for
%% each unique Event-Category/Event-Name pair
%%--------------------------------------------------------------------
-spec handle_specific_event({api_binary(), api_binary()}, kz_json:object(), kz_proplist()) -> 'ok'.
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
