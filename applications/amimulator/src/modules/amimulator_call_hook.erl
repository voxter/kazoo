-module(amimulator_call_hook).

-export([init/1, bindings/1, responders/1, get_extra_props/1, handle_event/1, handle_event/2]).

-include("../amimulator.hrl").

-define(STATE_UP, 6).

-define(CALLFLOW_CACHE, 'callflow_cache').
-define(CF_CONFIG_CAT, <<"callflow">>).
-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".park">>).

-define(DB_DOC_NAME, kapps_config:get(?MOD_CONFIG_CAT, <<"db_doc_name">>, <<"parked_calls">>)).
-define(DEFAULT_RINGBACK_TM, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_ringback_time">>, 120000)).
-define(PARKED_CALLS_KEY(Db), {'cf_park', 'parked_calls', Db}).

%%
%% Public functions
%%

init(AccountId) ->
    kz_hooks:register(AccountId).

bindings(Props) ->
    AccountId = props:get_value("AccountId", Props),
    [{'route', [{'realm', get_realm(AccountId)}]}].

responders(_Props) ->
    [{<<"dialplan">>, <<"route_req">>}].

get_extra_props(AccountId) ->
    case couch_mgr:get_results(kz_util:format_account_id(AccountId, 'encoded'), <<"callflows/crossbar_listing">>) of
        {'ok', JObjs} -> extract_featurecodes(JObjs);
        {'error', E} -> lager:debug("could not open callflows/crossbar_listing to get featurecodes (~p)", [E])
    end.

handle_event(EventJObj) ->
    handle_event(EventJObj, []).
    
handle_event(EventJObj, Props) ->
    {_EventType, EventName} = kz_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj, Props).
    
handle_specific_event(<<"CHANNEL_CREATE">>, EventJObj, _) ->
	lager:debug("new channel with id ~p", [kz_json:get_value(<<"Call-ID">>, EventJObj)]),

    %% First add the sip version of the call
	Call = amimulator_util:create_call(EventJObj),
    ami_sm:new_call(Call),

    %% Publish the new one plus any Local/ calls that are required for queue calls
    relay_new_calls(maybe_create_agent_calls(Call));

handle_specific_event(<<"CHANNEL_ANSWER">>, EventJObj, _) ->
    lager:debug("channel answer for channel with id ~p", [kz_json:get_value(<<"Call-ID">>, EventJObj)]),
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),

    amimulator_call_sup:relay_answer(CallId),
    amimulator_call_sup:relay_answer(<<CallId/binary, "-queue;1">>),
    amimulator_call_sup:relay_answer(<<CallId/binary, "-queue;2">>);

handle_specific_event(<<"CHANNEL_BRIDGE">>, EventJObj, _) ->
    lager:debug("channel bridge for channel with id ~p to ~p", [kz_json:get_value(<<"Call-ID">>, EventJObj)
                                                                ,kz_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj)]),
    amimulator_call_sup:relay_bridge(EventJObj);

handle_specific_event(<<"CHANNEL_DESTROY">>, EventJObj, _) ->
    lager:debug("channel destroy for channel with id ~p", [kz_json:get_value(<<"Call-ID">>, EventJObj)]),
    % lager:debug("channel destroy ~p", [EventJObj]),
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    HangupCause = kz_json:get_value(<<"Hangup-Cause">>, EventJObj),

    amimulator_call_sup:relay_destroy(HangupCause, CallId),
    amimulator_call_sup:relay_destroy(HangupCause, <<CallId/binary, "-queue;1">>),
    amimulator_call_sup:relay_destroy(HangupCause, <<CallId/binary, "-queue;2">>);
    
handle_specific_event(<<"DTMF">>, EventJObj, _) ->
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    Digit = kz_json:get_value(<<"DTMF-Digit">>, EventJObj),

    Payload = [
        {<<"Event">>, <<"DTMF">>},
        {<<"Privilege">>, <<"dtmf,all">>},
        {<<"Channel">>, CallId},
        {<<"Uniqueid">>, CallId},
        {<<"Digit">>, Digit},
        {<<"Direction">>, <<"Received">>},
        {<<"Begin">>, <<"Yes">>},
        {<<"End">>, <<"No">>}
    ],
    % TODO: Also need to do this with begin/end reversed
    
    amimulator_event_listener:publish_amqp_event({publish, Payload});

handle_specific_event(<<"route_req">>, EventJObj, Props) ->
    % lager:debug("route_req ~p", [EventJObj]),
    % lager:debug("props ~p", [Props]),
    maybe_handle_feature_code(kz_json:get_value(<<"Request">>, EventJObj), EventJObj, Props);

handle_specific_event(EventName, _EventJObj, _) ->
    lager:debug("unhandled call event ~p", [EventName]).

%%
%% Private functions
%%

get_realm(AccountId) ->
    {ok, AccountDoc} = couch_mgr:open_doc(<<"accounts">>, AccountId),
    kz_json:get_value(<<"realm">>, AccountDoc).

-spec extract_featurecodes(kz_json:objects()) -> proplist().
extract_featurecodes(JObjs) ->
    lists:foldl(fun(JObj, Acc) ->
        case kz_json:get_value(<<"featurecode">>, kz_json:get_value(<<"value">>, JObj)) of
            'undefined' -> Acc;
            FeatureCode -> maybe_add_featurecode(kz_json:get_value(<<"name">>, FeatureCode), kz_json:get_value(<<"number">>, FeatureCode), Acc)
        end
    end, [], JObjs).

-spec maybe_add_featurecode(api_binary(), api_binary(), proplist()) -> proplist().
maybe_add_featurecode(<<"park_and_retrieve">>, Number, FeatureCodes) ->
    [{Number, <<"park_and_retrieve">>} | FeatureCodes];
maybe_add_featurecode(_, _, FeatureCodes) ->
    FeatureCodes.

maybe_create_agent_calls(Call) ->
    maybe_create_agent_calls(amimulator_call:ccv(<<"Member-Call-ID">>, Call), Call).

maybe_create_agent_calls('undefined', Call) ->
    [Call];
maybe_create_agent_calls(MemberCallId, Call) ->
    MemberCall = ami_sm:call(MemberCallId),
    create_agent_calls(MemberCall, Call).

create_agent_calls('undefined', Call) ->
    [Call];
create_agent_calls(MemberCall, Call) ->
    LocalCall1 = amimulator_util:fork_agent_call_leg1(Call, MemberCall),
    LocalCall2 = amimulator_util:fork_agent_call_leg2(Call, MemberCall),

    MemberCall2 = amimulator_call:set_other_leg_call_id('undefined', MemberCall),
    MemberCall3 = amimulator_call:set_other_channel('undefined', MemberCall2),

    Call2 = amimulator_call:set_other_leg_call_id('undefined', Call),
    Call3 = amimulator_call:set_other_channel('undefined', Call2),

    ami_sm:new_call(LocalCall1),
    ami_sm:new_call(LocalCall2),
    ami_sm:update_call(MemberCall3),
    ami_sm:update_call(Call3),

    [Call3, LocalCall1, LocalCall2].

relay_new_calls(Calls) ->
    lists:foreach(fun(Call) ->
        amimulator_call_sup:relay_new_call(Call)
    end, Calls).

maybe_handle_feature_code(<<"*2*", Digit:1/binary, Slot:1/binary, _/binary>>, EventJObj, Props) ->
    case props:get_value(Digit, Props) of
        'undefined' -> 'ok';
        <<"park_and_retrieve">> -> handle_retrieve(Digit, Slot, EventJObj);
        _ -> 'ok'
    end;
maybe_handle_feature_code(<<"*", Digit:1/binary, Slot:1/binary, _/binary>>, EventJObj, Props) ->
    case props:get_value(Digit, Props) of
        'undefined' -> 'ok';
        <<"park_and_retrieve">> -> handle_retrieve(Digit, Slot, EventJObj);
        _ -> 'ok'
    end;
maybe_handle_feature_code(_, _, _) ->
    'ok'.

handle_retrieve(Digit, Slot, EventJObj) ->
    ParkedCalls = get_parked_calls(kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj)),
    case retrieve(Slot, ParkedCalls) of
        {'error', 'slot_empty'} -> handle_park(Digit, Slot, EventJObj);
        CallId ->
            Call = ami_sm:call(CallId),
            Payload = [{<<"Event">>, <<"UnParkedCall">>}
                       ,{<<"Privilege">>, <<"call,all">>}
                       ,{<<"Exten">>, <<"*", Digit/binary, Slot/binary>>}
                       ,{<<"Channel">>, amimulator_call:channel(Call)}
                       ,{<<"Parkinglot">>, <<"parkedcalls", Slot/binary>>}
                       ,{<<"From">>, amimulator_call:channel(Call)}
                       ,{<<"CallerIDNum">>, amimulator_call:id_number(Call)}
                       ,{<<"CallerIDName">>, amimulator_call:id_name(Call)}
                       ,{<<"ConnectedLineNum">>, <<"<unknown>">>}
                       ,{<<"ConnectedLineName">>, <<"<unknown>">>}
                       ,{<<"Uniqueid">>, CallId}
                      ],
            amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call))
    end.

handle_park(Digit, Slot, EventJObj) ->
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),
    Payload = [{<<"Event">>, <<"ParkedCall">>}
               ,{<<"Privilege">>, <<"call,all">>}
               ,{<<"Exten">>, <<"*", Digit/binary, Slot/binary>>}
               ,{<<"Channel">>, amimulator_call:channel(Call)}
               ,{<<"Parkinglot">>, <<"parkedcalls", Slot/binary>>}
               ,{<<"From">>, amimulator_call:channel(Call)}
               ,{<<"Timeout">>, ?DEFAULT_RINGBACK_TM div 1000}
               ,{<<"CallerIDNum">>, amimulator_call:id_number(Call)}
               ,{<<"CallerIDName">>, amimulator_call:id_name(Call)}
               ,{<<"ConnectedLineNum">>, <<"<unknown>">>}
               ,{<<"ConnectedLineName">>, <<"<unknown>">>}
               ,{<<"Uniqueid">>, CallId}
              ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call)).

get_parked_calls(AccountId) ->
    get_parked_calls(kz_util:format_account_id(AccountId, 'encoded'), AccountId).

get_parked_calls(AccountDb, AccountId) ->
    case kz_cache:peek_local(?CALLFLOW_CACHE, ?PARKED_CALLS_KEY(AccountDb)) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} ->
            fetch_parked_calls(AccountDb, AccountId)
    end.

fetch_parked_calls(AccountDb, AccountId) ->
    case couch_mgr:open_doc(AccountDb, ?DB_DOC_NAME) of
        {'error', 'not_found'} ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            Generators = [fun(J) -> kz_json:set_value(<<"_id">>, <<"parked_calls">>, J) end
                          ,fun(J) -> kz_json:set_value(<<"pvt_type">>, <<"parked_calls">>, J) end
                          ,fun(J) -> kz_json:set_value(<<"pvt_account_db">>, AccountDb, J) end
                          ,fun(J) -> kz_json:set_value(<<"pvt_account_id">>, AccountId, J) end
                          ,fun(J) -> kz_json:set_value(<<"pvt_created">>, Timestamp, J) end
                          ,fun(J) -> kz_json:set_value(<<"pvt_modified">>, Timestamp, J) end
                          ,fun(J) -> kz_json:set_value(<<"pvt_vsn">>, <<"1">>, J) end
                          ,fun(J) -> kz_json:set_value(<<"slots">>, kz_json:new(), J) end],
            lists:foldr(fun(F, J) -> F(J) end, kz_json:new(), Generators);
        {'ok', JObj} ->
            JObj;
        {'error', _R}=E ->
            lager:info("unable to get parked calls: ~p", [_R]),
            E
    end.

-spec retrieve(ne_binary(), kz_json:object()) ->
                      ne_binary() |
                      {'error', 'slot_empty'}.
retrieve(SlotNumber, ParkedCalls) ->
    case kz_json:get_value([<<"slots">>, SlotNumber], ParkedCalls) of
        'undefined' -> {'error', 'slot_empty'};
        Slot -> kz_json:get_ne_value(<<"Call-ID">>, Slot)
    end.
