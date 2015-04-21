-module(amimulator_call).

-export([init/1, bindings/1, responders/1, handle_event/1, handle_event/2]).

-include("../amimulator.hrl").

-define(STATE_UP, 6).

%%
%% Public functions
%%

init(AccountId) ->
    wh_hooks:register(AccountId).

bindings(_Props) ->
    [].

responders(_Props) ->
    [].

handle_event(EventJObj) ->
    handle_event(EventJObj, []).
    
handle_event(EventJObj, _Props) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).
    
handle_specific_event(<<"CHANNEL_CREATE">>=EventName, EventJObj) ->
    new_channel(EventJObj),
    new_state(EventName, EventJObj),
    extension_status(EventJObj),
    maybe_dial_event(EventJObj),
    maybe_change_agent_status(EventJObj);

handle_specific_event(<<"CHANNEL_ANSWER">>, EventJObj) ->
    busy_state(EventJObj),
    maybe_change_agent_status(EventJObj);

handle_specific_event(<<"CHANNEL_BRIDGE">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    OtherCallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj),

    Call = ami_sm:call(CallId),
    WhappsCall = props:get_value(<<"call">>, Call),
    OtherCall = ami_sm:call(OtherCallId),

    Updaters = [
        fun(Call2) -> props:set_value(<<"call">>,
            whapps_call:set_other_leg_call_id(OtherCallId, WhappsCall), Call2) end,
        fun(Call2) -> amimulator_util:bleg_ami_channel(Call2, undefined,
            [{OtherCallId, OtherCall}, {CallId, Call}]) end
    ],
    Call2 = lists:foldl(fun(F, Call3) -> F(Call3) end, Call, Updaters),

    ami_sm:update_call(CallId, Call2),

    Channel1 = props:get_value(<<"aleg_ami_channel">>, Call2),
    Channel2 = props:get_value(<<"bleg_ami_channel">>, Call2),

    Payload = [
        {<<"Event">>, <<"Link">>},
        {<<"Channel1">>, Channel1},
        {<<"Channel2">>, Channel2},
        {<<"Uniqueid1">>, CallId},
        {<<"Uniqueid2">>, OtherCallId}
    ],
    ami_ev:publish_amqp_event({publish, Payload});
    
handle_specific_event(<<"CHANNEL_DESTROY">>, EventJObj) ->
    destroy_channel(EventJObj);
    
handle_specific_event(<<"DTMF">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Digit = wh_json:get_value(<<"DTMF-Digit">>, EventJObj),

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
    
    ami_ev:publish_amqp_event({publish, Payload});
    
handle_specific_event(EventName, _EventJObj) ->
    lager:debug("AMI: unhandled call event ~p", [EventName]).

new_channel(EventJObj) ->
    case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            new_inbound_channel(EventJObj);
        <<"outbound">> ->
            new_outbound_channel(EventJObj)
    end.

new_inbound_channel(EventJObj) ->
    %lager:debug("channel ~p", [EventJObj]),
    Call = amimulator_util:create_call(EventJObj),
    WhappsCall = props:get_value(<<"call">>, Call),
    CallId = whapps_call:call_id(WhappsCall),

    DestExten = props:get_value(<<"bleg_exten">>, Call),
    SourceExten = props:get_value(<<"aleg_exten">>, Call),

    SourceCID = case DestExten of
        SourceExten ->
            <<"Voicemail">>;
        <<"*97">> ->
            <<"Voicemail">>;
        _ ->
            props:get_value(<<"aleg_cid">>, Call)
    end,

    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    ami_sm:new_call(CallId, Call),

    Payload = [
        {<<"Event">>, <<"Newchannel">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, 0},
        {<<"ChannelStateDesc">>, <<"Down">>},
        {<<"CallerIDNum">>, SourceCID},
        {<<"CallerIDName">>, SourceCID},
        {<<"AccountCode">>, <<"">>}, %% Always blank
        {<<"Exten">>, DestExten},
        {<<"Context">>, <<"from-internal">>},
        {<<"Uniqueid">>, CallId}
    ],
    ami_ev:publish_amqp_event({publish, Payload}).

new_outbound_channel(EventJObj) ->
    Call = amimulator_util:create_call(EventJObj),
    WhappsCall = props:get_value(<<"call">>, Call),
    CallId = whapps_call:call_id(WhappsCall),

    SourceCID = props:get_value(<<"aleg_cid">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    ami_sm:new_call(CallId, Call),
    case EndpointName of
        undefined ->
            lager:debug("Call ~p", [Call]),
            lager:debug("EventJObj ~p", [EventJObj]);
        _ ->
            ok
    end,

    Payload = [
        {<<"Event">>, <<"Newchannel">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, 0},
        {<<"ChannelStateDesc">>, <<"Down">>},
        {<<"CallerIDNum">>, SourceCID},
        {<<"CallerIDName">>, SourceCID},
        {<<"AccountCode">>, <<"">>}, %% Always blank
        {<<"Exten">>, <<"">>},
        {<<"Context">>, <<"from-internal">>},
        {<<"Uniqueid">>, CallId}
    ],
    ami_ev:publish_amqp_event({publish, Payload}).

new_state(<<"CHANNEL_CREATE">>, EventJObj) ->
    case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            ring_state(EventJObj);
        <<"outbound">> ->
            ringing_state(EventJObj)
    end.

ring_state(EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),
    WhappsCall = props:get_value(<<"call">>, Call),

    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),
 
    DestExten = props:get_value(<<"bleg_exten">>, Call),
    SourceExten = props:get_value(<<"aleg_exten">>, Call),

    OtherCID = case DestExten of
        SourceExten ->
            <<"Voicemail">>;
        _ ->
            maybe_internal_cid(WhappsCall,
                hd(binary:split(wh_json:get_value(<<"To">>, EventJObj), <<"@">>)))
    end,

    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, 4},
        {<<"ChannelStateDesc">>, <<"Ring">>},
        {<<"CallerIDNum">>, OtherCID},
        {<<"CallerIDName">>, OtherCID},
        {<<"ConnectedLineNum">>, <<"">>},
        {<<"ConnectedLineName">>, <<"">>},
        {<<"Uniqueid">>, CallId}
    ],
    ami_ev:publish_amqp_event({publish, Payload}).

maybe_internal_cid(Call, Number) ->
    case user_for_number(wnm_util:to_e164(Number), whapps_call:account_db(Call)) of
        {error, _E} ->
            Number;
        {ok, UserDoc} ->
            <<(wh_json:get_value(<<"username">>, UserDoc))/binary, " ",
                (wh_json:get_value(<<"first_name">>, UserDoc))/binary, " ",
                (wh_json:get_value(<<"last_name">>, UserDoc))/binary>>
    end.

user_for_number(Number, AccountDb) ->
    case couch_mgr:get_results(AccountDb, <<"callflow/listing_by_number">>, [{key, Number}]) of
        {ok, []} ->
            {error, number_not_found};
        {ok, [Result]} ->
            {ok, CFDoc} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, Result)),
            case maybe_user_in_flow(wh_json:get_value(<<"flow">>, CFDoc)) of
                {error, E} ->
                    {error, E};
                {ok, UserId} ->
                    couch_mgr:open_doc(AccountDb, UserId)
            end
    end.
    
maybe_user_in_flow(Flow) ->
    case wh_json:get_value(<<"module">>, Flow) of
        <<"user">> ->
            {ok, wh_json:get_value(<<"id">>, wh_json:get_value(<<"data">>, Flow))};
        _ ->
            case wh_json:get_value(<<"_">>, wh_json:get_value(<<"flow">>, Flow)) of
                undefined ->
                    {error, invalid_user_extension};
                SubFlow ->
                    maybe_user_in_flow(SubFlow)
            end
    end.

ringing_state(EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),

    SourceCID = props:get_value(<<"aleg_cid">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    OtherCall = ami_sm:call(wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj)),

    DestCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
        undefined ->
            props:get_value(<<"aleg_cid">>, OtherCall);
        QueueId ->
            AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
            {ok, Number} = amimulator_util:find_id_number(
                QueueId,
                wh_util:format_account_id(AccountId, encoded)
            ),
            <<"Queue ", Number/binary, " Call">>
    end,

    case ami_sm:maybe_ringing(EndpointName, CallId) of
        true ->
            Payload = [
                {<<"Event">>, <<"Newstate">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, EndpointName},
                {<<"ChannelState">>, 5},
                {<<"ChannelStateDesc">>, <<"Ringing">>},
                {<<"CallerIDNum">>, SourceCID},
                {<<"CallerIDName">>, SourceCID},
                {<<"ConnectedLineNum">>, DestCID},
                {<<"ConnectedLineName">>, DestCID},
                {<<"Uniqueid">>, CallId}
            ],
            ami_ev:publish_amqp_event({publish, Payload});
        false ->
            ok
    end.

busy_state(EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),

    SourceCID = props:get_value(<<"aleg_cid">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    DestCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
        undefined ->
            case wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj) of
                undefined ->
                    props:get_value(<<"bleg_cid">>, Call);
                OtherCallId ->
                    OtherCall = amimulator_util:get_call(OtherCallId),
                    props:get_value(<<"aleg_cid">>, OtherCall)
            end;
        QueueId ->
            AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
            {ok, Number} = amimulator_util:find_id_number(
                QueueId,
                wh_util:format_account_id(AccountId, encoded)
            ),
            <<"Queue ", Number/binary, " Call">>
    end,

    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, ?STATE_UP},
        {<<"ChannelStateDesc">>, <<"Up">>},
        {<<"CallerIDNum">>, SourceCID},
        {<<"CallerIDName">>, SourceCID},
        {<<"ConnectedLineNum">>, DestCID},
        {<<"ConnectedLineName">>, DestCID},
        {<<"Uniqueid">>, CallId}
    ],
    ami_ev:publish_amqp_event({publish, Payload}).

extension_status(EventJObj) ->
    case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            in_use_status(EventJObj);
        <<"outbound">> ->
            ringing_status(EventJObj)
    end.

in_use_status(EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),

    SourceExten = props:get_value(<<"aleg_exten">>, Call),
    Payload = [
        {<<"Event">>, <<"ExtensionStatus">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Extension">>, SourceExten},
        {<<"Context">>, <<"ext-local">>},
        {<<"Hint">>, <<"SIP/", SourceExten/binary, ",CustomPresence:", SourceExten/binary>>},
        {<<"Status">>, 1}
    ],
    ami_ev:publish_amqp_event({publish, Payload}).

ringing_status(EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),

    SourceExten = props:get_value(<<"aleg_exten">>, Call),
    Payload = [
        {<<"Event">>, <<"ExtensionStatus">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Extension">>, SourceExten},
        {<<"Context">>, <<"ext-local">>},
        {<<"Hint">>, <<"SIP/", SourceExten/binary, ",CustomPresence:", SourceExten/binary>>},
        {<<"Status">>, 8}
    ],
    ami_ev:publish_amqp_event({publish, Payload}).

maybe_dial_event(EventJObj) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj) of
        undefined ->
            ok;
        _OtherCallId ->
            dial_event(EventJObj)
    end.

dial_event(EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),

    DestExten = props:get_value(<<"bleg_exten">>, Call),
    SourceExten = props:get_value(<<"aleg_exten">>, Call),

    CID = case DestExten of
        SourceExten ->
            <<"Voicemail">>;
        <<"*97">> ->
            <<"Voicemail">>;
        _ ->
            props:get_value(<<"aleg_cid">>, Call)
    end,

    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    OtherCallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj),
    OtherCall = ami_sm:call(OtherCallId),

    {OtherCID, OtherEndpointName} = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
        undefined ->
            case wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj) of
                CallId ->
                    {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
                undefined ->
                    {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
                OtherCallId ->
                    OtherCall = ami_sm:call(OtherCallId),
                    {props:get_value(<<"aleg_cid">>, OtherCall), props:get_value(<<"aleg_ami_channel">>, OtherCall)}
            end;
        QueueId ->
            AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
            {ok, Number} = amimulator_util:find_id_number(
                QueueId,
                wh_util:format_account_id(AccountId, encoded)
            ),
            {<<"Queue ", Number/binary, " Call">>, <<>>}
    end,

    %% We need to publish only if the exten matches originally dialed one
    OtherDialed = props:get_value(<<"bleg_exten">>, OtherCall),
    case OtherDialed of
        undefined ->
            ok;
        _ ->
            Payload = [
                {<<"Event">>, <<"Dial">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"SubEvent">>, <<"Begin">>},
                {<<"Channel">>, OtherEndpointName},
                {<<"Destination">>, EndpointName},
                {<<"CallerIDNum">>, OtherCID},
                {<<"CallerIDName">>, OtherCID},
                {<<"ConnectedLineNum">>, CID},
                {<<"ConnectedLineName">>, CID},
                {<<"UniqueID">>, OtherCallId},
                {<<"DestUniqueid">>, CallId},
                {<<"Dialstring">>, CID}
            ],
            ami_ev:publish_amqp_event({publish, Payload})
    end.

destroy_channel(EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),

    SourceCID = props:get_value(<<"aleg_cid">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    DestCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
        undefined ->
            props:get_value(<<"bleg_cid">>, Call);
        QueueId ->
            AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
            {ok, Number} = amimulator_util:find_id_number(
                QueueId,
                wh_util:format_account_id(AccountId, encoded)
            ),
            <<"Queue ", Number/binary, " Call">>
    end,

    {Cause, CauseText} = case wh_json:get_value(<<"Hangup-Cause">>, EventJObj) of
        <<"NORMAL_CLEARING">> ->
            {<<"16">>, <<"Normal Clearing">>};
        _ ->
            {<<"0">>, <<"Not Defined">>}
    end,

    case ami_sm:maybe_ringing(EndpointName, CallId) of
        true ->
            Payload = [[
                {<<"Event">>, <<"Hangup">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, EndpointName},
                {<<"Uniqueid">>, CallId},
                {<<"CallerIDNum">>, SourceCID},
                {<<"CallerIDName">>, SourceCID},
                {<<"ConnectedLineNum">>, DestCID},
                {<<"ConnectedLineName">>, DestCID},
                {<<"Cause">>, Cause},
                {<<"Cause-txt">>, CauseText}
            ]] ++ maybe_leave_conference(CallId),

            maybe_change_agent_status(EventJObj),

            ami_ev:publish_amqp_event({publish, Payload});
        false ->
            ok
    end,

    ami_sm:delete_call(CallId).

maybe_leave_conference(CallId) ->
    case ami_sm:conf_cache(CallId) of
        undefined ->
            [];
        Cache ->
            CallerId = props:get_value(<<"CallerIDnum">>, Cache),
            Timestamp = props:get_value(<<"Timestamp">>, Cache),
            {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
            Duration = (MegaSecs * 1000000 + Secs) - Timestamp,
            [[
                {<<"Event">>, <<"MeetmeLeave">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, props:get_value(<<"Channel">>, Cache)},
                {<<"Uniqueid">>, props:get_value(<<"Uniqueid">>, Cache)},
                {<<"Meetme">>, props:get_value(<<"Meetme">>, Cache)},
                {<<"Usernum">>, props:get_value(<<"Usernum">>, Cache)},
                {<<"CallerIDNum">>, CallerId},
                {<<"CallerIDName">>, CallerId},
                {<<"ConnectedLineNum">>, <<"<unknown>">>},
                {<<"ConnectedLineName">>, <<"<unknown>">>},
                {<<"Duration">>, Duration}
            ]]
    end.

maybe_change_agent_status(EventJObj) ->
    Status = case wh_util:get_event_type(EventJObj) of
        {_, <<"CHANNEL_CREATE">>} ->
            6;
        {_, <<"CHANNEL_ANSWER">>} ->
            2;
        {_, <<"CHANNEL_DESTROY">>} ->
            1
    end,

    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], EventJObj) of
        undefined ->
            ok;
        AuthorizingId ->
            AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
            AccountDb = wh_util:format_account_id(AccountId, encoded),

            case cf_endpoint:get(AuthorizingId, AccountDb) of
                {error, _E} ->
                    ok;
                {ok, Endpoint} ->
                    case wh_json:get_value(<<"owner_id">>, Endpoint) of
                        undefined ->
                            ok;
                        OwnerId ->
                            {ok, UserDoc} = couch_mgr:open_doc(AccountDb, OwnerId),
                            case wh_json:get_value(<<"queues">>, UserDoc) of
                                undefined ->
                                    ok;
                                [] ->
                                    ok;
                                Queues ->
                                    change_agent_status(UserDoc, Queues, EventJObj, Status)
                            end
                    end
            end
    end.

change_agent_status(UserDoc, Queues, EventJObj, Status) ->
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    Username = wh_json:get_value(<<"username">>, UserDoc),
    FirstName = wh_json:get_value(<<"first_name">>, UserDoc),
    LastName = wh_json:get_value(<<"last_name">>, UserDoc),

    Payload = lists:foldl(fun(QueueId, Acc) ->
        case amimulator_util:find_id_number(QueueId, AccountDb) of
            {error, _E} ->
                Acc;
            {ok, QueueNumber} ->
                [[
                    {<<"Event">>, <<"QueueMemberStatus">>},
                    {<<"Queue">>, QueueNumber},
                    {<<"Location">>, <<"Local/", Username/binary, "@from-queue/n">>},
                    {<<"MemberName">>, <<FirstName/binary, " ", LastName/binary>>},
                    {<<"Membership">>, <<"dynamic">>},
                    {<<"Penalty">>, 0},
                    %{<<"LastCall">>, LastCall},
                    {<<"Status">>, Status},
                    {<<"Paused">>, 0}
                ] | Acc]
        end end, [], Queues),

    ami_ev:publish_amqp_event({publish, Payload}).




