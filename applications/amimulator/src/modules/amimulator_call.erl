-module(amimulator_call).

-export([get_bindings/1, handle_event/1, handle_event/2]).
-export([maybe_internal_cid/1]).

-include("../amimulator.hrl").

-define(STATE_UP, 6).

get_bindings(AccountId) ->
    wh_hooks:register(AccountId),
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
    maybe_dial_event(EventJObj);

handle_specific_event(<<"CHANNEL_ANSWER">>, EventJObj) ->
    busy_state(EventJObj);
    
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
    
    amimulator_amqp:publish_amqp_event({publish, Payload});
    
handle_specific_event(EventName, _EventJObj) ->
    lager:debug("AMI: unhandled call event ~p", [EventName]).

new_channel(EventJObj) ->
    lager:debug("AMI: new channel ~p", [EventJObj]),
    case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            new_inbound_channel(EventJObj);
        <<"outbound">> ->
            new_outbound_channel(EventJObj)
    end.

new_inbound_channel(EventJObj) ->
    DestExten = hd(binary:split(wh_json:get_value(<<"To">>, EventJObj), <<"@">>)),
    Call = amimulator_util:whapps_call(EventJObj),
    CallId = whapps_call:call_id(Call),
    _SourceExten = amimulator_util:maybe_get_exten(Call),
    SourceCID = amimulator_util:maybe_get_cid_name(Call),
    EndpointName = amimulator_util:maybe_get_endpoint_name(Call),

    amimulator_store:store(<<"call-", CallId/binary>>, Call),
    amimulator_store:store(<<"channel-", EndpointName/binary>>, CallId),

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
    amimulator_amqp:publish_amqp_event({publish, Payload}).

new_outbound_channel(EventJObj) ->
    Call = amimulator_util:whapps_call(EventJObj),
    CallId = whapps_call:call_id(Call),
    _SourceExten = amimulator_util:maybe_get_exten(Call),
    SourceCID = amimulator_util:maybe_get_cid_name(Call),
    EndpointName = amimulator_util:maybe_get_endpoint_name(Call),

    amimulator_store:store(<<"call-", CallId/binary>>, Call),
    amimulator_store:store(<<"channel-", EndpointName/binary>>, CallId),

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
    amimulator_amqp:publish_amqp_event({publish, Payload}).

new_state(<<"CHANNEL_CREATE">>, EventJObj) ->
    case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            ring_state(EventJObj);
        <<"outbound">> ->
            ringing_state(EventJObj)
    end.

ring_state(EventJObj) ->
    OtherCID = maybe_internal_cid(EventJObj),
    Call = amimulator_util:whapps_call(EventJObj),
    CallId = whapps_call:call_id(Call),
    _SourceExten = amimulator_util:maybe_get_exten(Call),
    EndpointName = amimulator_util:maybe_get_endpoint_name(Call),
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
    amimulator_amqp:publish_amqp_event({publish, Payload}).

maybe_internal_cid(EventJObj) ->
    Call = amimulator_util:whapps_call(EventJObj),
    Number = hd(binary:split(wh_json:get_value(<<"To">>, EventJObj), <<"@">>)),
    case user_for_number(Number, whapps_call:account_db(Call)) of
        {error, _E} ->
            Number;
        {ok, UserDoc} ->
            <<(wh_json:get_value(<<"first_name">>, UserDoc))/binary, " ",
                (wh_json:get_value(<<"last_name">>, UserDoc))/binary, " <",
                (wh_json:get_value(<<"username">>, UserDoc))/binary, ">">>
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
    Call = amimulator_util:whapps_call(EventJObj),
    CallId = whapps_call:call_id(Call),
    _SourceExten = amimulator_util:maybe_get_exten(Call),
    SourceCID = amimulator_util:maybe_get_cid_name(Call),
    EndpointName = amimulator_util:maybe_get_endpoint_name(Call),
    OtherCall = amimulator_util:whapps_call(whapps_call:other_leg_call_id(Call)),
    _DestExten = amimulator_util:maybe_get_exten(OtherCall),
    DestCID = amimulator_util:maybe_get_cid_name(OtherCall),
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
    amimulator_amqp:publish_amqp_event({publish, Payload}).

busy_state(EventJObj) ->
    %lager:debug("AMI: busy state ~p", [EventJObj]),
    Call = amimulator_util:whapps_call(EventJObj),
    CallId = whapps_call:call_id(Call),
    _Exten = amimulator_util:maybe_get_exten(Call),
    CID = amimulator_util:maybe_get_cid_name(Call),
    EndpointName = amimulator_util:maybe_get_endpoint_name(Call),
    OtherCID = case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            inbound_cid(EventJObj);
        <<"outbound">> ->
            OtherCall = amimulator_store:retrieve(<<"call-", (whapps_call:other_leg_call_id(Call))/binary>>),
            %OtherCall = amimulator_util:whapps_call(whapps_call:other_leg_call_id(Call)),
            amimulator_util:maybe_get_cid_name(OtherCall)
    end,
    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, ?STATE_UP},
        {<<"ChannelStateDesc">>, <<"Up">>},
        {<<"CallerIDNum">>, CID},
        {<<"CallerIDName">>, CID},
        {<<"ConnectedLineNum">>, OtherCID},
        {<<"ConnectedLineName">>, OtherCID},
        {<<"Uniqueid">>, CallId}
    ],
    amimulator_amqp:publish_amqp_event({publish, Payload}).

inbound_cid(EventJObj) ->
    case wh_json:get_value(<<"Callee-ID-Name">>, EventJObj) of
        undefined ->
            inbound_to(EventJObj);
        CalleeIdName ->
            <<CalleeIdName/binary, " <", (inbound_to(EventJObj))/binary, ">">>
    end.

inbound_to(EventJObj) ->
    hd(binary:split(wh_json:get_value(<<"To">>, EventJObj), <<"@">>)).

extension_status(EventJObj) ->
    case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            in_use_status(EventJObj);
        <<"outbound">> ->
            ringing_status(EventJObj)
    end.

in_use_status(EventJObj) ->
    Call = amimulator_util:whapps_call(EventJObj),
    SourceExten = amimulator_util:maybe_get_exten(Call),
    Payload = [
        {<<"Event">>, <<"ExtensionStatus">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Extension">>, SourceExten},
        {<<"Context">>, <<"ext-local">>},
        {<<"Hint">>, <<"SIP/", SourceExten/binary, ",CustomPresence:", SourceExten/binary>>},
        {<<"Status">>, 1}
    ],
    amimulator_amqp:publish_amqp_event({publish, Payload}).

ringing_status(EventJObj) ->
    Call = amimulator_util:whapps_call(EventJObj),
    SourceExten = amimulator_util:maybe_get_exten(Call),
    Payload = [
        {<<"Event">>, <<"ExtensionStatus">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Extension">>, SourceExten},
        {<<"Context">>, <<"ext-local">>},
        {<<"Hint">>, <<"SIP/", SourceExten/binary, ",CustomPresence:", SourceExten/binary>>},
        {<<"Status">>, 8}
    ],
    amimulator_amqp:publish_amqp_event({publish, Payload}).

maybe_dial_event(EventJObj) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj) of
        undefined ->
            ok;
        _OtherCallId ->
            dial_event(EventJObj)
    end.

dial_event(EventJObj) ->
    Call = amimulator_util:whapps_call(EventJObj),
    CallId = whapps_call:call_id(Call),
    _Exten = amimulator_util:maybe_get_exten(Call),
    CID = amimulator_util:maybe_get_cid_name(Call),
    EndpointName = amimulator_util:maybe_get_endpoint_name(Call),
    OtherCall = amimulator_util:whapps_call(whapps_call:other_leg_call_id(Call)),
    _OtherExten = amimulator_util:maybe_get_exten(OtherCall),
    OtherCID = amimulator_util:maybe_get_cid_name(OtherCall),
    OtherEndpointName = amimulator_util:maybe_get_endpoint_name(OtherCall),
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
        {<<"UniqueID">>, whapps_call:other_leg_call_id(Call)},
        {<<"DestUniqueid">>, CallId},
        {<<"Dialstring">>, CID}
    ],
    amimulator_amqp:publish_amqp_event({publish, Payload}).

destroy_channel(EventJObj) ->
    Call = amimulator_util:whapps_call(EventJObj),
    CallId = whapps_call:call_id(Call),
    amimulator_store:delete(<<"call-", CallId/binary>>),
    
    {Cause, CauseText} = case wh_json:get_value(<<"Hangup-Cause">>, EventJObj) of
        <<"NORMAL_CLEARING">> ->
            {<<"16">>, <<"Normal Clearing">>};
        _ ->
            {<<"0">>, <<"Not Defined">>}
    end,

    case cf_endpoint:get(Call) of
        {error, _E} -> ok;
        {ok, _Endpoint} ->
            SourceExten = amimulator_util:maybe_get_exten(Call),
            EndpointName = amimulator_util:maybe_get_endpoint_name(Call),

            amimulator_store:delete(<<"channel-", EndpointName/binary>>),

            Payload = [
                {<<"Event">>, <<"Hangup">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, EndpointName},
                {<<"Uniqueid">>, CallId},
                {<<"CallerIDNum">>, SourceExten},
                {<<"CallerIDName">>, SourceExten},
                {<<"ConnectedLineNum">>, <<"1">>},
                {<<"ConnectedLineName">>, <<"device">>},
                {<<"Cause">>, Cause},
                {<<"Cause-txt">>, CauseText}
            ],

            amimulator_amqp:publish_amqp_event({publish, Payload})
    end.
    
