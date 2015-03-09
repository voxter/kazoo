-module(amimulator_call).

-export([init_bindings/1, handle_event/1, handle_event/2]).

-include("../amimulator.hrl").

-define(STATUS_AVAILABLE, <<"0">>).
-define(STATUS_RING, <<"4">>).
-define(STATUS_RINGING, <<"5">>).
-define(STATUS_BUSY, <<"7">>).
-define(STATE_UP, 6).
%{ /* 0 AST_DEVICE_UNKNOWN */     "Unknown",     "UNKNOWN"     }, /*!< Valid, but unknown state */
%        { /* 1 AST_DEVICE_NOT_INUSE */   "Not in use",  "NOT_INUSE"   }, /*!< Not used */
%        { /* 2 AST_DEVICE IN USE */      "In use",      "INUSE"       }, /*!< In use */
%        { /* 3 AST_DEVICE_BUSY */        "Busy",        "BUSY"        }, /*!< Busy */
%        { /* 4 AST_DEVICE_INVALID */     "Invalid",     "INVALID"     }, /*!< Invalid - not known to Asterisk */
%        { /* 5 AST_DEVICE_UNAVAILABLE */ "Unavailable", "UNAVAILABLE" }, /*!< Unavailable (not registered) */
%        { /* 6 AST_DEVICE_RINGING */     "Ringing",     "RINGING"     }, /*!< Ring, ring, ring */
%        { /* 7 AST_DEVICE_RINGINUSE */   "Ring+Inuse",  "RINGINUSE"   }, /*!< Ring and in use */
%        { /* 8 AST_DEVICE_ONHOLD */      "On Hold",     "ONHOLD"      }, /*!< On Hold */
-define(CALL_BINDING(Event), {'call', [{'restrict_to', [Event]}
                                        ,'federate'
                                       ]}).

init_bindings(_CommPid) ->
    gen_listener:add_binding(
        wh_hooks_listener,
        ?CALL_BINDING(<<"DTMF">>)
    ),
    
    gen_listener:add_responder(
        wh_hooks_listener,
        {'amimulator_call', 'handle_event'},
        [{<<"call_event">>, <<"*">>}]
    ).

handle_event(EventJObj) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).
    
handle_event(EventJObj, _Props) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).
    
handle_specific_event(<<"CHANNEL_CREATE">>, EventJObj) ->
    new_channel(EventJObj),
    ringing_state(EventJObj),
    extension_status_ringing(EventJObj);

handle_specific_event(<<"CHANNEL_ANSWER">>, EventJObj) ->
    busy_state(EventJObj);
    
handle_specific_event(<<"CHANNEL_DESTROY">>, EventJObj) ->
    %lager:debug("AMI: channel destroy ~p", [EventJObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    %MsgId = wh_json:get_value(<<"Msg-ID">>, EventJObj),
    CallerIdNum = wh_json:get_value(<<"Caller-ID-Number">>, EventJObj),
    CallerIdName = wh_json:get_value(<<"Caller-ID-Name">>, EventJObj),
    %Username = wh_json:get_value(<<"Msg-ID">>, EventJObj),
    
    {Cause, CauseText} = case wh_json:get_value(<<"Hangup-Cause">>, EventJObj) of
        <<"NORMAL_CLEARING">> ->
            {<<"16">>, <<"Normal Clearing">>};
        _ ->
            {<<"0">>, <<"Not Defined">>}
    end,

    Call = whapps_call:from_json(EventJObj),
    CCVs = whapps_call:ccvs(Call),
    Call2 = case wh_json:get_value(<<"Authorizing-ID">>, CCVs) of
        undefined ->
            Call;
        AuthId ->
            whapps_call:set_authorizing_id(AuthId, Call)
    end,
    Call3 = case wh_json:get_value(<<"Account-ID">>, CCVs) of
        undefined ->
            Call2;
        AccountId ->
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            whapps_call:set_account_id(AccountId, whapps_call:set_account_db(AccountDb, Call2))
    end,
    case cf_endpoint:get(Call3) of
        {error, _E} -> ok;
        {ok, Endpoint} ->
            EndpointName = case wh_json:get_value(<<"pvt_type">>, Endpoint) of
                <<"device">> ->
                    {ok, EndpointDevice} = couch_mgr:open_doc(whapps_call:account_db(Call3), wh_json:get_value(<<"_id">>, Endpoint)),
                    wh_json:get_value(<<"name">>, EndpointDevice);
                _ ->
                    wh_json:get_value(<<"name">>, Endpoint)
            end,
            Payload = [
                {<<"Event">>, <<"Hangup">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, <<"SIP/", EndpointName/binary, "-00000004">>},
                {<<"Uniqueid">>, CallId},
                {<<"CallerIDNum">>, CallerIdNum},
                {<<"CallerIDName">>, CallerIdName},
                {<<"ConnectedLineNum">>, EndpointName},
                {<<"ConnectedLineName">>, <<"device">>},
                {<<"Cause">>, Cause},
                {<<"Cause-txt">>, CauseText}
            ],

            amimulator_amqp:publish_amqp_event({publish, Payload})
    end;
    
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
    %lager:debug("AMI: new channel ~p", [EventJObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    _CallerIdNum = wh_json:get_value(<<"Caller-ID-Number">>, EventJObj),
    _CallerIdName = wh_json:get_value(<<"Caller-ID-Name">>, EventJObj),

    Call = blackhole_ami_util:whapps_call(EventJObj),
    {Exten, EndpointName} = case cf_endpoint:get(Call) of
        {error, _E} ->
            % TODO: find out proper value for this EndpointName
            {<<"">>, <<"external">>};
        {ok, Endpoint} ->
            Exten2 = blackhole_ami_util:endpoint_name(whapps_call:account_db(Call), Endpoint),
            {Exten2, <<"SIP/", Exten2/binary, "-00000004">>}
    end,
    Payload = case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            To = hd(binary:split(wh_json:get_value(<<"To">>, EventJObj), <<"@">>)),
            [
                {<<"Event">>, <<"Newchannel">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, EndpointName},
                {<<"ChannelState">>, 0},
                {<<"ChannelStateDesc">>, <<"Down">>},
                {<<"CallerIDNum">>, Exten},
                {<<"CallerIDName">>, Exten},
                {<<"AccountCode">>, <<"">>}, %% Always blank
                % TODO: get the exten of call recipient
                {<<"Exten">>, To},
                {<<"Context">>, <<"from-internal">>},
                {<<"Uniqueid">>, CallId}
            ];
        <<"outbound">> ->
            [
                {<<"Event">>, <<"Newchannel">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, EndpointName},
                {<<"ChannelState">>, 0},
                {<<"ChannelStateDesc">>, <<"Down">>},
                {<<"CallerIDNum">>, Exten},
                {<<"CallerIDName">>, Exten},
                {<<"AccountCode">>, <<"">>}, %% Always blank
                % TODO: get the exten of call recipient
                {<<"Exten">>, <<"">>},
                {<<"Context">>, <<"from-internal">>},
                {<<"Uniqueid">>, CallId}
            ];
        _ ->
            lager:debug("AMI: unexpected Call-Direction in new channel")
    end,
    amimulator_amqp:publish_amqp_event({publish, Payload}).

ringing_state(EventJObj) ->
    %lager:debug("AMI: ringing ~p", [EventJObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    _CallerId = wh_json:get_value(<<"Caller-ID-Number">>, EventJObj),

    Call = blackhole_ami_util:whapps_call(EventJObj),
    {Exten, EndpointName} = case cf_endpoint:get(Call) of
        {error, _E} ->
            % TODO: find out proper value for this EndpointName
            {<<"">>, <<"external">>};
        {ok, Endpoint} ->
            Exten2 = blackhole_ami_util:endpoint_name(whapps_call:account_db(Call), Endpoint),
            {Exten2, <<"SIP/", Exten2/binary, "-00000004">>}
    end,
    Payload = case wh_json:get_value(<<"Call-Direction">>, EventJObj) of
        <<"inbound">> ->
            [
                {<<"Event">>, <<"Newstate">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, EndpointName},
                {<<"ChannelState">>, ?STATUS_RING},
                {<<"ChannelStateDesc">>, <<"Ring">>},
                {<<"CallerIDNum">>, Exten},
                {<<"CallerIDName">>, Exten},
                {<<"ConnectedLineNum">>, <<"">>},
                {<<"ConnectedLineName">>, <<"">>},
                {<<"Uniqueid">>, CallId}
            ];
        <<"outbound">> ->
            [
                {<<"Event">>, <<"Newstate">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, EndpointName},
                {<<"ChannelState">>, ?STATUS_RINGING},
                {<<"ChannelStateDesc">>, <<"Ringing">>},
                {<<"CallerIDNum">>, Exten},
                {<<"CallerIDName">>, Exten},
                % TODO: add connectedlinenum/name
                {<<"ConnectedLineNum">>, <<"">>},
                {<<"ConnectedLineName">>, <<"">>},
                {<<"Uniqueid">>, CallId}
            ];
        _ ->
            lager:debug("AMI: unexpected Call-Direction in new channel")
    end,
    amimulator_amqp:publish_amqp_event({publish, Payload}).

busy_state(EventJObj) ->
    lager:debug("AMI: busy state ~p", [EventJObj]),
    Channel = wh_json:get_value(<<"Call-ID">>, EventJObj),
    _CallerId = wh_json:get_value(<<"Caller-ID-Number">>, EventJObj),

    Call = blackhole_ami_util:whapps_call(EventJObj),
    {Exten, EndpointName} = case cf_endpoint:get(Call) of
        {error, _E} ->
            % TODO: find out proper value for this EndpointName
            {<<"">>, <<"external">>};
        {ok, Endpoint} ->
            Exten2 = blackhole_ami_util:endpoint_name(whapps_call:account_db(Call), Endpoint),
            {Exten2, <<"SIP/", Exten2/binary, "-00000004">>}
    end,
    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, ?STATE_UP},
        {<<"ChannelStateDesc">>, <<"Up">>},
        {<<"CallerIDNum">>, Exten},
        {<<"CallerIDName">>, Exten},
        % TODO: add connectlinenum/name
        {<<"ConnectedLineNum">>, <<"">>},
        {<<"ConnectedLineName">>, <<"">>},
        {<<"Uniqueid">>, Channel}
    ],
    amimulator_amqp:publish_amqp_event({publish, Payload}).
    
extension_status_ringing(_EventJObj) ->
    Payload = [
        {<<"Event">>, <<"ExtensionStatus">>},
        {<<"Privilege">>, <<"call, all">>},
        % TODO: add extension (351)
        {<<"Extension">>, <<"100">>},
        % TODO: set appropriately
        {<<"Context">>, <<"ext-local">>},
        % TODO: add hint (SIP/351&Custom:DND351)
        {<<"Hint">>, <<"SIP/100">>},
        {<<"Status">>, ?STATUS_RINGING}
    ],
    
    amimulator_amqp:publish_amqp_event({publish, Payload}).