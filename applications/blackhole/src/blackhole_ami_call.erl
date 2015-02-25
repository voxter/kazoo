-module(blackhole_ami_call).

-export([init_bindings/0, handle_event/1, handle_event/2]).

-include("blackhole.hrl").

-define(STATUS_AVAILABLE, <<"0">>).
-define(STATUS_RINGING, <<"6">>).
-define(STATUS_BUSY, <<"3">>).
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

init_bindings() ->
    gen_listener:cast(blackhole_ami_amqp, {add_call_binding, <<"c6795c40e9c4d3afa5eb7bcbbc30fcdc">>}),
    gen_listener:add_binding(
        wh_hooks_listener,
        ?CALL_BINDING(<<"DTMF">>)
    ),
    gen_listener:add_responder(
        wh_hooks_listener,
        {'blackhole_ami_call', 'handle_event'},
        [{<<"call_event">>, <<"*">>}]
    ),
    blackhole_bindings:bind(<<"call.CHANNEL_CREATE.*">>, ?MODULE, handle_event),
    %blackhole_bindings:bind(<<"call.CHANNEL_ANSWER.*">>, ?MODULE, handle_event),
    %blackhole_bindings:bind(<<"call.DTMF.*">>, ?MODULE, handle_event),
    %blackhole_bindings:bind(<<"call.CHANNEL_EXECUTE.*">>, ?MODULE, handle_event),
    blackhole_bindings:bind(<<"call.CHANNEL_DESTROY.*">>, ?MODULE, handle_event).

handle_event(EventJObj) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).
    
handle_event(EventJObj, _Props) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    case EventName of
        <<"DTMF">> ->
            handle_specific_event(EventName, EventJObj);
        _ ->
            ok
    end.
    
handle_specific_event(<<"CHANNEL_CREATE">>, EventJObj) ->
    new_channel(EventJObj),
    extension_status_ringing(EventJObj);
    
handle_specific_event(<<"CHANNEL_DESTROY">>, EventJObj) ->
    %gen_listener:cast(blackhole_ami_amqp, {out, io_lib:format("~p", [EventJObj])}),
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

    Payload = <<"Event: Hangup",
        "\nPrivilege: call,all",
        "\nChannel: ",
        %CallId/binary,
        "\nUniqueid: ",
        CallId/binary,
        "\nCallerIDNum: ",
        CallerIdNum/binary,
        "\nCallerIDName: ",
        CallerIdName/binary,
        %ConnectedLineNum: 351
        %ConnectedLineName: device
        "\nCause: ",
        Cause/binary,
        "\nCause-txt: ",
        CauseText/binary,
        "\n\n">>,
    
    gen_listener:cast(blackhole_ami_amqp, {out, Payload});
    
handle_specific_event(<<"DTMF">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Digit = wh_json:get_value(<<"DTMF-Digit">>, EventJObj),

    Payload = <<
        "Event: DTMF",
        "\nPrivilege: dtmf,all",
        "\nChannel: ",
        %CallId/binary,
        "\nUniqueid: ",
        CallId/binary,
        "\nDigit: ",
        Digit/binary,
        "\nDirection: ",
        %Received,
        "\nBegin: Yes",
        "\nEnd: No",
        "\n\n"
    >>,
    % Also need to do this with begin/end reversed
    
    gen_listener:cast(blackhole_ami_amqp, {out, Payload});
    
handle_specific_event(EventName, _EventJObj) ->
    lager:debug("AMI: unhandled call event ~p", [EventName]).

new_channel(EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    CallerId = wh_json:get_value(<<"Caller-ID-Number">>, EventJObj),

    Payload = <<
        "Event: Newchannel",
        "\nPrivilege: call,all",
        "\nChannel: ",
        %CallId/binary,
        "\nChannelState: 0",
        "\nChannelStateDesc: Down",
        "\nCallerIDNum: ",
        CallerId/binary,
        "\nCallerIDName: ",
        CallerId/binary,
        "\nAccountCode: ", % Always blank
        "\nExten: ",
        % internal?
        "\nContext: from-piston",
        "\nUniqueid: ",
        CallId/binary,
        "\n\n"
    >>,
    
    gen_listener:cast(blackhole_ami_amqp, {out, Payload}).
    
extension_status_ringing(_EventJObj) ->
    Payload = <<
        "Event: ExtensionStatus",
        "\nPrivilege: call,all",
        "\nExtension: ",
        %351,
        "\nContext: ",
        %ext-local
        "\nHint: ",
        %SIP/351&Custom:DND351,
        "\nStatus: ",
        ?STATUS_RINGING/binary,
        "\n\n"
    >>,
    
    gen_listener:cast(blackhole_ami_amqp, {out, Payload}).