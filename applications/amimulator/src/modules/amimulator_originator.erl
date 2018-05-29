-module(amimulator_originator).
-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("../amimulator.hrl").

-record(state, {}).
-type state() :: #state{}.

-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?MODULE}
                           ,?MODULE
                           ,[{'bindings', []}, {'responders', []}]
                           ,[]
                           ).

-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:debug("AMI: Started originator for handling AMI dials ~p", [self()]),
    {'ok', #state{}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    lager:debug("AMI: unhandled call"),
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({"blindxfer", Props}, State) ->
    Props1 = amimulator_util:update_originate_props(Props),
    control_queue_exec(Props1, fun blind_transfer/2),
    {'noreply', State};
handle_cast({"atxfer", Props}, State) ->
    Props1 = amimulator_util:update_originate_props(Props),
    control_queue_exec(Props1, fun attended_transfer/2),
    {'noreply', State};
handle_cast({"vmxfer", Props}, State) ->
    Props1 = amimulator_util:update_originate_props(Props),
    control_queue_exec(Props1, fun vm_transfer/2),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueName}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(Msg, State) ->
    lager:debug("Unhandled cast ~p", [Msg]),
    {'noreply', State}.

control_queue_exec(Props, Function) ->
    Call = props:get_value(<<"Call">>, Props),

    case amimulator_call:control_queue(Call) of
        'undefined' -> 'error';
        CtrlQ ->
            lager:debug("Got control queue ~p", [CtrlQ]),
            Function(Props, amimulator_call:set_control_queue(CtrlQ, Call))
    end.

blind_transfer(Props, Call) ->
    WhappsCall = amimulator_call:to_kapps_call(Call),
                                                % kapps_call_command:unbridge(WhappsCall2),
    kapps_call_command:hangup('true', WhappsCall),

    SourceExten = amimulator_call:id_number(Call),
    SourceCID = amimulator_call:id_name(Call),
    DestExten = props:get_value(<<"Exten">>, Props),
    CallId = amimulator_call:call_id(Call),
    TargetCallId = <<"blind-transfer-", (kz_binary:rand_hex(4))/binary>>,

    CCVs = props:filter_undefined([{<<"Account-ID">>, amimulator_call:account_id(Call)}
                                  ,{<<"Authorizing-ID">>, amimulator_call:authorizing_id(Call)}
                                  ,{<<"Channel-Authorized">>, 'true'}
                                   %% TODO add account realm to amimulator_call
                                  ,{<<"From-URI">>, <<SourceExten/binary, "@", (kapps_call:account_realm(WhappsCall))/binary>>}
                                  ,{<<"Ignore-Early-Media">>, 'true'}
                                                % ,{<<"Amimulator-Blind-Transfer">>, <<"true">>}
                                  ]),

    Endpoint = kz_json:from_list(
                 props:filter_undefined(
                   [{<<"Invite-Format">>, <<"loopback">>}
                   ,{<<"Route">>, DestExten}
                   ,{<<"To-DID">>, DestExten}
                   ,{<<"To-Realm">>, kapps_call:account_realm(WhappsCall)}
                   ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                   ,{<<"Outbound-Call-ID">>, TargetCallId}
                   ,{<<"Ignore-Early-Media">>, 'true'}
                   ,{<<"Existing-Call-ID">>, CallId}
                   ])),

    Request = props:filter_undefined(
                [{<<"Endpoints">>, [Endpoint]}
                ,{<<"Outbound-Call-ID">>, TargetCallId}
                ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                ,{<<"Msg-ID">>, kz_binary:rand_hex(4)}
                ,{<<"Continue-On-Fail">>, 'true'}
                ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
                                                    ,<<"Authorizing-Type">>, <<"Authorizing-ID">>
                                                    ,<<"Channel-Authorized">>
                                                    ]}
                ,{<<"Application-Name">>, <<"bridge">>}
                ,{<<"Timeout">>, 30}

                ,{<<"Outbound-Caller-ID-Name">>, SourceCID}
                ,{<<"Outbound-Caller-ID-Number">>, SourceExten}
                ,{<<"Caller-ID-Name">>, SourceCID}
                ,{<<"Caller-ID-Number">>, SourceExten}

                ,{<<"Existing-Call-ID">>, CallId}
                ,{<<"Resource-Type">>, <<"originate">>}
                ,{<<"Originate-Immediate">>, 'true'}
                ,{<<"Simplify-Loopback">>, 'true'}
                ,{<<"Ignore-Early-Media">>, 'true'}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),

    kapi_resource:publish_originate_req(Request).

attended_transfer(Props, Call) ->
    KappsCall = amimulator_call:to_kapps_call(Call),
    DestExten = props:get_value(<<"Exten">>, Props),
    kapps_call_command:attended_transfer(DestExten, KappsCall).

vm_transfer(Props, Call) ->
    WhappsCall = amimulator_call:to_kapps_call(Call),
                                                %kapps_call_command:unbridge(WhappsCall2),
    kapps_call_command:hangup('true', WhappsCall),

    SourceExten = amimulator_call:id_number(Call),
    SourceCID = amimulator_call:id_name(Call),
    DestExten = props:get_value(<<"Exten">>, Props),
    CallId = amimulator_call:call_id(Call),
    TargetCallId = <<"vm-transfer-", (kz_binary:rand_hex(4))/binary>>,

    CCVs = props:filter_undefined(
             [{<<"Account-ID">>, amimulator_call:account_id(Call)}
             ,{<<"Authorizing-ID">>, amimulator_call:authorizing_id(Call)}
             ,{<<"Channel-Authorized">>, 'true'}
             ,{<<"From-URI">>, <<SourceExten/binary, "@", (kapps_call:account_realm(WhappsCall))/binary>>}
             ,{<<"Ignore-Early-Media">>, 'true'}
             ]),

    Endpoint = kz_json:from_list(
                 props:filter_undefined(
                   [{<<"Invite-Format">>, <<"loopback">>}
                   ,{<<"Route">>, <<"**", DestExten/binary>>}
                   ,{<<"To-DID">>, <<"**", DestExten/binary>>}
                   ,{<<"To-Realm">>, kapps_call:account_realm(WhappsCall)}
                   ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                   ,{<<"Outbound-Call-ID">>, TargetCallId}
                   ,{<<"Ignore-Early-Media">>, 'true'}
                   ,{<<"Existing-Call-ID">>, CallId}
                   ])),

    Request = props:filter_undefined(
                [{<<"Endpoints">>, [Endpoint]}
                ,{<<"Outbound-Call-ID">>, TargetCallId}
                ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                ,{<<"Msg-ID">>, kz_binary:rand_hex(4)}
                ,{<<"Continue-On-Fail">>, 'true'}
                ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
                                                    ,<<"Authorizing-Type">>, <<"Authorizing-ID">>
                                                    ,<<"Channel-Authorized">>
                                                    ]}
                ,{<<"Application-Name">>, <<"bridge">>}
                ,{<<"Timeout">>, 30}

                ,{<<"Outbound-Caller-ID-Name">>, SourceCID}
                ,{<<"Outbound-Caller-ID-Number">>, SourceExten}
                ,{<<"Caller-ID-Name">>, SourceCID}
                ,{<<"Caller-ID-Number">>, SourceExten}

                ,{<<"Existing-Call-ID">>, CallId}
                ,{<<"Resource-Type">>, <<"originate">>}
                ,{<<"Originate-Immediate">>, 'true'}
                ,{<<"Simplify-Loopback">>, 'true'}
                ,{<<"Ignore-Early-Media">>, 'true'}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),

    kapi_resource:publish_originate_req(Request).

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("AMI: unhandled info"),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, _State) ->
    lager:debug("AMI: Originator on pid ~p terminating: ~p", [self(), Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
