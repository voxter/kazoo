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

-record(state, {
}).

start_link() ->
    gen_listener:start_link(?MODULE
                           ,[{'bindings', []}
                            ,{'responders', []}
                            ], []).

init([]) ->
    lager:debug("AMI: Started originator for handling AMI dials ~p", [self()]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    lager:debug("AMI: unhandled call"),
    {reply, {error, not_implemented}, State}.

handle_cast({"originate", Props}, State) ->
	dial(update_props(Props)),
	{noreply, State};
handle_cast({"blindxfer", Props}, State) ->
    blind_transfer(update_props(Props)),
    {noreply, State};
handle_cast({"vmxfer", Props}, State) ->
    vm_transfer(update_props(Props)),
    {noreply, State};
handle_cast({"atxfer", Props}, State) ->
    attended_transfer(update_props(Props)),
    {noreply, State};
handle_cast({"pickupchan", Props}, State) ->
	pickup_channel(update_props(Props)),
	{noreply, State};
handle_cast({"eavesdrop", Props}, State) ->
    eavesdrop_req(update_props(Props)),
    {noreply, State};
handle_cast({gen_listener, {created_queue, _QueueName}}, State) ->
    {noreply, State};
handle_cast({gen_listener, {is_consuming, _IsConsuming}}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:debug("AMI: unhandled cast"),
    {noreply, State}.

dial(Props) ->
    Call = create_call_from_props(Props),

    DestExten = proplists:get_value(<<"Exten">>, Props),

    CCVs = [{<<"Account-ID">>, proplists:get_value(<<"AccountId">>, Props)}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
            ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
            ,{<<"Web-Dial">>, <<"true">>}
           ],

    Request = [{<<"Application-Name">>, <<"transfer">>}
               ,{<<"Application-Data">>, wh_json:from_list([{<<"Route">>, DestExten}])}
               ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
               ,{<<"Endpoints">>, get_endpoints(Props, Call)}
               ,{<<"Timeout">>, <<"30">>}
               ,{<<"Ignore-Early-Media">>, <<"true">>}
               ,{<<"Media">>, <<"process">>}
               ,{<<"Outbound-Caller-ID-Name">>, <<"Web Dial ", DestExten/binary>>}
               ,{<<"Outbound-Caller-ID-Number">>, DestExten}
               ,{<<"Outbound-Callee-ID-Name">>, whapps_call:to(Call)}
               ,{<<"Outbound-Callee-ID-Number">>, whapps_call:to(Call)}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
               										,<<"Retain-CID">>
               										,<<"Authorizing-ID">>
               										,<<"Authorizing-Type">>
               										,<<"Web-Dial">>
               									   ]}
               | wh_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
              
    wapi_resource:publish_originate_req(props:filter_undefined(Request)).

%% Using the props computed from Originate action, establish a whapps_call
create_call_from_props(Props) ->
    Routines = [
        fun(C) -> whapps_call:set_account_db(proplists:get_value(<<"AccountDb">>, Props), C) end,
        fun(C) -> whapps_call:set_account_id(proplists:get_value(<<"AccountId">>, Props), C) end,
        fun(C) -> maybe_assign_aleg_props(Props, C) end
    ],
    lists:foldl(fun(F, C) -> F(C) end, whapps_call:new(), Routines).

%% If we can get the authorizing id from the originating channel, set some extra props
maybe_assign_aleg_props(Props, Call) ->
    case aleg_authorizing_id(Props) of
        {error, E} ->
            lager:debug("AMI: origination could not find aleg authorizing id (~p)", [E]),
            Call;
        {ok, AuthorizingId} ->
            assign_aleg_props(AuthorizingId, Props, Call)
    end.

%% _id from couch for destination endpoint
aleg_authorizing_id(Props) ->
    ViewOptions = [{key, proplists:get_value(<<"SourceExten">>, Props)}],
    case couch_mgr:get_results(proplists:get_value(<<"AccountDb">>, Props), <<"users/list_by_username">>, ViewOptions) of
        {ok, []} ->
            {error, endpoint_not_found};
        {ok, [Result]} ->
            {ok, wh_json:get_value(<<"id">>, Result)}
    end.

%% Set extra props for originating channel
assign_aleg_props(AuthorizingId, Props, Call) ->
    To = proplists:get_value(<<"SourceExten">>, Props),
    Routines = [
        fun(C) -> whapps_call:set_authorizing_id(AuthorizingId, C) end,
        fun(C) -> whapps_call:set_authorizing_type(<<"user">>, C) end,
        fun(C) -> whapps_call:set_request(<<To/binary, "@blackholeami">>, C) end,
        fun(C) -> whapps_call:set_to(<<To/binary, "@blackholeami">>, C) end
    ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

%% Augment props received via AMI with additional data for call origination
update_props(Props) ->
    Routines = [
        fun(Props2) -> [{<<"AccountDb">>
        				,wh_util:format_account_id(proplists:get_value(<<"AccountId">>, Props), 'encoded')}
        			   ] ++ Props2 end,
        fun(Props2) -> [{<<"SourceExten">>
        				,channel_to_exten(props:get_value(<<"Channel">>, Props))}
        			   ]
            ++ Props2 end
    ],
    lists:foldl(fun(F, Props2) -> F(Props2) end, Props, Routines).

channel_to_exten(Channel) ->
	binary:replace(hd(binary:split(Channel, <<"@">>)), <<"SIP/">>, <<"">>).

%% Find the endpoints associated with the user placing the originate request
get_endpoints(Props, Call) ->
    UserId = whapps_call:authorizing_id(Call),
    Number = proplists:get_value(<<"SourceExten">>, Props),
    Properties = wh_json:from_list([
        {<<"can_call_self">>, false}
    ]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Properties, aleg_cid(Number, Call)) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end, [], cf_attributes:owned_by(UserId, <<"device">>, Call)).

%% Caller ID properties for call coming from originate request
aleg_cid(Number, Call) ->
    Routines = [fun(C) -> whapps_call:set_custom_channel_var(<<"Retain-CID">>, <<"true">>, C) end
                ,fun(C) -> whapps_call:set_caller_id_name(wh_util:to_binary(Number), C) end
                ,fun(C) -> whapps_call:set_caller_id_number(wh_util:to_binary(Number), C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

blind_transfer(Props) ->
    Call = props:get_value(<<"Call">>, Props),
    WhappsCall = props:get_value(<<"call">>, Call),

    DestExten = props:get_value(<<"Exten">>, Props),

    case control_queue(WhappsCall) of
    	{'error', E} ->
    		lager:debug("Could not fetch control queue for call (~p)", [E]);
    	{'ok', JObj} ->
    		CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
    		lager:debug("got control queue ~p", [CtrlQ]),
    		WhappsCall2 = whapps_call:set_control_queue(CtrlQ, WhappsCall),
    		%whapps_call_command:unbridge(WhappsCall2),
    		whapps_call_command:hangup('true', WhappsCall2),

    		%% Now begin call origination
			TargetCallId = <<"blind-transfer-", (wh_util:rand_hex_binary(4))/binary>>,
			SourceExten = props:get_value(<<"aleg_exten">>, Call),
			CID = props:get_value(<<"aleg_cid">>, Call),
			CallId = whapps_call:call_id(WhappsCall2),

		    CCVs = props:filter_undefined(
		             [{<<"Account-ID">>, whapps_call:account_id(WhappsCall2)}
		              ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(WhappsCall2)}
		              ,{<<"Channel-Authorized">>, 'true'}
		              ,{<<"From-URI">>, <<SourceExten/binary, "@", (whapps_call:account_realm(WhappsCall2))/binary>>}
		              ,{<<"Ignore-Early-Media">>, 'true'}
		              ,{<<"Amimulator-Blind-Transfer">>, <<"true">>}
		             ]),

		    Endpoint = wh_json:from_list(
		                 props:filter_undefined(
		                   [{<<"Invite-Format">>, <<"loopback">>}
		                    ,{<<"Route">>,  DestExten}
		                    ,{<<"To-DID">>, DestExten}
		                    ,{<<"To-Realm">>, whapps_call:account_realm(WhappsCall2)}
		                    ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
		                    ,{<<"Outbound-Call-ID">>, TargetCallId}
		                    ,{<<"Ignore-Early-Media">>, 'true'}
		                    ,{<<"Existing-Call-ID">>, CallId}
		                   ])),

		    Request = props:filter_undefined(
		                [{<<"Endpoints">>, [Endpoint]}
		                 ,{<<"Outbound-Call-ID">>, TargetCallId}
		                 ,{<<"Dial-Endpoint-Method">>, <<"single">>}
		                 ,{<<"Msg-ID">>, wh_util:rand_hex_binary(4)}
		                 ,{<<"Continue-On-Fail">>, 'true'}
		                 ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
		                 ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
		                                                      ,<<"Authorizing-Type">>, <<"Authorizing-ID">>
		                                                      ,<<"Channel-Authorized">>
		                                                     ]}
		                 ,{<<"Application-Name">>, <<"bridge">>}
		                 ,{<<"Timeout">>, 30}

		                 ,{<<"Outbound-Caller-ID-Name">>, CID}
		                 ,{<<"Outbound-Caller-ID-Number">>, SourceExten}
		                 ,{<<"Caller-ID-Name">>, CID}
		                 ,{<<"Caller-ID-Number">>, SourceExten}

		                 ,{<<"Existing-Call-ID">>, CallId}
		                 ,{<<"Resource-Type">>, <<"originate">>}
		                 ,{<<"Originate-Immediate">>, 'true'}
		                 ,{<<"Simplify-Loopback">>, 'true'}
		                 ,{<<"Ignore-Early-Media">>, 'true'}
		                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
		                ]),

			lager:debug("request ~p", [Request]),
		    wapi_resource:publish_originate_req(Request)
    end.

control_queue(Call) ->
	Req = props:filter_undefined([{<<"Call-ID">>, whapps_call:call_id(Call)}
		   						  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
		  						 ]),
    ReqResp = whapps_util:amqp_pool_request(Req
                                            ,fun wapi_amimulator:publish_control_queue_req/1
                                            ,fun wapi_amimulator:control_queue_resp_v/1
                                           ),
    case ReqResp of
    	{'error', 'timeout'} ->
    		case whapps_call:other_leg_call_id(Call) of
    			'undefined' ->
    				{'error', 'not_found'};
    			OtherCallId ->
		    		ReqResp2 = whapps_util:amqp_pool_request(props:set_value(<<"Call-ID">>, OtherCallId, Req)
		    												 ,fun wapi_amimulator:publish_control_queue_req/1
		    												 ,fun wapi_amimulator:control_queue_resp_v/1
		    												),
		    		case ReqResp2 of
		    			{'error', _E}=Error -> Error;
		    			{'ok', Resp} ->
		    				{'ok', Resp}
		    		end
		    end;
        {'error', _E}=Error -> Error;
        {'ok', Resp} ->
        	{'ok', Resp}
    end.

attended_transfer(Props) ->
    Call = props:get_value(<<"Call">>, Props),
    WhappsCall = props:get_value(<<"call">>, Call),

    CallId = whapps_call:call_id(WhappsCall),
    DestExten = props:get_value(<<"Exten">>, Call),

    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"transfer">>}
           ,{<<"Data">>, wh_json:from_list(
                           [{<<"target">>, DestExten}
                           ])
            }
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    lager:debug("attempting to transfer ~s to ~s", [CallId, DestExten]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1).

vm_transfer(Props) ->
	Call = props:get_value(<<"Call">>, Props),
    WhappsCall = props:get_value(<<"call">>, Call),

    DestExten = props:get_value(<<"Exten">>, Props),

    case control_queue(WhappsCall) of
    	{'error', E} ->
    		lager:debug("Could not fetch control queue for call (~p)", [E]);
    	{'ok', JObj} ->
    		CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
    		lager:debug("got control queue ~p", [CtrlQ]),
    		WhappsCall2 = whapps_call:set_control_queue(CtrlQ, WhappsCall),
    		%whapps_call_command:unbridge(WhappsCall2),
    		whapps_call_command:hangup('true', WhappsCall2),

    		%% Now begin call origination
			TargetCallId = <<"blind-transfer-", (wh_util:rand_hex_binary(4))/binary>>,
			SourceExten = props:get_value(<<"aleg_exten">>, Call),
			CID = props:get_value(<<"aleg_cid">>, Call),
			CallId = whapps_call:call_id(WhappsCall2),

		    CCVs = props:filter_undefined(
		             [{<<"Account-ID">>, whapps_call:account_id(WhappsCall2)}
		              ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(WhappsCall2)}
		              ,{<<"Channel-Authorized">>, 'true'}
		              ,{<<"From-URI">>, <<SourceExten/binary, "@", (whapps_call:account_realm(WhappsCall2))/binary>>}
		              ,{<<"Ignore-Early-Media">>, 'true'}
		             ]),

		    Endpoint = wh_json:from_list(
		                 props:filter_undefined(
		                   [{<<"Invite-Format">>, <<"loopback">>}
		                    ,{<<"Route">>,  <<"**", DestExten/binary>>}
		                    ,{<<"To-DID">>, <<"**", DestExten/binary>>}
		                    ,{<<"To-Realm">>, whapps_call:account_realm(WhappsCall2)}
		                    ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
		                    ,{<<"Outbound-Call-ID">>, TargetCallId}
		                    ,{<<"Ignore-Early-Media">>, 'true'}
		                    ,{<<"Existing-Call-ID">>, CallId}
		                   ])),

		    Request = props:filter_undefined(
		                [{<<"Endpoints">>, [Endpoint]}
		                 ,{<<"Outbound-Call-ID">>, TargetCallId}
		                 ,{<<"Dial-Endpoint-Method">>, <<"single">>}
		                 ,{<<"Msg-ID">>, wh_util:rand_hex_binary(4)}
		                 ,{<<"Continue-On-Fail">>, 'true'}
		                 ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
		                 ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
		                                                      ,<<"Authorizing-Type">>, <<"Authorizing-ID">>
		                                                      ,<<"Channel-Authorized">>
		                                                     ]}
		                 ,{<<"Application-Name">>, <<"bridge">>}
		                 ,{<<"Timeout">>, 30}

		                 ,{<<"Outbound-Caller-ID-Name">>, CID}
		                 ,{<<"Outbound-Caller-ID-Number">>, SourceExten}
		                 ,{<<"Caller-ID-Name">>, CID}
		                 ,{<<"Caller-ID-Number">>, SourceExten}

		                 ,{<<"Existing-Call-ID">>, CallId}
		                 ,{<<"Resource-Type">>, <<"originate">>}
		                 ,{<<"Originate-Immediate">>, 'true'}
		                 ,{<<"Simplify-Loopback">>, 'true'}
		                 ,{<<"Ignore-Early-Media">>, 'true'}
		                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
		                ]),

			lager:debug("request ~p", [Request]),
		    wapi_resource:publish_originate_req(Request)
    end.

pickup_channel(Props) ->
	lager:debug("props ~p", [Props]),
	Call = props:get_value(<<"Call">>, Props),
    WhappsCall = props:get_value(<<"call">>, Call),

	%% Now begin call origination
	TargetCallId = <<"pickup-channel-", (wh_util:rand_hex_binary(4))/binary>>,
	SourceExten = props:get_value(<<"SourceExten">>, Props),
	%CID = props:get_value(<<"aleg_cid">>, Call),
	CID = <<"testing">>,
	CallId = whapps_call:call_id(WhappsCall),

    CCVs = props:filter_undefined(
             [{<<"Account-ID">>, whapps_call:account_id(WhappsCall)}
              ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(WhappsCall)}
              ,{<<"Channel-Authorized">>, 'true'}
              ,{<<"From-URI">>, <<SourceExten/binary, "@", (whapps_call:account_realm(WhappsCall))/binary>>}
              ,{<<"Ignore-Early-Media">>, 'true'}
             ]),

    Endpoint = wh_json:from_list(
                 props:filter_undefined(
                   [{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
                    ,{<<"Outbound-Call-ID">>, TargetCallId}
                    ,{<<"Ignore-Early-Media">>, 'true'}
                    ,{<<"Existing-Call-ID">>, CallId}
                   ])),

    Request = props:filter_undefined(
                [{<<"Endpoints">>, [Endpoint]}
                 ,{<<"Outbound-Call-ID">>, TargetCallId}
                 ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                 ,{<<"Msg-ID">>, wh_util:rand_hex_binary(4)}
                 ,{<<"Continue-On-Fail">>, 'true'}
                 ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
                 ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
                                                      ,<<"Authorizing-Type">>, <<"Authorizing-ID">>
                                                      ,<<"Channel-Authorized">>
                                                     ]}
                 ,{<<"Application-Name">>, <<"bridge">>}
                 ,{<<"Timeout">>, 30}

                 ,{<<"Outbound-Caller-ID-Name">>, CID}
                 ,{<<"Outbound-Caller-ID-Number">>, SourceExten}
                 ,{<<"Caller-ID-Name">>, CID}
                 ,{<<"Caller-ID-Number">>, SourceExten}

                 ,{<<"Existing-Call-ID">>, CallId}
                 ,{<<"Resource-Type">>, <<"originate">>}
                 ,{<<"Originate-Immediate">>, 'true'}
                 ,{<<"Simplify-Loopback">>, 'true'}
                 ,{<<"Ignore-Early-Media">>, 'true'}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),

	lager:debug("request ~p", [Request]),
    wapi_resource:publish_originate_req(Request).

eavesdrop_req(Props) ->
    Call = create_call_from_props(Props),

    SourceEndpoints = get_endpoints(Props, Call),

    AccountId = proplists:get_value(<<"AccountId">>, Props),
    CCVs = props:filter_undefined([{<<"Account-ID">>, AccountId}]),
    MsgId = case proplists:get_value(<<"ActionID">>, Props) of
        undefined -> wh_util:rand_hex_binary(16);
        ActionID -> ActionID
    end,

    Channel = <<(hd(binary:split(proplists:get_value(<<"Data">>, Props), <<",">>)))/binary, "-", (amimulator_util:channel_tail(whapps_call:call_id(Call)))/binary>>,
    lager:debug("channel ~p", [Channel]),

    EavesdropCallId = whapps_call:call_id(props:get_value(<<"call">>, ami_sm:call_by_channel(Channel))),
    lager:debug("eavesdropping on call id ~p", [EavesdropCallId]),

    Prop = wh_json:set_values(props:filter_undefined([
        {<<"Msg-ID">>, MsgId},
        {<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)},
        {<<"Timeout">>, <<"30">>},
        {<<"Endpoints">>, SourceEndpoints},
        {<<"Export-Custom-Channel-Vars">>, [
            <<"Account-ID">>,
            <<"Retain-CID">>,
            <<"Authorizing-ID">>,
            <<"Authorizing-Type">>
        ]},
        {<<"Account-ID">>, AccountId},
        {<<"Resource-Type">>, <<"originate">>},
        {<<"Application-Name">>, <<"eavesdrop">>},
        {<<"Eavesdrop-Call-ID">>, EavesdropCallId},
        {<<"Eavesdrop-Group-ID">>, undefined},
        {<<"Eavesdrop-Mode">>, <<"listen">>}
        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ]), wh_json:new()),
              
    lager:debug("sending eavesdrop request"),
    case whapps_util:amqp_pool_collect(Prop
                                       ,fun wapi_resource:publish_originate_req/1
                                       ,fun until_callback/1
                                       ,5000
                                      ) of
        {'ok', [OrigJObj|_]} ->
            lager:debug("originate is ready to execute"),
            send_originate_execute(OrigJObj, wh_json:get_value(<<"Server-ID">>, OrigJObj));
        {'error', E} ->
            lager:debug("error originating: ~p", [E]);
        {'timeout', _} ->
            lager:debug("error originating: timeout")
    end.

-spec until_callback(wh_json:objects()) -> boolean().
until_callback([JObj | _]) ->
    wapi_dialplan:originate_ready_v(JObj).

-spec send_originate_execute(wh_json:object(), ne_binary()) -> 'ok'.
send_originate_execute(JObj, Q) ->
    Prop = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_dialplan:publish_originate_execute(wh_json:get_value(<<"Server-ID">>, JObj), Prop).

handle_info(_Info, State) ->
    lager:debug("AMI: unhandled info"),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

terminate(Reason, _State) ->
    lager:debug("AMI: Originator on pid ~p terminating: ~p", [self(), Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
