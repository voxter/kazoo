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

start_link() ->
    gen_listener:start_link({'local', ?MODULE}
    	                    ,?MODULE
                            ,[{'bindings', []}, {'responders', []}]
                            ,[]
	                       ).

init([]) ->
    lager:debug("AMI: Started originator for handling AMI dials ~p", [self()]),
    {'ok', #state{}}.

handle_call(_Request, _From, State) ->
    lager:debug("AMI: unhandled call"),
    {'reply', {'error', 'not_implemented'}, State}.

handle_cast({"originate", Props}, State) ->
	dial(update_props(Props)),
	{'noreply', State};
handle_cast({"blindxfer", Props}, State) ->
    control_queue_exec(update_props(Props), fun blind_transfer/2),
    {'noreply', State};
handle_cast({"atxfer", Props}, State) ->
    attended_transfer(update_props(Props)),
    {'noreply', State};
handle_cast({"vmxfer", Props}, State) ->
    control_queue_exec(update_props(Props), fun vm_transfer/2),
    {'noreply', State};
handle_cast({"pickupchan", Props}, State) ->
	pickup_channel(update_props(Props)),
	{'noreply', State};
handle_cast({"eavesdrop", Props}, State) ->
    eavesdrop_req(update_props(Props)),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueName}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(Msg, State) ->
    lager:debug("Unhandled cast ~p", [Msg]),
    {'noreply', State}.

dial(Props) ->
    Call = create_call_from_props(Props),

    DestExten = proplists:get_value(<<"Exten">>, Props),

    CCVs = [
    	{<<"Account-ID">>, proplists:get_value(<<"AccountId">>, Props)},
        {<<"Retain-CID">>, <<"true">>},
        {<<"Inherit-Codec">>, <<"false">>},
        {<<"Authorizing-Type">>, kapps_call:authorizing_type(Call)},
        {<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)},
        {<<"Web-Dial">>, <<"true">>},
        {<<"Flip-Direction-On-Bridge">>, <<"true">>}
    ],

    Request = [{<<"Application-Name">>, <<"transfer">>}
               ,{<<"Application-Data">>, kz_json:from_list([{<<"Route">>, DestExten}])}
               ,{<<"Msg-ID">>, kz_util:rand_hex_binary(16)}
               ,{<<"Endpoints">>, get_endpoints(Props, Call)}
               ,{<<"Timeout">>, <<"30">>}
               ,{<<"Ignore-Early-Media">>, <<"true">>}
               ,{<<"Media">>, <<"process">>}
               ,{<<"Caller-ID-Name">>, <<"Web Dial ", DestExten/binary>>}
               ,{<<"Caller-ID-Number">>, DestExten}
               ,{<<"Outbound-Caller-ID-Name">>, <<"Web Dial ", DestExten/binary>>}
               ,{<<"Outbound-Caller-ID-Number">>, DestExten}
               ,{<<"Outbound-Callee-ID-Name">>, <<"Outbound Call">>}
               ,{<<"Outbound-Callee-ID-Number">>, <<"context_2">>}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
               										,<<"Retain-CID">>
               										,<<"Authorizing-ID">>
               										,<<"Authorizing-Type">>
               										,<<"Web-Dial">>
               										,<<"Flip-Direction-On-Bridge">>
               									   ]}
               | kz_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],

    kapi_resource:publish_originate_req(props:filter_undefined(Request)).

%% Using the props computed from Originate action, establish a kapps_call
create_call_from_props(Props) ->
    Routines = [
        fun(C) -> kapps_call:set_account_db(proplists:get_value(<<"AccountDb">>, Props), C) end,
        fun(C) -> kapps_call:set_account_id(proplists:get_value(<<"AccountId">>, Props), C) end,
        fun(C) -> maybe_assign_aleg_props(Props, C) end
    ],
    lists:foldl(fun(F, C) -> F(C) end, kapps_call:new(), Routines).

%% If we can get the authorizing id from the originating channel, set some extra props
maybe_assign_aleg_props(Props, Call) ->
    case aleg_authorizing_id(Props) of
        {'error', E} ->
            lager:debug("AMI: origination could not find aleg authorizing id (~p)", [E]),
            Call;
        {'ok', AuthorizingId} ->
            assign_aleg_props(AuthorizingId, Props, Call)
    end.

%% _id from couch for destination endpoint
aleg_authorizing_id(Props) ->
    ViewOptions = [{'key', proplists:get_value(<<"SourceExten">>, Props)}],
    case kz_datamgr:get_results(proplists:get_value(<<"AccountDb">>, Props), <<"users/list_by_username">>, ViewOptions) of
        {'ok', []} -> {'error', 'endpoint_not_found'};
        {'ok', [Result]} -> {'ok', kz_json:get_value(<<"id">>, Result)}
    end.

%% Set extra props for originating channel
assign_aleg_props(AuthorizingId, Props, Call) ->
    To = proplists:get_value(<<"SourceExten">>, Props),
    Routines = [
        fun(C) -> kapps_call:set_authorizing_id(AuthorizingId, C) end,
        fun(C) -> kapps_call:set_authorizing_type(<<"user">>, C) end,
        fun(C) -> kapps_call:set_request(<<To/binary, "@blackholeami">>, C) end,
        fun(C) -> kapps_call:set_to(<<To/binary, "@blackholeami">>, C) end,
        fun(C) -> kapps_call:set_resource_type(<<"audio">>, C) end
    ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

%% Augment props received via AMI with additional data for call origination
update_props(Props) ->
    Routines = [
        fun(Props2) -> [{<<"AccountDb">>
        				,kz_util:format_account_id(proplists:get_value(<<"AccountId">>, Props), 'encoded')}
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
get_endpoints(_Props, Call) ->
    UserId = kapps_call:authorizing_id(Call),
    % Number = proplists:get_value(<<"SourceExten">>, Props),
    Properties = kz_json:from_list([
        {<<"can_call_self">>, 'true'}
    ]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case kz_endpoint:build(EndpointId, Properties, aleg_cid("000", Call)) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end, [], kz_attributes:owned_by(UserId, <<"device">>, Call)).

%% Caller ID properties for call coming from originate request
aleg_cid(CID, Call) ->
    Routines = [fun(C) -> kapps_call:set_custom_channel_var(<<"Retain-CID">>, <<"true">>, C) end
                ,fun(C) -> kapps_call:set_caller_id_name(kz_util:to_binary(CID), C) end
                ,fun(C) -> kapps_call:set_caller_id_number(kz_util:to_binary(CID), C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

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
	TargetCallId = <<"blind-transfer-", (kz_util:rand_hex_binary(4))/binary>>,

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
                 ,{<<"Msg-ID">>, kz_util:rand_hex_binary(4)}
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

attended_transfer(Props) ->
    Call = props:get_value(<<"Call">>, Props),

    CallId = amimulator_call:call_id(Call),
    DestExten = props:get_value(<<"Exten">>, Props),

    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"transfer">>}
           ,{<<"Data">>, kz_json:from_list(
                           [{<<"target">>, DestExten}
                           ])
            }
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    lager:debug("Attempting to transfer ~s to ~s", [CallId, DestExten]),
    kz_amqp_worker:cast(API, fun kapi_metaflow:publish_action/1).

vm_transfer(Props, Call) ->
    WhappsCall = amimulator_call:to_kapps_call(Call),
	%kapps_call_command:unbridge(WhappsCall2),
	kapps_call_command:hangup('true', WhappsCall),

	SourceExten = amimulator_call:id_number(Call),
    SourceCID = amimulator_call:id_name(Call),
    DestExten = props:get_value(<<"Exten">>, Props),
	CallId = amimulator_call:call_id(Call),
	TargetCallId = <<"vm-transfer-", (kz_util:rand_hex_binary(4))/binary>>,

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
                 ,{<<"Msg-ID">>, kz_util:rand_hex_binary(4)}
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

pickup_channel(Props) ->
    NewCall = create_call_from_props(Props),
    Call = ami_sm:call_by_channel(props:get_value(<<"Data">>, Props)),
    DestExten = amimulator_call:id_number(Call),

    CCVs = [{<<"Account-ID">>, proplists:get_value(<<"AccountId">>, Props)}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, amimulator_call:authorizing_type(Call)}
            ,{<<"Authorizing-ID">>, amimulator_call:authorizing_id(Call)}
           ],

    Request = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Existing-Call-ID">>, amimulator_call:other_leg_call_id(Call)}
               ,{<<"Msg-ID">>, kz_util:rand_hex_binary(16)}
               ,{<<"Endpoints">>, get_endpoints(Props, NewCall)}
               ,{<<"Timeout">>, <<"30">>}
               ,{<<"Ignore-Early-Media">>, <<"true">>}
               ,{<<"Media">>, <<"process">>}
               ,{<<"Outbound-Caller-ID-Name">>, <<"Web Pickup ", DestExten/binary>>}
               ,{<<"Outbound-Caller-ID-Number">>, DestExten}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                                   ,<<"Retain-CID">>
                                                   ,<<"Authorizing-ID">>
                                                   ,<<"Authorizing-Type">>
                                                   ]}
                | kz_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],

    kapi_resource:publish_originate_req(props:filter_undefined(Request)).

eavesdrop_req(Props) ->
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    Call = create_call_from_props(Props),

    Channel = hd(binary:split(props:get_value(<<"Data">>, Props), <<",">>)),
    DestExten = lists:nth(2, binary:split(Channel, <<"/">>)),
    EavesdropCallId = amimulator_call:call_id(ami_sm:call_by_channel(Channel)),
    EavesdropMode = case lists:nth(2, binary:split(props:get_value(<<"Data">>, Props), <<",">>)) of
    	<<"w">> ->
    		<<"whisper">>;
    	<<"bq">> ->
    		<<"listen">>;
    	Other ->
    		lager:debug("Unsupported eavesdrop mode ~p, defaulting to listen", [Other])
    end,

    CCVs = props:filter_undefined([{<<"Account-ID">>, AccountId}]),

    SourceEndpoints = get_endpoints(props:delete(<<"SourceExten">>, Props), Call),

    Prop = kz_json:set_values(props:filter_undefined([
	         {<<"Msg-ID">>, kz_util:rand_hex_binary(16)}
	         ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
	         ,{<<"Timeout">>, <<"30">>}
	         ,{<<"Endpoints">>, SourceEndpoints}
	         ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
	         ,{<<"Ignore-Early-Media">>, <<"true">>}
	         ,{<<"Outbound-Caller-ID-Name">>, <<"Eavesdrop ", DestExten/binary>>}
	         ,{<<"Outbound-Caller-ID-Number">>, DestExten}
	         ,{<<"Export-Custom-Channel-Vars">>, [
	            <<"Account-ID">>
	            ,<<"Retain-CID">>
	            ,<<"Authorizing-ID">>
	            ,<<"Authorizing-Type">>
	          ]}
	         ,{<<"Account-ID">>, AccountId}
	         ,{<<"Resource-Type">>, <<"originate">>}
	         ,{<<"Application-Name">>, <<"eavesdrop">>}
	         ,{<<"Eavesdrop-Call-ID">>, EavesdropCallId}
	         ,{<<"Eavesdrop-Group-ID">>, 'undefined'}
	         ,{<<"Eavesdrop-Mode">>, EavesdropMode}
                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
	    	]), kz_json:new()),

    lager:debug("Eavesdropping on call id ~p", [EavesdropCallId]),
    case kapps_util:amqp_pool_collect(Prop
                                       ,fun kapi_resource:publish_originate_req/1
                                       ,fun until_callback/1
                                       ,5000
                                      ) of
        {'ok', [OrigJObj|_]} ->
            lager:debug("Successful originate, executing eavesdrop"),
            send_originate_execute(OrigJObj, kz_json:get_value(<<"Server-ID">>, OrigJObj));
        {'error', E} ->
            lager:debug("error originating: ~p", [E]);
        {'timeout', _} ->
            lager:debug("error originating: timeout")
    end.

-spec until_callback(kz_json:objects()) -> boolean().
until_callback([JObj | _]) ->
    kapi_dialplan:originate_ready_v(JObj).

-spec send_originate_execute(kz_json:object(), ne_binary()) -> 'ok'.
send_originate_execute(JObj, Q) ->
    Prop = [{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_dialplan:publish_originate_execute(kz_json:get_value(<<"Server-ID">>, JObj), Prop).

handle_info(_Info, State) ->
    lager:debug("AMI: unhandled info"),
    {'noreply', State}.

handle_event(_JObj, _State) ->
    {'reply', []}.

terminate(Reason, _State) ->
    lager:debug("AMI: Originator on pid ~p terminating: ~p", [self(), Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
