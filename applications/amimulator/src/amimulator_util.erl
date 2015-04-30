-module(amimulator_util).

-include("amimulator.hrl").

-export([parse_payload/1, format_prop/1, format_binary/1, format_json_events/1,
	index_of/2, initial_calls/1, create_call/1,
    get_call/1, bleg_ami_channel/3, endpoint_exten/2, whapps_call/1,
    maybe_get_exten/1, maybe_get_endpoint_name/1, endpoint_name/2, maybe_get_cid_name/1,
    find_id_number/2, queue_for_number/2,
    filter_registered_events/4, whapps_call_from_cf_exe/1, channel_tail/1]).

-export([basic_calls/1]).

%% AMI commands broken up by newlines
parse_payload(Payload) ->
    Lines = filter_empty(binary:split(Payload, <<"\r\n">>, [global])),
    lists:foldl(fun(Parameter, Acc) ->
        KV = binary:split(Parameter, <<":">>),
        {K, V} = {lists:nth(1, KV), lists:nth(2, KV)},
        Prop = {K, binary:replace(V, <<" ">>, <<>>)},
        [Prop] ++ Acc
        end, [], Lines).
    
%% Eliminates trailing lines from client payload
filter_empty(Parameters) ->
    lists:foldl(fun(Param, Acc) ->
        case Param of
            <<>> ->
                Acc;
            _ ->
                [Param] ++ Acc
        end end, [], Parameters).

%% Recursive proplist formatting for writes to socket
format_prop({V}) ->
    <<(wh_util:to_binary(V))/binary, "\r\n">>;
format_prop({K, V}) ->
    <<(wh_util:to_binary(K))/binary, ": ", (wh_util:to_binary(V))/binary, "\r\n">>.

format_binary([KV|Rest]) ->
    Head = format_prop(KV),
    Tail = format_binary(Rest),
    <<Head/binary, Tail/binary>>;
format_binary([]) ->
    <<"\r\n">>.

%% Format a set of events for publishing to AMQP
format_json_events(Events) ->
    format_json_events(Events, []).

format_json_events([], Acc) ->
    Acc;
format_json_events([{_K, _V}|_Other]=KVs, _Acc) ->
    [{KVs}];
format_json_events([Event|Events], Acc) ->
    format_json_events(Events, Acc ++ [{Event}]).




index_of(Element, List) ->
	index_of(Element, List, 1).

index_of(_, [], _) ->
	not_found;
index_of(Element, [Element|_], Index) ->
	Index;
index_of(Element, [_|T], Index) ->
	index_of(Element, T, Index+1).








create_call(EventJObj) ->
    {CallId, WhappsCall} = call_from_json(EventJObj),

    BCs = case whapps_call:other_leg_call_id(WhappsCall) of
        CallId ->
            lager:debug("Why are the legs the same ID"),
            [{CallId, WhappsCall}];
        undefined ->
            [{CallId, WhappsCall}];
        OtherLegCallId ->
            [{OtherLegCallId, ami_sm:call(OtherLegCallId)}, {CallId, WhappsCall}]
    end,

    better_call(EventJObj, BCs).

get_call(CallId) ->
    ami_sm:call(CallId).

basic_calls(AccountId) ->
	Req = [
        {<<"Account-ID">>, AccountId},
        {<<"Active-Only">>, 'true'}
        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ],
    case wh_amqp_worker:call_collect(
        Req,
        fun wapi_call:publish_query_account_channels_req/1,
        {'ecallmgr', fun wapi_call:query_account_channels_resp_v/1}
    ) of
        {'ok', RespJObjs} ->
        	%% Now we can produce all the channels and update the state master
            LookupChannels = lists:foldl(fun(RespJObj, ChannelsAcc) ->
                Channels = wh_json:get_value(<<"Channels">>, RespJObj),
                case Channels of
                    undefined ->
                    	ChannelsAcc;
                    _ ->
                    	NewChannels = lists:foldl(fun(Channel, Acc) ->
                    		CallId = wh_json:get_value(<<"uuid">>, Channel),
                    		[{CallId, Channel} | Acc]
                    	end, ChannelsAcc, Channels),
                    	NewChannels
                end
            end, [], RespJObjs),

            lists:foldl(fun({_CallId, Channel}, Acc) ->
            	[call_from_channel(Channel, LookupChannels) | Acc]
            end, [], LookupChannels);
        E ->
            lager:debug("Could not get channel statuses: ~p", [E])
    end.


call_from_channel(JObj, Lookup) ->
	Routines = [
		fun({Call, WhappsCall}) ->
			{Call, whapps_call:set_call_id(wh_json:get_value(<<"uuid">>, JObj), WhappsCall)} end,
		fun({Call, WhappsCall}) ->
			{Call, whapps_call:set_other_leg_call_id(wh_json:get_value(<<"other_leg">>, JObj), WhappsCall)} end,
		fun({Call, WhappsCall}) ->
			{Call, whapps_call:set_bridge_id(wh_json:get_value(<<"bridge_id">>, JObj), WhappsCall)} end,
		fun({Call, WhappsCall}) ->
			{Call, whapps_call:set_account_id(wh_json:get_value(<<"account_id">>, JObj), WhappsCall)} end,
		fun({Call, WhappsCall}) ->
			{Call, whapps_call:set_authorizing_id(wh_json:get_value(<<"authorizing_id">>, JObj, <<>>), WhappsCall)} end,
		fun({Call, WhappsCall}) ->
			{Call, whapps_call:set_authorizing_type(wh_json:get_value(<<"authorizing_type">>, JObj, <<>>), WhappsCall)} end,
		fun({Call, WhappsCall}) ->
			{Call, whapps_call:set_to_user(wh_json:get_value(<<"destination">>, JObj), WhappsCall)} end,

		fun({Call, WhappsCall}) ->
			{props:set_value(<<"direction">>, wh_json:get_value(<<"direction">>, JObj), Call), WhappsCall} end,
		fun({Call, WhappsCall}) ->
			CallId = wh_json:get_value(<<"uuid">>, JObj),
			Props = case wh_json:get_value(<<"bridge_id">>, JObj) of
				CallId ->
					AccountDb = whapps_call:account_db(WhappsCall),
					case wh_json:get_value(<<"authorizing_id">>, JObj) of
						undefined ->
							ALeg = case maybe_cellphone_endpoint2(
								whapps_call:to_user(WhappsCall), props:get_value(<<"direction">>, Call),
								CallId, wh_json:get_value(<<"presence_id">>, JObj), AccountDb) of
								{direction, D} ->
									[
										{<<"aleg_cid">>, props:get_value(<<"cid">>, D)},
										{<<"aleg_exten">>, props:get_value(<<"cid">>, D)},
										{<<"aleg_ami_channel">>, props:get_value(<<"channel">>, D)}
									];
								{endpoint, Endpoint} ->
									[
										{<<"aleg_cid">>, endpoint_cid(Endpoint, AccountDb)},
										{<<"aleg_exten">>, endpoint_exten(Endpoint, AccountDb)},
										{<<"aleg_ami_channel">>, endpoint_channel(Endpoint, AccountDb, CallId)}
									]
							end,
							{ok, Endpoint2} = couch_mgr:open_doc(AccountDb,
								wh_json:get_value(<<"authorizing_id">>,
									props:get_value(whapps_call:other_leg_call_id(WhappsCall), Lookup))),
							BLeg = [
								{<<"bleg_cid">>, endpoint_cid(Endpoint2, AccountDb)},
								{<<"bleg_exten">>, endpoint_exten(Endpoint2, AccountDb)},
								{<<"bleg_ami_channel">>, endpoint_channel(Endpoint2, AccountDb, CallId)}
							],
							ALeg ++ BLeg;
						_ ->
							{ok, Endpoint} = cf_endpoint:get(WhappsCall),
							ALeg = [
								{<<"aleg_cid">>, endpoint_cid(Endpoint, AccountDb)},
								{<<"aleg_exten">>, endpoint_exten(Endpoint, AccountDb)},
								{<<"aleg_ami_channel">>, endpoint_channel(Endpoint, AccountDb, CallId)}
							],
							OtherLegCallId = whapps_call:other_leg_call_id(WhappsCall),
							OtherChannel = props:get_value(OtherLegCallId, Lookup),
							BLeg = case maybe_cellphone_endpoint2(
								wh_json:get_value(<<"destination">>, OtherChannel), wh_json:get_value(<<"direction">>, OtherChannel),
								CallId, wh_json:get_value(<<"presence_id">>, OtherChannel), AccountDb) of
								{direction, D} ->
									[
										{<<"bleg_cid">>, props:get_value(<<"cid">>, D)},
										{<<"bleg_exten">>, props:get_value(<<"cid">>, D)},
										{<<"bleg_ami_channel">>, props:get_value(<<"channel">>, D)}
									];
								{endpoint, Endpoint} ->
									[
										{<<"bleg_cid">>, endpoint_cid(Endpoint, AccountDb)},
										{<<"bleg_exten">>, endpoint_exten(Endpoint, AccountDb)},
										{<<"bleg_ami_channel">>, endpoint_channel(Endpoint, AccountDb, OtherLegCallId)}
									]
							end,
							ALeg ++ BLeg
					end;
				_ ->
					[]
			end,
			{props:set_values(Props, Call), WhappsCall}
		end
	],
    {Call, WhappsCall} = lists:foldl(fun(F, {Call, WhappsCall}) -> F({Call, WhappsCall}) end, {[], whapps_call:new()}, Routines).%,

    %% Remove other leg if it's just the same leg
    % CallId = whapps_call:call_id(Call),
    % Call2 = case whapps_call:other_leg_call_id(Call) of
    %     CallId ->
    %         whapps_call:set_other_leg_call_id(undefined, Call);
    %     _ ->
    %         Call
    % end,

    %{whapps_call:call_id(WhappsCall), WhappsCall}.

maybe_cellphone_endpoint2(To, Direction, CallId, PresenceId, AccountDb) ->
    {ok, Results} = couch_mgr:get_results(AccountDb, <<"devices/call_forwards">>),

    Number = case Direction of
    	<<"inbound">> ->
    		hd(binary:split(PresenceId, <<"@">>));
    	<<"outbound">> ->
    		To
    end,

    E164 = wnm_util:to_e164(Number),
    case find_call_forward(E164, AccountDb, Results) of
    	false ->
    		{direction, call_direction_endpoint2(To, Direction, CallId, PresenceId)};
    	Endpoint ->
    		{endpoint, Endpoint}
    end.





find_call_forward(_, _, []) ->
	false;
find_call_forward(E164, AccountDb, [Result|Others]) ->
	case wnm_util:to_e164(wh_json:get_value(<<"key">>, Result)) of
		E164 ->
			Value = wh_json:get_value(<<"value">>, Result),
			{ok, Device} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, Value));
		_ ->
			find_call_forward(E164, AccountDb, Others)
	end.




call_direction_endpoint2(To, Direction, CallId, PresenceId) ->
    Props = case Direction of
        <<"inbound">> ->
            [
            	{<<"channel">>, channel_string(hd(binary:split(PresenceId, <<"@">>)), CallId)},
	            {<<"cid">>, hd(binary:split(PresenceId, <<"@">>))}
            ];
        <<"outbound">> ->
            [
            	{<<"channel">>, channel_string(To, CallId)},
            	{<<"cid">>, To}
            ]
    end.%,
 %    case props:get_value(<<"cid">>, Props) of
 %    	<<"Unknown">> ->
 %    		call_direction_cid(Call, WhappsCall, Props);
 %    	<<"Device QuickCall">> ->
 %    		call_direction_cid(Call, WhappsCall, Props);
	% 	_ ->
	% 		Props
	% end.




















%% Fetches all whapps calls for an account and adds them to the data store
initial_calls(AccountId) ->
    Req = [
        {<<"Account-ID">>, AccountId},
        {<<"Active-Only">>, true}
        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ],
    case wh_amqp_worker:call_collect(
        Req,
        fun wapi_call:publish_query_account_channels_req/1,
        {ecallmgr, fun wapi_call:query_account_channels_resp_v/1}
    ) of
        {ok, RespJObjs} ->




            %% Now we can produce all the channels and update the state master
            BasicCalls = lists:foldl(fun(RespJObj, Calls) ->
                Channels = wh_json:get_value(<<"Channels">>, RespJObj),
                case Channels of
                    undefined -> Calls;
                    _ -> calls_from_json(Channels) ++ Calls
                end
            end, [], RespJObjs),

            %% Finally, make these calls not suck
            lists:foldl(fun(RespJObj, Acc) ->
                Channels = wh_json:get_value(<<"Channels">>, RespJObj),
                case Channels of
                    undefined -> Acc;
                    _ -> better_calls(Channels, BasicCalls) ++ Acc
                end
            end, [], RespJObjs);
        E ->
            lager:debug("Could not get channel statuses: ~p", [E])
    end.

calls_from_json(JObjs) ->
    lists:foldl(fun(JObj, Calls) ->
        [call_from_json(JObj) | Calls]
    end, [], JObjs).

call_from_json(JObj) ->
    Routines = [
        fun(Call) -> whapps_call:set_authorizing_id(
            wh_json:get_first_defined([<<"authorizing_id">>, [<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>]], JObj, <<>>), Call) end,
        fun(Call) -> whapps_call:set_account_id(
            wh_json:get_first_defined([<<"account_id">>, [<<"Custom-Channel-Vars">>, <<"Account-ID">>]], JObj), Call) end,
        fun(Call) ->
        	case wh_json:get_value(<<"Custom-Channel-Vars">>, JObj) of
        		undefined ->
        			Call;
        		CCVs ->
		        	whapps_call:set_custom_channel_vars(wh_json:to_proplist(CCVs), Call)
		    end end,
        fun(Call) -> whapps_call:set_account_db(
            wh_util:format_account_id(whapps_call:account_id(Call), encoded), Call) end,
        fun(Call) -> whapps_call:set_call_id(
            wh_json:get_first_defined([<<"uuid">>, <<"Call-ID">>], JObj), Call) end,
        fun(Call) -> whapps_call:set_other_leg_call_id(
            wh_json:get_first_defined([<<"other_leg">>, <<"Other-Leg-Call-ID">>], JObj), Call) end,
        fun(Call) ->
        	ToString = wh_json:get_first_defined([<<"destination">>, <<"To">>], JObj, <<"@">>),
        	To = case binary:split(ToString, <<"@">>) of
                [_ToUser, _ToRealm] ->
                    ToString;
                [ToUser] ->
                    <<ToUser/binary, "@">>
            end,
            whapps_call:set_to(To, Call) end,
        fun(Call) ->
            FromString = wh_json:get_first_defined([<<"From">>], JObj, <<"@">>),
            From = case binary:split(FromString, <<"@">>) of
                [_FromUser, _FromRealm] ->
                    FromString;
                [FromUser] ->
                    <<FromUser/binary, "@">>
            end,
            whapps_call:set_from(From, Call) end
    ],
    Call = lists:foldl(fun(F, Call) -> F(Call) end, whapps_call:new(), Routines),

    %% Remove other leg if it's just the same leg
    CallId = whapps_call:call_id(Call),
    Call2 = case whapps_call:other_leg_call_id(Call) of
        CallId ->
            whapps_call:set_other_leg_call_id(undefined, Call);
        _ ->
            Call
    end,

    {whapps_call:call_id(Call2), Call2}.

%% Adds a whole bunch of extra data to make whapps_calls more useful
better_calls(JObjs, BasicCalls) ->
    lists:foldl(fun(JObj, Calls) ->
            [better_call(JObj, BasicCalls)] ++ Calls
    end, [], JObjs).

better_call(JObj, BasicCalls) ->
    CallId = wh_json:get_first_defined([<<"uuid">>, <<"Call-ID">>], JObj),
    Call = props:get_value(CallId, BasicCalls),

    Routines = [
        fun(Call2, JObj2, _BC) -> 
            CallDirection = wh_json:get_first_defined([<<"direction">>, <<"Call-Direction">>], JObj2,
                <<"inbound">>),
            props:set_value(<<"direction">>, CallDirection, Call2) end,
        fun aleg_cid/3,
        fun aleg_exten/3,
        fun aleg_ami_channel/3,
        fun bleg_cid/3,
        fun bleg_exten/3,
        fun bleg_ami_channel/3,
        fun(Call2, JObj2, _BC) -> props:set_value(<<"username">>, 
            wh_json:get_first_defined([<<"username">>, <<"Username">>], JObj2), Call2) end,
        fun(Call2, JObj2, _BC) -> props:set_value(<<"answered">>, 
            wh_json:get_first_defined([<<"answered">>], JObj2), Call2) end,
        fun(Call2, JObj2, _BC) -> props:set_value(<<"elapsed_s">>, 
            wh_json:get_first_defined([<<"elapsed_s">>], JObj2), Call2) end
    ],
    lists:foldl(
        fun(F, BCall) -> F(BCall, JObj, BasicCalls) end,
        [{<<"call">>, Call}],
        Routines
    ).

aleg_cid(Call, _ChannelJObj, _BC) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    case WhappsCall of
        undefined ->
            try throw(42) catch 42 -> wh_util:log_stacktrace() end;
        _ ->
            ok
    end,
    case cf_endpoint:get(WhappsCall) of
        %% An external endpoint
        {error, _E} ->
        	props:set_value(<<"aleg_cid">>, props:get_value(<<"cid">>, maybe_cellphone_endpoint(Call)), Call);
        %% Some internal extension
        {ok, Endpoint} ->
            props:set_value(<<"aleg_cid">>, endpoint_cid(Endpoint, whapps_call:account_db(WhappsCall)), Call)
    end.

aleg_exten(Call, _ChannelJObj, _BC) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    case cf_endpoint:get(WhappsCall) of
        %% An external endpoint
        {error, _E} ->
            case props:get_value(<<"direction">>, Call) of
                <<"inbound">> ->
                    props:set_value(<<"aleg_exten">>, whapps_call:from_user(WhappsCall), Call);
                <<"outbound">> ->
                    props:set_value(<<"aleg_exten">>, whapps_call:to_user(WhappsCall), Call)
            end;
        %% Some internal extension
        {ok, Endpoint} ->
            props:set_value(<<"aleg_exten">>, endpoint_exten(Endpoint, whapps_call:account_db(WhappsCall)), Call)
    end.

aleg_ami_channel(Call, _ChannelJObj, _BC) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    case cf_endpoint:get(WhappsCall) of
        %% An external endpoint
        {error, _E} ->
            props:set_value(<<"aleg_ami_channel">>, props:get_value(<<"channel">>, maybe_cellphone_endpoint(Call)), Call);
        %% Some internal extension
        {ok, Endpoint} ->
            props:set_value(<<"aleg_ami_channel">>, endpoint_channel(Endpoint, whapps_call:account_db(WhappsCall), whapps_call:call_id(WhappsCall)), Call)
    end.

maybe_cellphone_endpoint(Call) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    AccountDb = wh_util:format_account_id(whapps_call:account_id(WhappsCall), encoded),
    {ok, Results} = couch_mgr:get_results(AccountDb, <<"devices/call_forwards">>),
    E164 = wnm_util:to_e164(whapps_call:to_user(WhappsCall)),
    case lists:foldl(fun(Result, Found) ->
        case Found of
            false ->
                {ok, Device} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, Result)),
                case {wnm_util:to_e164(wh_json:get_value([<<"call_forward">>, <<"number">>], Device)),
                    wh_json:get_value(<<"owner_id">>, Device)} of
                    {_, undefined} ->
                        false;
                    {E164, _} ->
                        [{<<"channel">>, endpoint_channel(Device, AccountDb, whapps_call:call_id(WhappsCall))}
                         ,{<<"cid">>, endpoint_cid(Device, AccountDb)}
                        ];
                    _ ->
                        false
                end;
            _ ->
                Found
        end
    end, false, Results) of
        false ->
            %if length(Results) > 0 ->
            %    {ok, Device} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, hd(Results))),
            %    props:set_value(<<"aleg_ami_channel">>,
            %        endpoint_channel(Device, AccountDb, whapps_call:call_id(WhappsCall)), Call);
            %true ->
                call_direction_endpoint(Call);
            %end;
        CellphoneEndpoint ->
            CellphoneEndpoint
    end.

call_direction_endpoint(Call) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    Props = case props:get_value(<<"direction">>, Call) of
        <<"inbound">> ->
            [{<<"channel">>, channel_string(
                whapps_call:from_user(WhappsCall),
                whapps_call:call_id(WhappsCall)
             )}
             ,{<<"cid">>, whapps_call:caller_id_name(WhappsCall)}
            ];
        <<"outbound">> ->
            [{<<"channel">>, channel_string(
                whapps_call:to_user(WhappsCall),
                whapps_call:call_id(WhappsCall)
             )}
             ,{<<"cid">>, whapps_call:caller_id_name(WhappsCall)}
            ]
    end,
    case props:get_value(<<"cid">>, Props) of
    	<<"Unknown">> ->
    		call_direction_cid(Call, WhappsCall, Props);
    	<<"Device QuickCall">> ->
    		call_direction_cid(Call, WhappsCall, Props);
		_ ->
			Props
	end.

call_direction_cid(Call, WhappsCall, Props) ->
	case props:get_value(<<"direction">>, Call) of
	    <<"inbound">> ->
	        props:set_value(<<"cid">>, whapps_call:from_user(WhappsCall), Props);
	    <<"outbound">> ->
	        props:set_value(<<"cid">>, whapps_call:to_user(WhappsCall), Props)
	end.

bleg_cid(Call, ChannelJObj, BC) ->
    case props:get_value(whapps_call:other_leg_call_id(props:get_value(<<"call">>, Call)),
        BC) of
        undefined ->
            props:set_value(<<"bleg_cid">>, wh_json:get_value(<<"Caller-ID-Name">>, ChannelJObj, undefined), Call);
        OtherCall ->
            Direction = case props:get_value(<<"direction">>, Call) of
                <<"inbound">> -> <<"outbound">>;
                <<"outbound">> -> <<"inbound">>
            end,

            case is_tuple(OtherCall) of
                true ->
                    props:set_value(<<"bleg_cid">>, props:get_value(<<"aleg_cid">>,
                        aleg_cid([{<<"call">>, OtherCall},
                        {<<"direction">>, Direction}], undefined, undefined)), Call);
                _ ->
                    props:set_value(<<"bleg_cid">>, props:get_value(<<"aleg_cid">>, OtherCall), Call)
            end
    end.

bleg_exten(Call, _ChannelJObj, BC) ->
    case props:get_value(whapps_call:other_leg_call_id(props:get_value(<<"call">>, Call)),
        BC) of
        undefined ->
            props:set_value(<<"bleg_exten">>, whapps_call:to_user(props:get_value(<<"call">>, Call)), Call);
        OtherCall ->
            Direction = case props:get_value(<<"direction">>, Call) of
                <<"inbound">> -> <<"outbound">>;
                <<"outbound">> -> <<"inbound">>
            end,

            case is_tuple(OtherCall) of
                true ->
                    props:set_value(<<"bleg_exten">>, props:get_value(<<"aleg_exten">>,
                    aleg_exten([{<<"call">>, OtherCall},
                    {<<"direction">>, Direction}], undefined, undefined)), Call);
                _ ->
                    props:set_value(<<"bleg_exten">>, props:get_value(<<"aleg_exten">>, OtherCall), Call)
            end
    end.

bleg_ami_channel(Call, _ChannelJObj, BC) ->
    case props:get_value(whapps_call:other_leg_call_id(props:get_value(<<"call">>, Call)),
        BC) of
        undefined ->
            %% TODO, find the call somehow
            Call;
        OtherCall ->
            Direction = case props:get_value(<<"direction">>, Call) of
                <<"inbound">> -> <<"outbound">>;
                <<"outbound">> -> <<"inbound">>
            end,

            case is_tuple(OtherCall) of
                true ->
                    props:set_value(<<"bleg_ami_channel">>, props:get_value(<<"aleg_ami_channel">>,
                        aleg_ami_channel([{<<"call">>, OtherCall},
                        {<<"direction">>, Direction}], undefined, undefined)), Call);
                _ ->
                    props:set_value(<<"bleg_ami_channel">>, props:get_value(<<"aleg_ami_channel">>, OtherCall), Call)
            end
    end.

endpoint_cid(Endpoint, AccountDb) ->
    case wh_json:get_value(<<"owner_id">>, Endpoint) of
        undefined ->
            wh_json:get_value(<<"name">>, Endpoint);
        OwnerId ->
            {ok, Owner} = couch_mgr:open_doc(AccountDb, OwnerId),
            <<(wh_json:get_value(<<"username">>, Owner))/binary, " ",
                (wh_json:get_value(<<"first_name">>, Owner))/binary, " ",
                (wh_json:get_value(<<"last_name">>, Owner))/binary>>
    end.

endpoint_channel(Endpoint, AccountDb, CallId) ->
    channel_string(endpoint_exten(Endpoint, AccountDb), CallId).

channel_string(Exten, CallId) ->
    <<"SIP/", Exten/binary, "-", (channel_tail(CallId))/binary>>.

endpoint_exten(Endpoint, AccountDb) ->
    case wh_json:get_value(<<"owner_id">>, Endpoint) of
        undefined ->
            wh_json:get_value(<<"name">>, Endpoint);
        OwnerId ->
            case couch_mgr:open_doc(AccountDb, OwnerId) of
                {ok, Owner} ->
                    <<(wh_json:get_value(<<"username">>, Owner))/binary>>;
                _ ->
                    wh_json:get_value(<<"name">>, Endpoint)
            end
    end.





whapps_call(CallRef) ->
    Call = case wh_json:is_json_object(CallRef) of
        true ->
            whapps_call_from_json(CallRef);
        false ->
            whapps_call_from_ecallmgr(CallRef)
    end,
    Updaters = [
        fun(Call2) -> case whapps_call:account_id(Call2) of
            undefined ->
                Call2;
            AccountId ->
                whapps_call:set_account_db(wh_util:format_account_id(AccountId, encoded), Call2)
        end end
    ],
    lists:foldl(fun(F, Call2) -> F(Call2) end, Call, Updaters).

whapps_call_from_json(EventJObj) ->
    Call = whapps_call:from_json(EventJObj),
    CCVs = whapps_call:ccvs(Call),
    Call2 = case wh_json:get_value(<<"Authorizing-ID">>, CCVs) of
        undefined ->
            Call;
        AuthId ->
            whapps_call:set_authorizing_id(AuthId, Call)
    end,
    case wh_json:get_value(<<"Account-ID">>, CCVs) of
        undefined ->
            Call2;
        AccountId ->
            whapps_call:set_account_id(AccountId, Call2)
    end.

whapps_call_from_ecallmgr(CallId) ->
    case ecallmgr_rpc(ecallmgr_fs_channel, fetch, [CallId, record]) of
        {error, E} ->
            lager:debug("Error ~p when fetching channel from ecallmgr", [E]),
            error;
        {ok, Channel} ->
            whapps_call:from_json(wh_json:from_list(ecallmgr_fs_channel:to_api_props(Channel)))
    end.

ecallmgr_rpc(Mod, Fun, Params) ->
    {ok, Shortname} = inet:gethostbyname(element(2, inet:gethostname())),
    case element(2, Shortname) of
        undefined ->
            {error, undefined_hostname};
        Hostname ->
            RemoteNode = wh_util:to_atom("ecallmgr@" ++ Hostname),
            Result = rpc:call(RemoteNode, Mod, Fun, Params),
            % TODO: disconnect from all new nodes
            erlang:disconnect_node(RemoteNode),
            Result
    end.

maybe_get_exten(Call) ->
    case cf_endpoint:get(Call) of
        %% An external endpoint
        {error, _E} ->
            whapps_call:callee_id_name(Call);
        %% Some internal extension
        {ok, Endpoint} ->
            endpoint_name(whapps_call:account_db(Call), Endpoint)
    end.

maybe_get_endpoint_name(Call) ->
    Exten = maybe_get_exten(Call),
    <<"SIP/", Exten/binary, "-", (channel_tail(whapps_call:call_id(Call)))/binary>>.

endpoint_name(AcctDb, Endpoint) ->
    case wh_json:get_value(<<"pvt_type">>, Endpoint) of
        <<"device">> ->
            {ok, EndpointDevice} = couch_mgr:open_doc(AcctDb, wh_json:get_value(<<"_id">>, Endpoint)),
            wh_json:get_value(<<"name">>, EndpointDevice);
        _ ->
            wh_json:get_value(<<"name">>, Endpoint)
    end.

maybe_get_cid_name(Call) ->
    case cf_endpoint:get(Call) of
        %% An external endpoint
        {error, _E} ->
            whapps_call:callee_id_name(Call);
        %% Some internal extension
        {ok, Endpoint} ->
            cid_name(whapps_call:account_db(Call), Endpoint)
    end.

cid_name(AcctDb, Endpoint) ->
    case wh_json:get_value(<<"owner_id">>, Endpoint) of
        undefined ->
            wh_json:get_value(<<"name">>, Endpoint);
        OwnerId ->
            {ok, Owner} = couch_mgr:open_doc(AcctDb, OwnerId),
            <<(wh_json:get_value(<<"username">>, Owner))/binary, " ",
                (wh_json:get_value(<<"first_name">>, Owner))/binary, " ",
                (wh_json:get_value(<<"last_name">>, Owner))/binary>>
    end.

find_id_number(Id, AccountDb) ->
    {ok, Results} = couch_mgr:get_results(AccountDb, <<"callflows/crossbar_listing">>),
    maybe_id_in_callflows(Id, Results, AccountDb).

maybe_id_in_callflows(_, [], _) ->
    {error, not_found};
maybe_id_in_callflows(Id, [Result|Results], AccountDb) ->
    CFId = wh_json:get_value(<<"id">>, Result),
    case maybe_id_in_callflow(Id, CFId, AccountDb) of
        false ->
            maybe_id_in_callflows(Id, Results, AccountDb);
        Number ->
            {ok, Number}
    end.

maybe_id_in_callflow(Id, CFId, AccountDb) ->
    {ok, CFDoc} = couch_mgr:open_doc(AccountDb, CFId),
    case maybe_id_in_callflow(Id, wh_json:get_value(<<"flow">>, CFDoc)) of
        false ->
            false;
        true ->
            hd(wh_json:get_value(<<"numbers">>, CFDoc))
    end.
    
maybe_id_in_callflow(Id, Flow) ->
    Data = wh_json:get_value(<<"data">>, Flow),
    %% Skipping queue login and queue logout possibilities
    case {wh_json:get_value(<<"id">>, Data), wh_json:get_value(<<"module">>, Flow)} of
        {_, <<"acdc_queue">>} ->
            recurse_to_child_callflow(Id, Flow);
        {Id, _} ->
            true;
        _ ->
            recurse_to_child_callflow(Id, Flow)
    end.

recurse_to_child_callflow(Id, Flow) ->
    Children = wh_json:get_value(<<"children">>, Flow),
    case wh_json:get_value(<<"_">>, Children) of
        undefined ->
            false;
        SubFlow ->
            maybe_id_in_callflow(Id, SubFlow)
    end.

queue_for_number(Number, AccountDb) ->
    case couch_mgr:get_results(AccountDb, <<"callflow/listing_by_number">>, [{key, Number}]) of
        {ok, []} ->
            {error, number_not_found};
        {ok, [Result]} ->
            {ok, CFDoc} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, Result)),
            case maybe_queue_in_flow(wh_json:get_value(<<"flow">>, CFDoc)) of
                {error, E} ->
                    {error, E};
                {ok, QueueId} ->
                    couch_mgr:open_doc(AccountDb, QueueId)
            end
    end.
    
maybe_queue_in_flow(Flow) ->
    case wh_json:get_value(<<"module">>, Flow) of
        <<"acdc_member">> ->
            {ok, wh_json:get_value(<<"id">>, wh_json:get_value(<<"data">>, Flow))};
        _ ->
            case wh_json:get_value(<<"_">>, wh_json:get_value(<<"flow">>, Flow)) of
                undefined ->
                    {error, invalid_queue_extension};
                SubFlow ->
                    maybe_queue_in_flow(SubFlow)
            end
    end.

%% Ensures the message belongs to the current listener process
%% and publishes it to the Mod:handle_specific_event/2 function
%% in the Mod supplied
filter_registered_events(EventName, EventJObj, CommPid, Mod) ->
    AccountId = gen_server:call(CommPid, account_id),
    case wh_json:get_value(
        [<<"Custom-Channel-Vars">>, <<"Account-ID">>],
        EventJObj
    ) of
        %% Maybe sometimes, the events come in via different formats
        undefined ->
            lager:debug("May need to add additional account id check for event! ~p", [EventJObj]);
        %% Event is destined to be published!
        AccountId ->
            Mod:handle_specific_event(EventName, EventJObj);
        %% Event does not belong to this listener
        _ ->
            ok
    end.

%% Look through active calls and find the cf_exe process with the desired call ID
whapps_call_from_cf_exe(CallId) ->
    whapps_call_from_cf_exe(CallId, cf_exe_sup:workers()).

whapps_call_from_cf_exe(_CallId, []) ->
    not_found;
whapps_call_from_cf_exe(CallId, [Worker|Workers]) ->
    case cf_exe:get_call(Worker) of
        {ok, Call} ->
            Call;
        _ ->
            whapps_call_from_cf_exe(CallId, Workers)
    end.

%% Returns an 8-digit tail for channels for AMI calls
channel_tail(CallId) ->
    Digest = crypto:hash('md5', wh_util:to_binary(CallId)),
    MD5 = lists:flatten([io_lib:format("~2.16.0b", [Part]) || <<Part>> <= Digest]),
    list_to_binary(lists:sublist(MD5, length(MD5)-7, 8)).









