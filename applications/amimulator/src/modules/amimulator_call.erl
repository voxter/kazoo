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
	%lager:debug("Channel create for channel ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)]),
	lager:debug("Channel create ~p", [EventJObj]),
	Call = new_channel(EventJObj),

	spawn(fun() ->
		new_channel_event(Call),
	    extension_status(Call),
	    maybe_dial_event(EventJObj, Call),
	    new_state(EventName, EventJObj, Call),
	    maybe_change_agent_status(EventName, Call)
	end);

handle_specific_event(<<"CHANNEL_ANSWER">>=EventName, EventJObj) ->
	CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
	lager:debug("Channel answer for channel ~p", [CallId]),
	% lager:debug("Event data: ~p", [EventJObj]),
	Call = ami_sm:call(CallId),
	answer(Call, CallId),

	spawn(fun() ->
    	busy_state(EventJObj, Call, CallId),
    	maybe_change_agent_status(EventName, Call)
	end);

handle_specific_event(<<"CHANNEL_BRIDGE">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    OtherCallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj),
	lager:debug("Channel bridge from ~p to ~p", [CallId, OtherCallId]),

    Call = ami_sm:call(CallId),
    WhappsCall = props:get_value(<<"call">>, Call),
    OtherCall = ami_sm:call(OtherCallId),

    UpdaterParams = [{OtherCallId, OtherCall}, {CallId, Call}],
    Updaters = [
        fun(Call2) -> props:set_value(<<"call">>,
            whapps_call:set_other_leg_call_id(OtherCallId, WhappsCall), Call2) end,
        fun(Call2) -> amimulator_util:bleg_ami_channel(Call2, undefined, UpdaterParams) end,
        fun(Call2) -> amimulator_util:bleg_cid(Call2, undefined, UpdaterParams) end,
        fun(Call2) ->
        	FlipDirection = wh_util:to_atom(whapps_call:custom_channel_var(<<"Flip-Direction-On-Bridge">>, <<"false">>, WhappsCall)) or
        					  (whapps_call:caller_id_name(WhappsCall) =:= <<"Device QuickCall">>),

       		if FlipDirection ->
       			case props:get_value(<<"direction">>, Call2) of
    				<<"inbound">> -> props:set_value(<<"direction">>, <<"outbound">>, Call2);
    				<<"outbound">> -> props:set_value(<<"direction">>, <<"inbound">>, Call2)
    			end;
    		true ->
    			Call2
    		end end
    ],
    Call2 = lists:foldl(fun(F, Call3) -> F(Call3) end, Call, Updaters),

    ami_sm:update_call(CallId, Call2),

    spawn(fun() ->
    	Channel1 = props:get_value(<<"aleg_ami_channel">>, Call2),
	    Channel2 = props:get_value(<<"bleg_ami_channel">>, Call2),

	    %% Don't publish bridge and dial if the channels are bridging to themselves, just causes problems
	    if Channel1 =:= Channel2 ->
	    	ok;
	    true ->
		    SourceCID = props:get_value(<<"aleg_cid">>, Call2),
			OtherCID = props:get_value(<<"bleg_cid">>, Call2),

			case props:get_value(<<"direction">>, Call2) of
				<<"inbound">> ->
					bridge_and_dial(Channel1, Channel2, CallId, OtherCallId, SourceCID, OtherCID);
				<<"outbound">> ->
					bridge_and_dial(Channel2, Channel1, OtherCallId, CallId, OtherCID, SourceCID)
			end
		end
    end);
    
handle_specific_event(<<"CHANNEL_DESTROY">>=EventName, EventJObj) ->
	lager:debug("Channel destroy for channel ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)]),
    destroy_channel(EventName, EventJObj);
    
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
	Call = amimulator_util:create_call(EventJObj),
	CallId = whapps_call:call_id(props:get_value(<<"call">>, Call)),
	ami_sm:new_call(CallId, Call),
	Call.

new_channel_event(Call) ->
    case props:get_value(<<"direction">>, Call) of
        <<"inbound">> ->
            new_inbound_channel(Call);
        <<"outbound">> ->
        	% lager:debug("outbound channel ~p", [EventJObj]),
            new_outbound_channel(Call)
    end.

new_inbound_channel(Call) ->
    CallId = whapps_call:call_id(props:get_value(<<"call">>, Call)),

    DestExten = props:get_value(<<"bleg_exten">>, Call),
    SourceExten = props:get_value(<<"aleg_exten">>, Call),

    SourceCID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
    	<<"Voicemail">>;
    true ->
    	props:get_value(<<"aleg_cid">>, Call)
    end,

    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    Payload = new_channel_payload(EndpointName, SourceCID, SourceCID, DestExten, CallId),
    ami_ev:publish_amqp_event({publish, Payload}).

new_outbound_channel(Call) ->
    CallId = whapps_call:call_id(props:get_value(<<"call">>, Call)),

    SourceCID = props:get_value(<<"aleg_cid">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    case EndpointName of
        undefined ->
        	lager:debug("Error: Endpoint name undefined"),
            lager:debug("Call ~p", [Call]);
        _ ->
            ok
    end,

    Payload = new_channel_payload(EndpointName, SourceCID, SourceCID, CallId),
    ami_ev:publish_amqp_event({publish, Payload}).

new_channel_payload(Channel, CallerIDNum, CallerIDName, Uniqueid) ->
	new_channel_payload(Channel, CallerIDNum, CallerIDName, <<"">>, Uniqueid).
new_channel_payload(Channel, CallerIDNum, CallerIDName, Exten, Uniqueid) ->
	[
		{<<"Event">>, <<"Newchannel">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, Channel},
        {<<"ChannelState">>, 0},
        {<<"ChannelStateDesc">>, <<"Down">>},
        {<<"CallerIDNum">>, CallerIDNum},
        {<<"CallerIDName">>, CallerIDName},
        {<<"AccountCode">>, <<"">>}, %% Always blank
        {<<"Exten">>, Exten},
        {<<"Context">>, <<"from-internal">>},
        {<<"Uniqueid">>, Uniqueid}
	].

extension_status(Call) ->
    SourceExten = props:get_value(<<"aleg_exten">>, Call),
    Status = case props:get_value(<<"direction">>, Call) of
    	<<"inbound">> -> 1;
    	<<"outbound">> -> 8
    end,

    Payload = [
    	{<<"Event">>, <<"ExtensionStatus">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Extension">>, SourceExten},
        {<<"Context">>, <<"ext-local">>},
        {<<"Hint">>, <<"SIP/", SourceExten/binary, ",CustomPresence:", SourceExten/binary>>},
        {<<"Status">>, Status}
    ],
    ami_ev:publish_amqp_event({publish, Payload}).

maybe_dial_event(EventJObj, Call) ->
	OtherCallId = whapps_call:other_leg_call_id(props:get_value(<<"call">>, Call)),
    case OtherCallId of
        undefined ->
            ok;
        _ ->
            dial_event(EventJObj, Call, OtherCallId)
    end.

dial_event(EventJObj, Call, OtherCallId) ->
	WhappsCall = props:get_value(<<"call">>, Call),
    CallId = whapps_call:call_id(WhappsCall),

    DestExten = props:get_value(<<"bleg_exten">>, Call),
    SourceExten = props:get_value(<<"aleg_exten">>, Call),

    CID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
    	<<"Voicemail">>;
    true ->
    	props:get_value(<<"aleg_cid">>, Call)
    end,

    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    OtherCall = ami_sm:call(OtherCallId),

    {OtherCID, OtherEndpointName} = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
        undefined ->
            % case OtherCallId of
            %     CallId ->
            %         {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
            %     undefined ->
            %         {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
            %     _ ->
            %         {props:get_value(<<"aleg_cid">>, OtherCall), props:get_value(<<"aleg_ami_channel">>, OtherCall)}
            % end;
            {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
        QueueId ->
            case amimulator_util:find_id_number(
                QueueId,
                whapps_call:account_db(WhappsCall)
            ) of
	            {error, E} ->
	            	lager:debug("Could not find queue extension ~p", [E]),
	            	case OtherCallId of
		                CallId ->
		                    {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
		                undefined ->
		                    {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
		                _ ->
		                    {props:get_value(<<"aleg_cid">>, OtherCall), props:get_value(<<"aleg_ami_channel">>, OtherCall)}
		            end;
	            {ok, Number} ->
	            	{<<"Queue ", Number/binary, " Call">>, <<>>}
            end
    end,

    %% We need to publish only if the exten matches originally dialed one
    OtherDialed = props:get_value(<<"bleg_exten">>, OtherCall),
    case OtherDialed of
        undefined ->
            ok;
        _ ->
        	case EndpointName of
        		OtherEndpointName ->
        			ok;
        		_ ->
		        	Payload = dial(OtherEndpointName, EndpointName, OtherCID, OtherCID, CID, CID, OtherCallId, CallId, CID),
		            ami_ev:publish_amqp_event({publish, Payload})
		    end
    end.

dial(Channel, Destination, CallerIDNum, CallerIDName, ConnectedLineNum, ConnectedLineName, Uniqueid, DestUniqueid, Dialstring) ->
	[
		{<<"Event">>, <<"Dial">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"SubEvent">>, <<"Begin">>},
        {<<"Channel">>, Channel},
        {<<"Destination">>, Destination},
        {<<"CallerIDNum">>, CallerIDNum},
        {<<"CallerIDName">>, CallerIDName},
        {<<"ConnectedLineNum">>, ConnectedLineNum},
        {<<"ConnectedLineName">>, ConnectedLineName},
        {<<"UniqueID">>, Uniqueid},
        {<<"DestUniqueid">>, DestUniqueid},
        {<<"Dialstring">>, Dialstring}
	].

bridge_and_dial(SourceChannel, DestChannel, SourceCallId, DestCallId, SourceCID, DestCID) ->
	Payload = [[
		{<<"Event">>, <<"Link">>},
        {<<"Channel1">>, SourceChannel},
        {<<"Channel2">>, DestChannel},
        {<<"Uniqueid1">>, SourceCallId},
        {<<"Uniqueid2">>, DestCallId}
	], [
        {<<"Event">>, <<"Dial">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"SubEvent">>, <<"Begin">>},
        {<<"Channel">>, SourceChannel},
        {<<"Destination">>, DestChannel},
        {<<"CallerIDNum">>, SourceCID},
        {<<"CallerIDName">>, SourceCID},
        {<<"ConnectedLineNum">>, DestCID},
        {<<"ConnectedLineName">>, DestCID},
        {<<"UniqueID">>, SourceCallId},
        {<<"DestUniqueid">>, DestCallId},
        {<<"Dialstring">>, DestCID}
	]],
    ami_ev:publish_amqp_event({publish, Payload}).

new_state(<<"CHANNEL_CREATE">>, EventJObj, Call) ->
	case wh_json:get_value(<<"Callee-ID-Number">>, EventJObj) of
		%% This special case occurs when outbound calls are placed (i.e. cellphones)
		%% it breaks the ring/ringing state because there are 2 legs to the outbound device with opposite call directions
		%% thus I ignore these legs
		<<"context_2">> ->
			ok;
		_ ->
		    case props:get_value(<<"direction">>, Call) of
		        <<"inbound">> ->
		            ring_state(Call);
		        <<"outbound">> ->
		        	% maybe_pre_dial_event(EventJObj),
		            ringing_state(EventJObj, Call)
		    end
	end.

ring_state(Call) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    CallId = whapps_call:call_id(WhappsCall),

    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),
    DestExten = props:get_value(<<"bleg_exten">>, Call),
    SourceExten = props:get_value(<<"aleg_exten">>, Call),

    OtherCID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
    	<<"Voicemail">>;
    true ->
    	props:get_value(<<"bleg_cid">>, Call)
    end,

    % OtherCID = case DestExten of
    %     SourceExten ->
    %         <<"Voicemail">>;
    %     _ ->
    %         maybe_internal_cid(WhappsCall,
    %             hd(binary:split(wh_json:get_value(<<"To">>, EventJObj), <<"@">>)))
    % end,

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

ringing_state(EventJObj, Call) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    CallId = whapps_call:call_id(WhappsCall),

    SourceCID = props:get_value(<<"aleg_cid">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    OtherCallId = whapps_call:other_leg_call_id(WhappsCall),
    OtherCall = ami_sm:call(OtherCallId),

    OtherCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
        undefined ->
            % case OtherCallId of
            %     CallId ->
            %         props:get_value(<<"bleg_cid">>, Call);
            %     undefined ->
            %         props:get_value(<<"bleg_cid">>, Call);
            %     _ ->
            %         props:get_value(<<"aleg_cid">>, OtherCall)
            % end;
            props:get_value(<<"bleg_cid">>, Call);
        QueueId ->
            case amimulator_util:find_id_number(
                QueueId,
                whapps_call:account_db(WhappsCall)
            ) of
	            {error, E} ->
	            	lager:debug("Could not find queue extension ~p", [E]),
	            	props:get_value(<<"bleg_cid">>, Call);
	            {ok, Number} ->
	            	<<"Queue ", Number/binary, " Call">>
            end
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
                {<<"ConnectedLineNum">>, OtherCID},
                {<<"ConnectedLineName">>, OtherCID},
                {<<"Uniqueid">>, CallId}
            ],
            ami_ev:publish_amqp_event({publish, Payload});
        false ->
            ok
    end.

answer(undefined, CallId) ->
	lager:debug("Answer for unregistered call with id ~p", [CallId]),
	ami_sm:flag_early_answer(CallId);

answer(Call, CallId) ->
	EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),
	ami_sm:answer(EndpointName, CallId).

busy_state(EventJObj, Call, CallId) ->
	WhappsCall = props:get_value(<<"call">>, Call),
    SourceCID = props:get_value(<<"aleg_cid">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    OtherCallId = whapps_call:other_leg_call_id(WhappsCall),
    OtherCall = ami_sm:call(OtherCallId),

    OtherCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
        undefined ->
            % case OtherCallId of
            %     CallId ->
            %         props:get_value(<<"bleg_cid">>, Call);
            %     undefined ->
            %         props:get_value(<<"bleg_cid">>, Call);
            %     _ ->
            %         props:get_value(<<"aleg_cid">>, OtherCall)
            % end;
            props:get_value(<<"bleg_cid">>, Call);
        QueueId ->
            {ok, Number} = amimulator_util:find_id_number(
                QueueId,
                whapps_call:account_db(WhappsCall)
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
        {<<"ConnectedLineNum">>, OtherCID},
        {<<"ConnectedLineName">>, OtherCID},
        {<<"Uniqueid">>, CallId}
    ],
    ami_ev:publish_amqp_event({publish, Payload}).

destroy_channel(EventName, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),

    SourceCID = props:get_value(<<"aleg_cid">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    DestCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
        undefined ->
            props:get_value(<<"bleg_cid">>, Call);
        QueueId ->
            AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
            case amimulator_util:find_id_number(
                QueueId,
                wh_util:format_account_id(AccountId, encoded)
            ) of
	            {error, E} ->
	            	lager:debug("Could not find queue extension ~p", [E]),
	            	props:get_value(<<"bleg_cid">>, Call);
	            {ok, Number} ->
	            	<<"Queue ", Number/binary, " Call">>
            end
    end,

    {Cause, CauseText} = case wh_json:get_value(<<"Hangup-Cause">>, EventJObj) of
        <<"NORMAL_CLEARING">> ->
            {<<"16">>, <<"Normal Clearing">>};
        _ ->
            {<<"0">>, <<"Not Defined">>}
    end,

    case {ami_sm:call_id_in_channel(CallId, EndpointName), ami_sm:answered_or_ignored(EndpointName, CallId)} of
        {true, true} ->
        	lager:debug("Sending hangup for reason ~p", [wh_json:get_value(<<"Hangup-Cause">>, EventJObj)]),
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

            maybe_change_agent_status(EventName, Call),

            ami_ev:publish_amqp_event({publish, Payload});
        {false, _} ->
        	ok;
        	%lager:debug("eventjobj ~p", [EventJObj]),
        	%lager:debug("Call not destroyed because it was not for the channel (~p)", [EndpointName]);
        {_, false} ->
        	ok
        	%lager:debug("eventjobj ~p", [EventJObj]),
            %lager:debug("Call not destroyed because it was not the correct answered channel")
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

maybe_change_agent_status(EventName, Call) ->
	WhappsCall = props:get_value(<<"call">>, Call),
    Status = case EventName of
        <<"CHANNEL_CREATE">> ->
            6;
        <<"CHANNEL_ANSWER">> ->
            2;
        <<"CHANNEL_DESTROY">> ->
            1
    end,

    %% TODO this does not work for cell phone of agent (on destroy there is no authorizing id)
    case whapps_call:authorizing_id(WhappsCall) of
        undefined ->
            ok;
        AuthorizingId ->
            AccountDb = whapps_call:account_db(WhappsCall),

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
                                    change_agent_status(AccountDb, UserDoc, Queues, Status)
                            end
                    end
            end
    end.

change_agent_status(AccountDb, UserDoc, Queues, Status) ->
    Username = wh_json:get_value(<<"username">>, UserDoc),
    FirstName = wh_json:get_value(<<"first_name">>, UserDoc),
    LastName = wh_json:get_value(<<"last_name">>, UserDoc),

    Payload = lists:foldl(fun(QueueId, Acc) ->
        case couch_mgr:get_results(AccountDb, <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
            {error, _E} ->
                Acc;
            {ok, []} ->
            	Acc;
            {ok, Results} ->
            	Value = wh_json:get_value(<<"value">>, hd(Results)),
        		Number = hd(Value),
            	%% TODO add the stats in here
                [[
                    {<<"Event">>, <<"QueueMemberStatus">>},
                    {<<"Queue">>, Number},
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






