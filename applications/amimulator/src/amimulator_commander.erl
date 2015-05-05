-module(amimulator_commander).

-export([handle/2]).
-export([queue_stats/2]).

-include("amimulator.hrl").

-define(AMI_DB, <<"ami">>).
    
%% Handle a payload sent as an AMI command
handle(Payload, AccountId) ->
    Props = amimulator_util:parse_payload(Payload),
    handle_event(update_props(Props, AccountId)).

update_props(Props, AccountId) ->
    Routines = [
        fun(Props2) -> [{<<"AccountId">>, AccountId}] ++ Props2 end,
        fun(Props2) -> case AccountId of
            <<>> ->
                Props2;
            _ ->
                [{<<"AccountDb">>, wh_util:format_account_id(AccountId, encoded)}] ++ Props2
        end end
    ],
    lists:foldl(fun(F, Props2) -> F(Props2) end, Props, Routines).
    
handle_event(Props) ->
    %lager:debug("AMI event params: ~p", [Parameters]),
    Action = string:to_lower(wh_util:to_list(proplists:get_value(<<"Action">>, Props))),
    handle_event(Action, Props).
    
% TODO: add AMI username lookup db initialization
% TODO: validate md5 key on login
% TODO: validate secret mode login (secret in TCP payload)
handle_event("login", Props) ->
    Username = proplists:get_value(<<"Username">>, Props),
    %Secret = proplists:get_value(<<"Secret">>, Props),
    Secret = undefined,

    %% Maybe the action id was not supplied (queue stats)
    ActionIDStr = case proplists:get_value(<<"ActionID">>, Props) of
        undefined ->
            [];
        ActionID ->
            [{<<"ActionID">>, ActionID}]
    end,

    {ok, AMIDoc} = couch_mgr:open_doc(?AMI_DB, Username),
    %case wh_json:get_value(<<"Secret">>, AMIDoc) of
    case Secret of
        undefined ->
            %% Successful login
            lager:debug("successful login, starting event listener"),
            
            %% Record account id that is being used for logged in account
            AccountId = wh_json:get_value(<<"account_id">>, AMIDoc),
            ami_sm:set_account_id(AccountId),
            gen_server:cast(self(), {login, AccountId}),

            %% Maybe launch the account's AMQP consumer
            ami_ev:maybe_start(AccountId),
            
            Payload = [[
                {<<"Response">>, <<"Success">>}
            ] ++ ActionIDStr ++ [
                {<<"Message">>, <<"Authentication accepted">>}
            ]],
            {ok, {Payload, broken}};
        _ ->
            %% Failed login
            Payload = [
                {<<"Response">>, <<"Error">>}
            ] ++ ActionIDStr ++ [
                {<<"Message">>, <<"Authentication failed">>}
            ],
            {ok, {Payload, n}}
    end;
handle_event("logoff", _Props) ->
    gen_server:cast(self(), {logout}),
    {logoff, ok};
handle_event("challenge", Props) ->
    Challenge = random:uniform(899999999) + 100000000,
    ActionID = proplists:get_value(<<"ActionID">>, Props),
    Payload = [
        {<<"Asterisk Call Manager/1.1">>},
        {<<"Response">>, <<"Success">>},
        {<<"Challenge">>, Challenge},
        {<<"ActionID">>, ActionID}
    ],
    {ok, {Payload, n}};
handle_event("ping", _Props) ->
   {Megasecs, Secs, Microsecs} = os:timestamp(),
   Timestamp = Megasecs * 1000000 + Secs + Microsecs / 1000000,
   Payload = [
       {<<"Response">>, <<"Success">>},
       {<<"Ping">>, <<"Pong">>},
       {<<"Timestamp">>, Timestamp}
   ],
   {ok, {Payload, n}};
% Handle AMI Status action
handle_event("status", Props) ->
    Calls = ami_sm:calls(props:get_value(<<"AccountId">>, Props)),
    Payload = initial_channel_status(Calls, Props, <<"Status">>),
    {ok, {Payload, n}};

handle_event("queuestatus", Props) ->
	Header = [[
		{<<"Response">>, <<"Success">>},
        {<<"Message">>, <<"Queue status will follow">>}
	]],
	Footer = [[
		{<<"Event">>, <<"QueueStatusComplete">>}
	]],
	{'ok', {Header ++ queues_status(Props) ++ Footer, 'n'}};

handle_event("sippeers", Props) ->
	SipPeers = sip_peers(Props),

    Payload = [[
        {<<"Response">>, <<"Success">>},
        {<<"EventList">>, <<"start">>},
        {<<"Message">>, <<"Peer status list will follow">>}
    ]] ++ SipPeers ++ [[
        {<<"Event">>, <<"PeerlistComplete">>},
        {<<"EventList">>, <<"Complete">>},
        {<<"ListItems">>, length(SipPeers)}
    ]],
    {ok, {Payload, n}};
handle_event("mailboxcount", Props) ->
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),
    Mailbox = proplists:get_value(<<"Mailbox">>, Props),
    ActionId = proplists:get_value(<<"ActionID">>, Props),
    Exten = hd(binary:split(Mailbox, <<"@">>)),

    %% NEED TO get_results UPDATE THIS VIEW BAZINGA
    Payload = case couch_mgr:get_results(AccountDb, <<"vmboxes/crossbar_listing">>, [{key, Exten}]) of
        {ok, [Result]} ->
            Value = wh_json:get_value(<<"value">>, Result),
            [
                {<<"Response">>, <<"Success">>},
                {<<"ActionID">>, ActionId},
                {<<"Message">>, <<"Mailbox Message Count">>},
                {<<"Mailbox">>, Mailbox},
                {<<"UrgMessages">>, 0},
                {<<"NewMessages">>, wh_json:get_value(<<"new_messages">>, Value)},
                {<<"OldMessages">>, wh_json:get_value(<<"old_messages">>, Value)}
            ];
        _ ->
            mailbox_count_error(ActionId, Mailbox)
    end,
    {ok, {Payload, n}};
handle_event("queuepause", Props) ->
    queue_pause(Props);
handle_event("queueadd", Props) ->
    queue_add(Props);
handle_event("queueremove", Props) ->
    queue_remove(Props);
handle_event("originate", Props) ->
    case proplists:get_value(<<"Channel">>, Props) of
        undefined ->
            {error, channel_not_specified};
        _ ->
            case proplists:get_value(<<"Application">>, Props) of
                <<"ChanSpy">> ->
                    gen_server:cast(self(), {originator, "eavesdrop", Props});
               	<<"PickupChan">> ->
               		Props2 = props:set_value(<<"Channel">>, props:get_value(<<"Data">>, 
               			props:set_value(<<"SourceExten">>, hd(binary:split(props:get_value(<<"Channel">>, Props), <<"/">>)),
               			Props)), Props),
               		gen_server:cast(self(), {originator, "pickupchan", Props2});
                _ ->
                    gen_server:cast(self(), {originator, "originate", Props})
            end
    end;
handle_event("redirect", Props) ->
    EndpointName = props:get_value(<<"Channel">>, Props),

    Call = ami_sm:call_by_channel(EndpointName),
    case Call of
        undefined ->
            lager:debug("Missing call when trying to transfer"),
            ok;
        _ ->
            case props:get_value(<<"Context">>, Props) of
                <<"default">> ->
                    gen_server:cast(self(), {originator, "vmxfer", props:set_value(<<"Call">>, Call, Props)});
                _ ->
                    gen_server:cast(self(), {originator, "blindxfer", props:set_value(<<"Call">>, Call, Props)})
            end
    end;
handle_event("atxfer", Props) ->
    EndpointName = props:get_value(<<"Channel">>, Props),

    Call = ami_sm:call_by_channel(EndpointName),
    case Call of
        undefined ->
            lager:debug("Missing call when trying to transfer attended"),
            ok;
        _ ->
            gen_server:cast(self(), {originator, "atxfer", props:set_value(<<"Call">>, Call, Props)})
    end;
handle_event("hangup", Props) ->
    EndpointName = props:get_value(<<"Channel">>, Props),
    CallId = ami_sm:call_by_channel(EndpointName),
    Call = amimulator_util:whapps_call_from_cf_exe(CallId),
    whapps_call_command:hangup(Call);
handle_event("getvar", Props) ->
    case getvar(proplists:get_value(<<"Variable">>, Props), Props) of
        undefined ->
            {error, undefined};
        {Payload, Mode} ->
            {ok, {Payload, Mode}};
        Payload ->
            {ok, {Payload, n}}
    end;
handle_event("command", Props) ->
    case command(proplists:get_value(<<"Command">>, Props), Props) of
        undefined ->
            {error, undefined};
        {Payload, Mode} ->
            {ok, {Payload, Mode}};
        Payload ->
            {ok, {Payload, n}}
    end;
handle_event("userevent", Props) ->
    case user_event(proplists:get_value(<<"UserEvent">>, Props), Props) of
        undefined ->
            {error, undefined};
        Payload ->
            {ok, {Payload, n}}
    end;
handle_event("events", Props) ->
    Events = case props:get_value(<<"EventMask">>, Props) of
        <<"off">> ->
            <<"Off">>;
        EventMask ->
            EventMask
    end,
    Payload = [
        {<<"Response">>, <<"Success">>},
        {<<"Events">>, Events}
    ],
    {ok, {Payload, n}};
handle_event(Event, Props) ->
    lager:debug("no handler defined for event ~p, props ~p", [Event, Props]),
    {error, no_action}.

initial_channel_status(Calls, Props, Format) ->
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    FilteredCalls = lists:filter(fun(Call) ->  
        case whapps_call:account_id(props:get_value(<<"call">>, Call)) of
            AccountId -> true;
            _ -> false
        end
    end, Calls),
    

    case Format of
        <<"Status">> ->
            FormattedCalls = lists:foldl(fun(Call, List) ->
                WhappsCall = props:get_value(<<"call">>, Call),

                case {whapps_call:other_leg_call_id(WhappsCall) /= undefined, props:get_value(<<"username">>, Call)} of
                    %{true, undefined} -> [ami_channel_status(Call, bridged, Format)] ++ List;
                    {_, undefined} -> List;
                    {true, _} -> [ami_channel_status(Call, bridged_extension, Format)] ++ List;
                    {false, _} -> [ami_channel_status(Call, bridged_extension, Format)] ++ List;
                    {_, _} -> lager:debug("undefined channel status format for call ~p", [Call])
                end
            end, [], FilteredCalls),

            [[
                {<<"Response">>, <<"Success">>},
                {<<"Message">>, <<"Channel status will follow">>}
            ]] ++ FormattedCalls ++ [[
                {<<"Event">>, <<"StatusComplete">>},
                {<<"Items">>, length(FormattedCalls)}
            ]];
        <<"concise">> ->
            FormattedCalls = lists:foldl(fun(Call, List) ->
                WhappsCall = props:get_value(<<"call">>, Call),

                case {whapps_call:other_leg_call_id(WhappsCall) /= undefined, props:get_value(<<"username">>, Call)} of
                    %{true, undefined} -> [ami_channel_status(Call, bridged, Format)] ++ List;
                    {_, undefined} -> List;
                    {true, _} -> [ami_channel_status(Call, bridged_extension, Format)] ++ List;
                    {false, _} -> [ami_channel_status(Call, bridged_extension, Format)] ++ List;
                    {_, _} -> lager:debug("undefined channel status format for call ~p", [Call])
                end
            end, [], FilteredCalls),

            {[
                <<"Response: Follows\r\nPrivilege: Command\r\n">>
            ] ++ FormattedCalls ++ [
                <<"--END COMMAND--\r\n\r\n">>
            ], raw}
    end.

% Existing whapps_call to AMI status translator
ami_channel_status(Call, Schema, <<"Status">>) ->
    ALegCID = props:get_value(<<"aleg_cid">>, Call),
    BLegCID = props:get_value(<<"bleg_cid">>, Call),
    WhappsCall = props:get_value(<<"call">>, Call),

    {ChannelState, ChannelStateDesc} = case props:get_value(<<"answered">>, Call) of
        false ->
            {0, <<"Down">>};
        true ->
            {6, <<"Up">>};
        _ ->
            {6, <<"Up">>}
    end,

    case props:get_value(<<"direction">>, Call) of
    	<<"inbound">> ->
    		ami_channel_status(
    			<<"Status">>,
    			Schema,
    			props:get_value(<<"bleg_ami_channel">>, Call),
    			BLegCID,
    			ALegCID,
    			whapps_call:account_id(WhappsCall),
    			ChannelState,
    			ChannelStateDesc,
    			props:get_value(<<"bleg_exten">>, Call),
    			props:get_value(<<"elapsed_s">>, Call),
    			props:get_value(<<"aleg_ami_channel">>, Call),
    			whapps_call:other_leg_call_id(WhappsCall),
    			whapps_call:call_id(WhappsCall)
    		);
    	<<"outbound">> ->
    		ami_channel_status(
    			<<"Status">>,
    			Schema,
    			props:get_value(<<"aleg_ami_channel">>, Call),
    			ALegCID,
    			BLegCID,
    			whapps_call:account_id(WhappsCall),
    			ChannelState,
    			ChannelStateDesc,
    			props:get_value(<<"aleg_exten">>, Call),
    			props:get_value(<<"elapsed_s">>, Call),
    			props:get_value(<<"bleg_ami_channel">>, Call),
    			whapps_call:call_id(WhappsCall),
    			whapps_call:other_leg_call_id(WhappsCall)
    		)
    end;

ami_channel_status(Call, _Schema, <<"concise">>) ->
    Channel = binary_to_list(props:get_value(<<"aleg_ami_channel">>, Call)),
    Exten = binary_to_list(props:get_value(<<"aleg_exten">>, Call)),
    {DestChannel, BridgedTo} = case props:get_value(<<"bleg_ami_channel">>, Call) of
        undefined ->
            {binary_to_list(props:get_value(<<"bleg_exten">>, Call)), "(None)"};
        BlegChannel ->
            {binary_to_list(BlegChannel), binary_to_list(BlegChannel)}
    end,
    CID = binary_to_list(props:get_value(<<"aleg_cid">>, Call)),
    Direction = props:get_value(<<"direction">>, Call),
    RunTime = wh_util:to_list(props:get_value(<<"elapsed_s">>, Call)),

    Data = case Direction of
        <<"outbound">> ->
            "(Outgoing Line)";
        <<"inbound">> ->
            DestChannel
    end,

    ChannelStateDesc = case props:get_value(<<"answered">>, Call) of
        false ->
            "Down";
        true ->
            "Up";
        _ ->
            "Up"
    end,

    wh_util:to_binary(Channel ++ "!from-internal!" ++ Exten ++ "!1!" ++ ChannelStateDesc ++
        "!Dial!" ++ Data ++ "!" ++
        CID ++ "!!!3!" ++ RunTime ++ "!" ++ BridgedTo ++ "\n");
ami_channel_status(Call, _Schema, <<"verbose">>) ->
    Channel = binary_to_list(props:get_value(<<"aleg_ami_channel">>, Call)),
    Exten = binary_to_list(props:get_value(<<"aleg_exten">>, Call)),
    {DestChannel, BridgedTo} = case props:get_value(<<"bleg_ami_channel">>, Call) of
        undefined ->
            {binary_to_list(props:get_value(<<"bleg_exten">>, Call)), "(None)"};
        BlegChannel ->
            {binary_to_list(BlegChannel), binary_to_list(BlegChannel)}
    end,
    CID = binary_to_list(props:get_value(<<"bleg_cid">>, Call)),
    Direction = props:get_value(<<"direction">>, Call),
    RunTime = props:get_value(<<"elapsed_s">>, Call),

    Data = case Direction of
        <<"outbound">> ->
            "(Outgoing Line)";
        <<"inbound">> ->
            DestChannel
    end,

    ChannelStateDesc = case props:get_value(<<"answered">>, Call) of
        false ->
            "Down";
        true ->
            "Up";
        _ ->
            "Up"
    end,

    {H, M, S} = {RunTime div 3600, RunTime rem 3600 div 60, RunTime rem 60},
    TimeString = wh_util:to_list(if H > 99 ->
        io_lib:format("~B:~2..0B:~2..0B", [H, M, S]);
    true ->
        io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S])
    end),

    wh_util:to_binary(fit_list(Channel, 20) ++ " from-internal        " ++ fit_list(Exten, 16) ++ " " ++
        "   1 " ++ fit_list(ChannelStateDesc, 7) ++ " Dial         " ++ fit_list(Data, 25) ++ " " ++
        fit_list(CID, 15) ++ " " ++ fit_list(TimeString, 8) ++ "                      " ++
        fit_list(BridgedTo, 20) ++ "\n").

fit_list(List, Size) ->
    fit_list(List, length(List), Size).

fit_list(List, ListSize, Size) when ListSize > Size ->
    lists:sublist(List, Size);
fit_list(List, ListSize, Size) when ListSize =:= Size ->
    List;
fit_list(List, ListSize, Size) when ListSize < Size ->
    append_space(List, Size-ListSize).

append_space(List, Count) ->
    lists:reverse(append_space(lists:reverse(List), Count, 0)).

append_space(RevList, Count, Count) ->
    RevList;
append_space(RevList, Count, Index) ->
    append_space(" " ++ RevList, Count, Index+1).

ami_channel_status(<<"Status">>, Schema, SourceChannel, SourceCID, DestCID, AccountId, ChannelState, ChannelStateDesc, SourceExtension,
	ElapsedSeconds, DestChannel, SourceCallId, DestCallId) ->
	AMI_Status_Header = [
        {<<"Event">>, <<"Status">>},
        {<<"Privilege">>, <<"Call">>}
    ],
    AMI_Status_Body = case Schema of
        bridged_extension -> [
             {<<"Channel">>, SourceChannel},
             {<<"CallerIDNum">>, SourceCID},
             {<<"CallerIDName">>, SourceCID},
             {<<"ConnectedLineNum">>, DestCID},
             {<<"ConnectedLineName">>, DestCID},
             {<<"Accountcode">>, AccountId},
             {<<"ChannelState">>, ChannelState}, % Numeric channel state
             {<<"ChannelStateDesc">>, ChannelStateDesc},
             %{<<"Context">>, proplists:get_value(<<"context">>, Call)},
             {<<"Context">>, <<"from-internal">>},
             {<<"Extension">>, SourceExtension},
             {<<"Priority">>, <<"12">>},
             {<<"Seconds">>, ElapsedSeconds},
             {<<"BridgedChannel">>, DestChannel},
             {<<"BridgedUniqueid">>, DestCallId}
         ];
         extension -> [
             {<<"Accountcode">>, AccountId}
         ];
         bridged -> [
             {<<"Channel">>, SourceChannel},
             {<<"CallerIDNum">>, SourceCID},
             {<<"CallerIDName">>, SourceCID},
             {<<"ConnectedLineNum">>, DestCID},
             {<<"ConnectedLineName">>, DestCID},
             {<<"Account">>, <<"">>},
             {<<"State">>, ChannelStateDesc},
             {<<"BridgedChannel">>, DestChannel},
             {<<"BridgedUniqueid">>, DestCallId}
         ];
         _ -> []
     end,
     AMI_Status_Footer = [
         {<<"Uniqueid">>, SourceCallId}
     ],
     AMI_Status_Header ++ AMI_Status_Body ++ AMI_Status_Footer.
    
queues_status(Props) ->
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),
    {'ok', Results} = couch_mgr:get_results(AccountDb, <<"queues/crossbar_listing">>),

    lists:foldl(fun(Result, Acc) ->
        QueueId = wh_json:get_value(<<"id">>, Result),
        case couch_mgr:open_doc(AccountDb, QueueId) of
        	{'error', E} ->
        		lager:debug("Error opening queue doc: ~p", [E]),
        		[] ++ Acc;
        	{'ok', QueueDoc} ->
        		case couch_mgr:get_results(AccountDb, <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
        			{'error', E} ->
        				lager:debug("Could not find queue number for queue ~p (~p)", [QueueId, E]),
        				[] ++ Acc;
        			{'ok', Results2} when length(Results2) =:= 1 ->
        				Value = wh_json:get_value(<<"value">>, hd(Results2)),
        				Number = hd(Value),

        				RawStats = queue_stats(QueueId, AccountId),
        				{Calls, Holdtime, TalkTime, Completed, Abandoned, AgentStats} = case RawStats of
        					{error, E} ->
        						lager:debug("Error ~p when getting queue stats for queue ~p", [E, QueueId]),
        						{0, 0, 0, 0, 0};
        					{ok, Resp} ->
        						count_stats(Resp)
        				end,

        				CompletedCalls = Completed - Abandoned,
        				AverageHold = case CompletedCalls of
        					0 ->
        						0.0;
        					_ ->
        						Holdtime / CompletedCalls
        				end,
        				WaitingCalls = Calls - Completed,

        				[[
        					{<<"Event">>, <<"QueueParams">>},
					        {<<"Queue">>, Number},
					        {<<"Max">>, wh_json:get_value(<<"max_queue_size">>, QueueDoc)},
					        {<<"Strategy">>, wh_json:get_value(<<"strategy">>, QueueDoc)},
					        %% Calls actually represents number of waiting calls
					        {<<"Calls">>, WaitingCalls},
					        {<<"Holdtime">>, AverageHold},
					        {<<"TalkTime">>, TalkTime},
					        {<<"Completed">>, CompletedCalls},
					        {<<"Abandoned">>, Abandoned},
					        % TODO: add servicelevel
					        {<<"ServiceLevel">>, 60},
					        {<<"ServicelevelPerf">>, 69.0}
        				]]
        				++ agent_statuses(QueueId, AccountId, Number, AgentStats)
        				++ Acc;
        			{'ok', Results2} ->
        				lager:debug("Too many results when trying to find queue number for queue ~p: ~p", [QueueId, Results2]),
        				[] ++ Acc
        		end
        end
    end, [], Results).
    
% TODO: maybe we need acdc stats to be persisted in couch
queue_stats(QueueId, AcctId) ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
             ,{<<"Queue-ID">>, QueueId}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    whapps_util:amqp_pool_request(
        Req,
        fun wapi_acdc_stats:publish_current_calls_req/1,
        fun wapi_acdc_stats:current_calls_resp_v/1
    ).

% TODO: may still need to add counting of agents fails
count_stats(Stats) ->
    AllStats = wh_json:get_value(<<"Handled">>, Stats, []) ++
        wh_json:get_value(<<"Abandoned">>, Stats, []) ++
        wh_json:get_value(<<"Waiting">>, Stats, []) ++
        wh_json:get_value(<<"Processed">>, Stats, []),
    count_stats(AllStats, {0, 0, 0, 0, 0, []}).
    
count_stats([], {Calls, Holdtime, TalkTime, Completed, Abandoned, AgentStats}) ->
    {Calls, Holdtime, TalkTime, Completed, Abandoned, AgentStats};
count_stats([Stat|Stats], {Calls, Holdtime, TalkTime, Completed, Abandoned, AgentStats}) ->
	Status = wh_json:get_value(<<"status">>, Stat),
	AgentStats2 = if (Status =:= <<"handled">>) or (Status =:= <<"processed">>) ->
		AgentId = wh_json:get_value(<<"agent_id">>, Stat),
		props:set_value(AgentId, [
				{<<"CallsTaken">>, props:get_value(<<"CallsTaken">>, props:get_value(AgentId, AgentStats, []), 0) + 1},
				{<<"LastCall">>, wh_json:get_first_defined([<<"processed_timestamp">>, <<"handled_timestamp">>], Stat, 0) - 62167219200}
			], AgentStats);
	true ->
		AgentStats
	end,

    case Status of
        <<"abandoned">> ->
            WaitTime = wh_json:get_value(<<"abandoned_timestamp">>, Stat) -
                wh_json:get_value(<<"entered_timestamp">>, Stat),
            count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime, Completed+1, Abandoned+1, AgentStats2});
        <<"waiting">> ->
            % TODO: updated the calculation for wait can call time
            WaitTime = wh_util:current_tstamp() - wh_json:get_value(<<"entered_timestamp">>, Stat),
            count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime, Completed, Abandoned, AgentStats2});
        <<"handled">> ->
            WaitTime = wh_json:get_value(<<"handled_timestamp">>, Stat) -
                wh_json:get_value(<<"entered_timestamp">>, Stat),
            CallTime = wh_util:current_tstamp() - wh_json:get_value(<<"handled_timestamp">>, Stat),
            count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime+CallTime, Completed+1, Abandoned, AgentStats2});
        <<"processed">> ->
            case wh_json:get_value(<<"abandoned_timestamp">>, Stat) of
                undefined ->
                    WaitTime = wh_json:get_value(<<"handled_timestamp">>, Stat) -
                        wh_json:get_value(<<"entered_timestamp">>, Stat),
                    CallTime = wh_json:get_value(<<"processed_timestamp">>, Stat) -
                        wh_json:get_value(<<"handled_timestamp">>, Stat),
                    count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime+CallTime, Completed+1, Abandoned, AgentStats2});
                _ ->
                    WaitTime = wh_json:get_value(<<"abandoned_timestamp">>, Stat) -
                        wh_json:get_value(<<"entered_timestamp">>, Stat),
                    count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime, Completed+1, Abandoned+1, AgentStats2})
            end
    end.

agent_statuses(QueueId, AccountId, Number, AgentStats) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    {ok, Results} = couch_mgr:get_results(AccountDb, <<"queues/agents_listing">>, [{key, QueueId}]),
    lists:foldl(fun(Result, Acc) ->
        AgentId = wh_json:get_value(<<"id">>, Result),
        [agent_status(AgentId, AccountId, Number, AgentStats) | Acc]
        end, [], Results).
        
agent_status(AgentId, AccountId, Number, AgentStats) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded), 
    {ok, UserDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    FirstName = wh_json:get_value(<<"first_name">>, UserDoc),
    LastName = wh_json:get_value(<<"last_name">>, UserDoc),
    Username = wh_json:get_value(<<"username">>, UserDoc),

    {'ok', Status} = acdc_agent_util:most_recent_status(AccountId, AgentId),
    % TODO: properly assigned paused based on status
    Paused = case Status of
        <<"paused">> ->
            1;
        _ ->
            0
    end,

    [
        {<<"Event">>, <<"QueueMember">>},
        {<<"Queue">>, Number},
        {<<"Name">>, <<FirstName/binary, " ", LastName/binary>>},
        {<<"Location">>, <<"Local/", Username/binary, "@from-queue/n">>},
        %% Membership static is also possible
        {<<"Membership">>, <<"dynamic">>},
        {<<"Penalty">>, 0},
        {<<"CallsTaken">>, props:get_value(<<"CallsTaken">>, props:get_value(AgentId, AgentStats, []), 0)},
        {<<"LastCall">>, props:get_value(<<"LastCall">>, props:get_value(AgentId, AgentStats, []), 0)},
        {<<"Status">>, translate_status(Status)},
        {<<"Paused">>, Paused}
    ].
    
translate_status(Status) ->
    % TODO: properly translate statuses
    case Status of
        <<"ready">> ->
            1;
        <<"connected">> ->
            2;
        <<"outbound">> ->
            2;
        <<"logged_out">> ->
            5;
        <<"paused">> ->
            5;
        <<"wrapup">> ->
            1;
        _ ->
            lager:debug("unspecified status ~p", [Status]),
            5
    end.

sip_peers(Props) ->
	AccountId = props:get_value(<<"AccountId">>, Props),
	AccountDb = props:get_value(<<"AccountDb">>, Props),
	{'ok', Results} = couch_mgr:get_results(AccountDb, <<"devices/listing_by_owner">>),
        lists:foldl(fun(Result, Registrations) ->
        	Value = wh_json:get_value(<<"value">>, Result),
        	case wh_json:get_value(<<"key">>, Result) of
        		'null' ->
        			[reg_entry(AccountId, wh_json:get_value(<<"id">>, Value), wh_json:get_value(<<"name">>, Value))] ++ Registrations;
        		OwnerId ->
        			case couch_mgr:open_doc(AccountDb, OwnerId) of
        				{'error', 'not_found'} ->
        					lager:debug("Missing owner ~p for endpoint with username ~p", [OwnerId, wh_json:get_value(<<"name">>, Value)]),
        					Registrations;
        				{'ok', Endpoint} ->
	            			[reg_entry(AccountId, wh_json:get_value(<<"_id">>, Endpoint), wh_json:get_value(<<"username">>, Endpoint))] ++ Registrations
	            	end
        	end
        end, [], Results).

reg_entry(AccountId, EndpointId, EndpointName) ->
	case ami_sm:registration(AccountId, EndpointId) of
		'not_registered' ->
			[
				{<<"Event">>, <<"PeerEntry">>},
                {<<"Channeltype">>, <<"SIP">>},
                {<<"ObjectName">>, EndpointName},
                {<<"ChanObjectType">>, <<"peer">>},
                {<<"IPaddress">>, <<"-none-">>},
                {<<"IPport">>, 0},
                {<<"Dynamic">>, <<"yes">>},
                {<<"Forcerport">>, <<"no">>},
                {<<"VideoSupport">>, <<"no">>},
                {<<"TextSupport">>, <<"no">>},
                {<<"ACL">>, <<"yes">>},
                {<<"Status">>, <<"UNKNOWN">>},
                {<<"RealtimeDevice">>, <<"no">>}
			];
		RegProps ->
			[
				{<<"Event">>, <<"PeerEntry">>},
                {<<"Channeltype">>, <<"SIP">>},
                {<<"ObjectName">>, EndpointName},
                {<<"ChanObjectType">>, <<"peer">>},
                {<<"IPaddress">>, props:get_value(<<"IP">>, RegProps)},
                {<<"IPport">>, props:get_value(<<"Port">>, RegProps)},
                {<<"Dynamic">>, <<"yes">>},
                {<<"Forcerport">>, <<"no">>},
                {<<"VideoSupport">>, <<"no">>},
                {<<"TextSupport">>, <<"no">>},
                {<<"ACL">>, <<"yes">>},
                {<<"Status">>, <<"OK (1 ms)">>},
                {<<"RealtimeDevice">>, <<"no">>}
			]
	end.

mailbox_count_error(ActionId, Mailbox) ->
    [
        {<<"Response">>, <<"Success">>},
        {<<"ActionID">>, ActionId},
        {<<"Message">>, <<"Mailbox Message Count">>},
        {<<"Mailbox">>, Mailbox},
        {<<"UrgMessages">>, 0},
        {<<"NewMessages">>, 0},
        {<<"OldMessages">>, 0}
    ].

queue_add(Props) ->
    Interface = proplists:get_value(<<"Interface">>, Props),
    Exten = hd(binary:split(binary:replace(Interface, <<"Local/">>, <<"">>), <<"@">>)),
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),

    %% Load agent doc having the Exten given as name
    case couch_mgr:get_results(
        AccountDb,
        <<"users/list_by_username">>,
        [{key, Exten}]
    ) of
        {ok, [Result]} ->
            queue_add(AccountId, AccountDb, Result, Props);
        _ ->
            %% Error, could not find user
            {error, not_a_user}
    end.

queue_add(AccountId, AccountDb, Result, Props) ->
    AgentId = wh_json:get_value(<<"id">>, Result),
    {ok, QueueDoc} = amimulator_util:queue_for_number(proplists:get_value(<<"Queue">>, Props), AccountDb),
    QueueId = wh_json:get_value(<<"_id">>, QueueDoc),

    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    couch_mgr:save_doc(AccountDb, cb_queues:maybe_add_queue_to_agent(QueueId, AgentDoc)),

    Prop = props:filter_undefined(
     [{<<"Account-ID">>, AccountId}
      ,{<<"Agent-ID">>, AgentId}
      ,{<<"Queue-ID">>, QueueId}
      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
     ]),
    wapi_acdc_agent:publish_login_queue(Prop),

    Payload = [
        {<<"Response">>, <<"Success">>},
        {<<"Message">>, <<"Added interface to queue">>}
    ],
    {ok, {Payload, n}}.

queue_remove(Props) ->
    Interface = proplists:get_value(<<"Interface">>, Props),
    Exten = hd(binary:split(binary:replace(Interface, <<"Local/">>, <<"">>), <<"@">>)),
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),

    %% Load agent doc having the Exten given as name
    {ok, [Result]} = couch_mgr:get_results(AccountDb,
        <<"users/list_by_username">>,
        [{key, Exten}]
    ),
    AgentId = wh_json:get_value(<<"id">>, Result),
    {ok, QueueDoc} = amimulator_util:queue_for_number(proplists:get_value(<<"Queue">>, Props), AccountDb),
    QueueId = wh_json:get_value(<<"_id">>, QueueDoc),

    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    couch_mgr:save_doc(AccountDb, cb_queues:maybe_rm_queue_from_agent(QueueId, AgentDoc)),

    Prop = props:filter_undefined(
     [{<<"Account-ID">>, AccountId}
      ,{<<"Agent-ID">>, AgentId}
      ,{<<"Queue-ID">>, QueueId}
      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
     ]),
    wapi_acdc_agent:publish_logout_queue(Prop),

    Payload = [
        {<<"Response">>, <<"Success">>},
        {<<"Message">>, <<"Removed interface from queue">>}
    ],
    {ok, {Payload, n}}.

queue_pause(Props) ->
    Interface = proplists:get_value(<<"Interface">>, Props),
    Exten = hd(binary:split(binary:replace(Interface, <<"Local/">>, <<"">>), <<"@">>)),
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),

    %% Load agent doc having the Exten given as name
    case couch_mgr:get_results(
        AccountDb,
        <<"users/list_by_username">>,
        [{key, Exten}]
    ) of
        {ok, [Result]} ->
            pause_exten(AccountId, Result, Props);
        _ ->
            %% Error, could not find user
            {error, not_a_user}
    end.

pause_exten(AccountId, Result, Props) ->
    AgentId = wh_json:get_value(<<"id">>, Result),

    case props:get_value(<<"Paused">>, Props) of
        <<"0">> ->
            Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
              ,{<<"Agent-ID">>, AgentId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
            wapi_acdc_agent:publish_resume(Prop),

            Payload = [
                {<<"Response">>, <<"Success">>},
                {<<"Message">>, <<"Interface unpaused successfully">>}
            ],
            {ok, {Payload, n}};
        <<"1">> ->
            Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Time-Limit">>, 600000}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
            wapi_acdc_agent:publish_pause(Prop),

            Payload = [
                {<<"Response">>, <<"Success">>},
                {<<"Message">>, <<"Interface paused successfully">>}
            ],
            {ok, {Payload, n}}
    end.

getvar(<<"CDR(dst)">>, Props) ->
    Channel = props:get_value(<<"Channel">>, Props),
    CallIds = ami_sm:channel_call_ids(Channel),
    case CallIds of
        %% The call id is undefined if the channel is not a sip peer
        undefined ->
            undefined;
        _ ->
            lager:debug("channel ~p", [props:get_value(<<"Channel">>, Props)]),
            CallId = hd(CallIds),
            lager:debug("callid ~p", [CallId]),
            Call = ami_sm:call(CallId),

            [
                {<<"Response">>, <<"Success">>},
                {<<"Variable">>, props:get_value(<<"Variable">>, Props)},
                {<<"Value">>, props:get_value(<<"bleg_cid">>, Call)},
                {<<"ActionID">>, props:get_value(<<"ActionID">>, Props)}
            ]
    end;
getvar(<<"AGENTBYCALLERID_", _CallerId/binary>>=Variable, _Props) ->
    {[
        <<"Response: Success\r\n">>,
        <<"Variable: ", Variable/binary, "\r\nValue: \r\n\r\n">>
    ], raw};
getvar(<<"EPOCH">>=Variable, _Props) ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    Timestamp = wh_util:to_binary(MegaSecs * 1000000 + Secs),
    {[
        <<"Response: Success\r\n">>,
        <<"Variable: ", Variable/binary, "\r\nValue: ", Timestamp/binary, "\r\n\r\n">>
    ], raw};
getvar(Variable, Props) ->
    lager:debug("Unhandled getvar ~p with props ~p", [Variable, Props]),
    undefined.

command(<<"meetme list ", MeetMeSpec/binary>>, Props) ->
    [Number, _Mode] = binary:split(MeetMeSpec, <<" ">>),
    maybe_list_conf(Number, Props);
command(<<"core show channels ", Verbosity/binary>>, Props) ->
    initial_channel_status(amimulator_util:initial_calls(proplists:get_value(<<"AccountId">>, Props)), Props, Verbosity);
command(CommandName, Props) ->
    lager:debug("Unhandled command ~p with props ~p", [CommandName, Props]),
    {[
        <<"Response: Follows\r\nPrivilege: Command\r\n">>,
        <<"No such command '", CommandName/binary, "' (type 'core show help ",
            CommandName/binary, "' for other possible commands)\n">>,
        <<"--END COMMAND--\r\n\r\n">>
    ], raw}.

user_event(<<"MEETME-REFRESH">>, Props) ->
    [[
        {<<"Response">>, <<"Success">>},
        {<<"Message">>, <<"Event Sent">>}
    ] ++ [
        {<<"Event">>, <<"UserEvent">>},
        {<<"Privilege">>, <<"user,all">>},
        {<<"UserEvent">>, <<"MEETME-REFRESH">>},
        {<<"Action">>, <<"UserEvent">>},
        {<<"Meetme">>, props:get_value(<<"Meetme">>, Props)}
    ]];
user_event(EventName, Props) ->
	lager:debug("Unhandled event ~p with props ~p", [EventName, Props]),
	undefined.

maybe_list_conf(Number, Props) ->
    AccountDb = props:get_value(<<"AccountDb">>, Props),
    {ok, Results} = couch_mgr:get_results(AccountDb, <<"conferences/conference_map">>),
    maybe_conf_has_number(Number, props:get_value(<<"ActionID">>, Props), Results).

maybe_conf_has_number(_Number, _ActionId, []) ->
    undefined;
maybe_conf_has_number(Number, ActionId, [Result|Results]) ->
    case wh_json:get_value([<<"value">>, <<"numbers">>], Result) of
        [_|_]=Numbers ->
            Found = lists:any(fun(Number2) ->
                if Number2 =:= Number ->
                        true;
                true ->
                        false
                end
            end, Numbers),
            if Found ->
                conf_details(ActionId, wh_json:get_value(<<"key">>, Result));
            true ->
                maybe_conf_has_number(Number, ActionId, Results)
            end;
        _ ->
            maybe_conf_has_number(Number, ActionId, Results)
    end.

conf_details(ActionId, ConfId) ->
    Req = props:filter_undefined([
        {<<"Conference-ID">>, ConfId}
        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ]),
    case whapps_util:amqp_pool_collect(
        Req,
        fun wapi_conference:publish_search_req/1,
        {ecallmgr, true}
    ) of
        {'error', E} ->
            lager:debug("conf_details error ~p", [E]),
            undefined;
        {'ok', Resp} ->
            maybe_conf_down(ActionId, Resp)
    end.

maybe_conf_down(_ActionId, []) ->
    undefined;
maybe_conf_down(ActionId, [JObj|JObjs]) ->
    case wh_json:get_value(<<"Participants">>, JObj) of
        undefined ->
            maybe_conf_down(ActionId, JObjs);
        Participants ->
            participant_payloads(Participants, wh_json:get_integer_value(<<"Run-Time">>, JObj), ActionId)
    end.

participant_payloads(Participants, RunTime, ActionId) ->
    {H, M, S} = {RunTime div 3600, RunTime rem 3600 div 60, RunTime rem 60},
    TimeString = wh_util:to_binary(if H > 99 ->
        io_lib:format("~B:~2..0B:~2..0B", [H, M, S]);
    true ->
        io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S])
    end),

    {[
        <<"Response: Follows\r\nPrivilege: Command\r\n">>,
        <<"ActionID: ", ActionId/binary, "\r\n">>
    ] ++ lists:foldl(fun(Participant, Payloads) ->
        CallId = wh_json:get_value(<<"Call-ID">>, Participant),
        Call = ami_sm:call(CallId),

        ParticipantId = wh_util:to_binary(wh_json:get_value(<<"Participant-ID">>, Participant)),
        %ParticipantId = <<"1">>,
        CallerId = props:get_value(<<"aleg_cid">>, Call),
        EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

        [
            <<ParticipantId/binary, "!!", CallerId/binary, "!",
                EndpointName/binary, "!!!!!-", ParticipantId/binary, "!", TimeString/binary, "\n">>
        ] ++ Payloads
        end, [], Participants) ++
    [
        <<"--END COMMAND--\r\n\r\n">>
    ], raw}.













