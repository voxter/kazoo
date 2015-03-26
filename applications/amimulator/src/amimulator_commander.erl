-module(amimulator_commander).

-export([handle/2]).
-export([registrations/1, reg_entries/2]).

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
    ActionID = proplists:get_value(<<"ActionID">>, Props),
    {ok, AMIDoc} = couch_mgr:open_doc(?AMI_DB, Username),
    %case wh_json:get_value(<<"Secret">>, AMIDoc) of
    case Secret of
        undefined ->
            %% Successful login
            lager:debug("successful login, starting event listener"),
            gen_server:cast(self(), {login, wh_json:get_value(<<"account_id">>, AMIDoc)}),
            
            Payload = [[
                {<<"Response">>, <<"Success">>},
                {<<"ActionID">>, ActionID},
                {<<"Message">>, <<"Authentication accepted">>}
            ],[
                {<<"Event">>, <<"FullyBooted">>},
                {<<"Privilege">>, <<"system,all">>},
                {<<"Status">>, <<"Fully Booted">>}
            ]],
            {ok, {Payload, broken}};
        _ ->
            %% Failed login
            Payload = [
                {<<"Response">>, <<"Error">>},
                {<<"ActionID">>, ActionID},
                {<<"Message">>, <<"Authentication failed">>}
            ],
            {ok, {Payload, n}}
    end;
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
    Payload = initial_channel_status(amimulator_util:initial_calls(proplists:get_value(<<"AccountId">>, Props)), Props),
    {ok, {Payload, n}};
handle_event("queuestatus", Props) ->
    Payload = [[
        {<<"Response">>, <<"Success">>},
        {<<"Message">>, <<"Queue status will follow">>}
    ]] ++ queues_status(Props) ++
    [[
        {<<"Event">>, <<"QueueStatusComplete">>}
    ]],
    {ok, {Payload, n}};
handle_event("sippeers", Props) ->
    SipPeers = lists:foldl(fun({_Number, _OnOff, Reg}, Acc) ->
        [Reg | Acc]
    end, [], reg_entries(registrations(Props), Props)),

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

    Payload = case couch_mgr:get_results(AccountDb, <<"vmboxes/listing_by_mailbox">>, [{key, Exten}]) of
        {ok, [Result]} ->
            lager:debug("result ~p", [Result]),
            case couch_mgr:open_doc(AccountDb, <<"asdf">>) of
                {ok, VMDoc} ->
                    Messages = wh_json:get_value(<<"messages">>, VMDoc),
                    NewCount = lists:foldl(fun(Message, Total) ->
                        case wh_json:get_value(<<"folder">>, Message) of
                            <<"new">> ->
                                Total + 1;
                            _ ->
                                Total
                        end
                    end, 0, Messages),
                    [
                        {<<"Response">>, <<"Success">>},
                        {<<"ActionID">>, ActionId},
                        {<<"Message">>, <<"Mailbox Message Count">>},
                        {<<"Mailbox">>, Mailbox},
                        {<<"UrgMessages">>, 0},
                        {<<"NewMessages">>, NewCount},
                        {<<"OldMessages">>, length(Messages) - NewCount}
                    ];
                {error, _E} ->
                    mailbox_count_error(ActionId, Mailbox)
            end;
        _ ->
            mailbox_count_error(ActionId, Mailbox)
    end,
    {ok, {Payload, n}};
handle_event("queuepause", Props) ->
    queue_pause(Props);
handle_event("queueadd", Props) ->
    queue_add(Props);
handle_event("queueremove", Props) ->
    Interface = proplists:get_value(<<"Interface">>, Props),
    Exten = hd(binary:split(binary:replace(Interface, <<"Local/">>, <<"">>), <<"@">>)),
    AccountId = proplists:get_value(<<"Account">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),

    %% Load agent doc having the Exten given as name
    {ok, [Result]} = couch_mgr:get_results(AccountDb,
        <<"users/list_by_username">>,
        [{key, Exten}]
    ),
    AgentId = wh_json:get_value(<<"id">>, Result),
    {ok, QueueDoc} = amimulator_util:queue_for_number(proplists:get_value(<<"Queue">>, Props), AccountDb),
    QueueId = wh_json:get_value(<<"_id">>, QueueDoc),

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
    {ok, {Payload, n}};
handle_event("originate", Props) ->
    case proplists:get_value(<<"Channel">>, Props) of
        undefined ->
            {error, channel_not_specified};
        _ ->
            case proplists:get_value(<<"Application">>, Props) of
                <<"ChanSpy">> ->
                    gen_server:cast(self(), {originator, "eavesdrop", Props});
                _ ->
                    gen_server:cast(self(), {originator, "originate", Props})
            end
    end;
handle_event("hangup", Props) ->
    CallId = amimulator_store:get(<<"channel-", (proplists:get_value(<<"Channel">>, Props))/binary>>),
    Call = amimulator_util:whapps_call_from_cf_exe(CallId),
    whapps_call_command:hangup(Call);
handle_event(Event, _Props) ->
    lager:debug("no handler defined for event ~p", [Event]),
    {error, no_action}.

initial_channel_status(Calls, Props) ->
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    FilteredCalls = lists:filter(fun(Call) ->  
        case whapps_call:account_id(props:get_value(<<"call">>, Call)) of
            AccountId -> true;
            _ -> false
        end
    end, Calls),
    FormattedCalls = lists:foldl(fun(Call, List) ->
        WhappsCall = props:get_value(<<"call">>, Call),

        case {whapps_call:other_leg_call_id(WhappsCall) /= undefined, proplists:is_defined(<<"username">>, Call)} of
            {true, true} -> [ami_channel_status(Call, bridged_extension)] ++ List;
            {true, false} -> [ami_channel_status(Call, bridged)] ++ List;
            {false, true} -> [ami_channel_status(Call, extension)] ++ List;
            {_, _} -> lager:debug("undefined channel status format for call ~p", [Call])
        end
    end, [], FilteredCalls),
    [[
        {<<"Response">>, <<"Success">>},
        {<<"Message">>, <<"Channel status will follow">>}
    ]] ++ FormattedCalls ++ [[
        {<<"Event">>, <<"StatusComplete">>},
        {<<"Items">>, length(FormattedCalls)}
    ]].
    
queues_status(Props) ->
    AcctId = proplists:get_value(<<"Account">>, Props),
    AcctSupers = acdc_queues_sup:find_acct_supervisors(AcctId),
    lists:foldl(fun(Super, Results) ->
    	case queue_status(Super) of
    		{ok, Status} ->
    			Status ++ Results;
    		{error, _E} ->
    			Results
    	end end, [], AcctSupers).
    
queue_status(Super) ->
    Manager = acdc_queue_sup:manager(Super),
    {AcctId, QueueId} = acdc_queue_manager:config(Manager),
    case queue_details(QueueId, AcctId) of
    	{ok, QueueDetails} ->
		    Agents = acdc_queue_manager:status(Manager),
		    AgentsStatus = agents_status(Agents, [], AcctId, QueueDetails),
		    {ok, format_queue_status(QueueDetails, AgentsStatus)};
		{error, E} ->
			{error, E}
	end.
    
format_queue_status(QueueDetails, AgentsStatus) ->
    {Calls, Holdtime, TalkTime, Completed, Abandoned} = proplists:get_value(<<"QueueStats">>, QueueDetails),
    CompletedCalls = Completed - Abandoned,
    AverageHold = case CompletedCalls of
        0 ->
            0;
        _ ->
            Holdtime / CompletedCalls
    end,
    [[
        {<<"Event">>, <<"QueueParams">>},
        {<<"Queue">>, proplists:get_value(<<"QueueNumber">>, QueueDetails)},
        {<<"Max">>, proplists:get_value(<<"Max">>, QueueDetails)},
        {<<"Strategy">>, proplists:get_value(<<"Strategy">>, QueueDetails)},
        {<<"Calls">>, Calls},
        {<<"Holdtime">>, AverageHold},
        {<<"TalkTime">>, TalkTime},
        {<<"Completed">>, CompletedCalls},
        {<<"Abandoned">>, Abandoned},
        % TODO: add servicelevel
        {<<"ServiceLevel">>, 60},
        {<<"ServicelevelPerf">>, 69.0}
    ]] ++
    AgentsStatus.
        
queue_details(QueueId, AcctId) ->
    AcctDb = wh_util:format_account_id(AcctId, encoded),
    {ok, QueueDoc} = couch_mgr:open_doc(AcctDb, QueueId),
    case amimulator_util:find_id_number(QueueId, AcctDb) of
    	{ok, QueueNumber} ->
    		{ok, [
		        {<<"QueueNumber">>, QueueNumber},
		        {<<"Queue">>, wh_json:get_value(<<"name">>, QueueDoc)},
		        {<<"Max">>, wh_json:get_value(<<"max_queue_size">>, QueueDoc)},
		        {<<"Strategy">>, translate_strat(wh_json:get_value(<<"strategy">>, QueueDoc))},
		        {<<"QueueStats">>, queue_stats(QueueId, AcctId)}
		    ]};
		{error, not_found} ->
			lager:debug("Extension for queue ~p not found", [QueueId]),
			{error, not_found};
		{error, E} ->
			{error, E}
	end.
        
translate_strat(Strat) ->
    % TODO: actually translate the strategy names
    Strat.
    
% TODO: maybe we need acdc stats to be persisted in couch
queue_stats(_QueueId, AcctId) ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
             ,{<<"Status">>, undefined}
             ,{<<"Agent-ID">>, undefined}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case whapps_util:amqp_pool_request(
        Req,
        fun wapi_acdc_stats:publish_current_calls_req/1,
        fun wapi_acdc_stats:current_calls_resp_v/1
    ) of
        {'error', E} -> E;
        {'ok', Resp} -> count_stats(Resp)
    end.

% TODO: may still need to add counting of agents fails
count_stats(Stats) ->
    AllStats = wh_json:get_value(<<"Handled">>, Stats, []) ++
        wh_json:get_value(<<"Abandoned">>, Stats, []) ++
        wh_json:get_value(<<"Waiting">>, Stats, []) ++
        wh_json:get_value(<<"Processed">>, Stats, []),
    count_stats(AllStats, {0, 0, 0, 0, 0}).
    
count_stats([], {Calls, Holdtime, TalkTime, Completed, Abandoned}) ->
    {Calls, Holdtime, TalkTime, Completed, Abandoned};
count_stats([Stat|Stats], {Calls, Holdtime, TalkTime, Completed, Abandoned}) ->
    case wh_json:get_value(<<"status">>, Stat) of
        <<"abandoned">> ->
            count_stats(Stats, {Calls, Holdtime, TalkTime, Completed+1, Abandoned+1});
        <<"waiting">> ->
            % TODO: updated the calculation for wait can call time
            WaitTime = wh_json:get_value(<<"entered_timestamp">>, Stat),
            count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime, Completed, Abandoned});
        <<"handled">> ->
            CallTime = wh_json:get_value(<<"handled_timestamp">>, Stat),
            count_stats(Stats, {Calls+1, Holdtime, TalkTime+CallTime, Completed+1, Abandoned});
        <<"processed">> ->
            count_stats(Stats, {Calls, Holdtime, TalkTime, Completed+1, Abandoned})
    end.

agents_status([], Statuses, _AccountId, _QueueDetails) ->
    Statuses;
agents_status([Agent|Agents], Statuses, AccountId, QueueDetails) ->
    case agent_status(Agent, AccountId, QueueDetails) of
        {error, _E} ->
            agents_status(Agents, Statuses, AccountId, QueueDetails);
        {ok, Status} ->
            agents_status(Agents, Statuses ++ [Status], AccountId, QueueDetails)
    end.
        
agent_status(AgentId, AcctId, QueueDetails) ->
    AcctDb = wh_util:format_account_id(AcctId, encoded),
    {ok, UserDoc} = couch_mgr:open_doc(AcctDb, AgentId),
    FirstName = wh_json:get_value(<<"first_name">>, UserDoc),
    LastName = wh_json:get_value(<<"last_name">>, UserDoc),
    Username = wh_json:get_value(<<"username">>, UserDoc),
    
    %% Ripped from cb_agents
    %Now = wh_util:current_tstamp(),
    %Yday = Now - ?SECONDS_IN_DAY,

    %Opts = [{<<"Status">>, undefined}
    %          ,{<<"Agent-ID">>, AgentId}
    %          ,{<<"Start-Range">>, Yday}
    %          ,{<<"End-Range">>, Now}
    %          ,{<<"Most-Recent">>, true}
    %         ],

    {'ok', Status} = acdc_agent_util:most_recent_status(AcctId, AgentId),
    % TODO: properly assigned paused based on status
    Paused = case Status of
        <<"paused">> ->
            1;
        _ ->
            0
    end,

    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined ->
            {error, not_logged_in};
        AgentSup ->
            AgentListener = acdc_agent_sup:listener(AgentSup),
            LastCall = case gen_listener:call(AgentListener, last_connect) of
                undefined ->
                    0;
                {MegaSecs, Secs, MicroSecs} ->
                    MegaSecs * 1000000 + Secs + MicroSecs / 1000000
            end,
            
            %% Agent status payload
            {ok, [
                {<<"Event">>, <<"QueueMember">>},
                {<<"Queue">>, proplists:get_value(<<"QueueNumber">>, QueueDetails)},
                {<<"Name">>, <<FirstName/binary, " ", LastName/binary>>},
                {<<"Location">>, <<"Local/", Username/binary, "@from-queue/n">>},
                %% Membership static is also possible
                {<<"Membership">>, <<"dynamic">>},
                {<<"Penalty">>, 0},
                %% CallsTaken handled by count_stats function
                {<<"LastCall">>, LastCall},
                {<<"Status">>, translate_status(Status)},
                {<<"Paused">>, Paused}
            ]}
    end.
    
translate_status(Status) ->
    % TODO: properly translate statuses
    case Status of
        <<"ready">> ->
            1;
        <<"paused">> ->
            5;
        _ ->
            5
    end.

registrations(Props) ->
    {ok, AccountDoc} = couch_mgr:open_doc(<<"accounts">>, proplists:get_value(<<"AccountId">>, Props)),
    AccountRealm = wh_json:get_value(<<"realm">>, AccountDoc),

    Req = [
        {<<"Realm">>, AccountRealm}
        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ],

    ReqResp = whapps_util:amqp_pool_collect(
        Req,
        fun wapi_registration:publish_query_req/1,
        {'ecallmgr', 'true'}
    ),
    case ReqResp of
        {'error', _} -> [];
        {_, JObjs} ->
            lists:foldl(fun(JObj, SipPeers) ->
                wh_json:get_value(<<"Fields">>, JObj, []) ++ SipPeers
            end, [], JObjs)
    end.

reg_entries(SipPeers, Props) ->
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),
    case couch_mgr:get_results(AccountDb, <<"devices/crossbar_listing">>) of
        {ok, Results} ->
            lists:foldl(fun(Result, Acc) ->
                {ok, DeviceDoc} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, Result)),
                maybe_add_reg_entry(DeviceDoc, SipPeers, Props, Acc)
            end, [], Results);
        _ ->
            []
    end.

maybe_add_reg_entry(Device, SipPeers, Props, Acc) ->
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),
    Number = case wh_json:get_value(<<"owner_id">>, Device) of
        undefined ->
            wh_json:get_value(<<"name">>, Device);
        OwnerId ->
            case couch_mgr:open_doc(AccountDb, OwnerId) of
                {ok, OwnerDoc} ->
                    wh_json:get_value(<<"username">>, OwnerDoc);
                _ ->
                    wh_json:get_value(<<"name">>, Device)
            end
    end,

    case already_has_reg_entry(Number, Acc) of
        true ->
            Acc;
        false ->
            case find_peer_reg(wh_json:get_value(<<"username">>, wh_json:get_value(<<"sip">>, Device)), SipPeers) of
                not_found ->
                    [{Number, off, [
                        {<<"Event">>, <<"PeerEntry">>},
                        {<<"Channeltype">>, <<"SIP">>},
                        {<<"ObjectName">>, Number},
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
                    ]} | Acc];
                JObj ->
                    [{Number, on, [
                        {<<"Event">>, <<"PeerEntry">>},
                        {<<"Channeltype">>, <<"SIP">>},
                        {<<"ObjectName">>, Number},
                        {<<"ChanObjectType">>, <<"peer">>},
                        {<<"IPaddress">>, wh_json:get_value(<<"contact_ip">>, JObj)},
                        {<<"IPport">>, wh_json:get_value(<<"contact_port">>, JObj)},
                        {<<"Dynamic">>, <<"yes">>},
                        {<<"Forcerport">>, <<"no">>},
                        {<<"VideoSupport">>, <<"no">>},
                        {<<"TextSupport">>, <<"no">>},
                        {<<"ACL">>, <<"yes">>},
                        {<<"Status">>, <<"OK (1 ms)">>},
                        {<<"RealtimeDevice">>, <<"no">>}
                    ]} | Acc]
            end
    end.

already_has_reg_entry(_Number, []) ->
    false;
already_has_reg_entry(Number, [{Number, on, _Reg}|_Regs]) ->
    true;
already_has_reg_entry(Number, [_Reg|Regs]) ->
    already_has_reg_entry(Number, Regs).

find_peer_reg(_Username, []) ->
    not_found;
find_peer_reg(Username, [Peer|Peers]) ->
    case wh_json:get_value(<<"Username">>, Peer) of
        Username ->
            cb_registrations:normalize_registration(Peer);
        _ ->
            find_peer_reg(Username, Peers)
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
    AccountId = proplists:get_value(<<"Account">>, Props),
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

queue_pause(Props) ->
    Interface = proplists:get_value(<<"Interface">>, Props),
    Exten = hd(binary:split(binary:replace(Interface, <<"Local/">>, <<"">>), <<"@">>)),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),

    %% Load agent doc having the Exten given as name
    case couch_mgr:get_results(
        AccountDb,
        <<"users/list_by_username">>,
        [{key, Exten}]
    ) of
        {ok, [Result]} ->
            pause_exten(Interface, AccountDb, Result, Props);
        _ ->
            %% Error, could not find user
            {error, not_a_user}
    end.

pause_exten(Interface, AccountDb, Result, Props) ->
    AgentId = wh_json:get_value(<<"id">>, Result),
    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    AgentName = <<(wh_json:get_value(<<"first_name">>, AgentDoc))/binary, " ",
        (wh_json:get_value(<<"last_name">>, AgentDoc))/binary
    >>,

    case acdc_agents_sup:find_agent_supervisor(
        proplists:get_value(<<"Account">>, Props),
        AgentId
    ) of
        undefined ->
            %% Pause error message
            {error, not_an_agent};
        Pid ->
            AgentFsm = acdc_agent_sup:fsm(Pid),

            %% Found the FSM, now pause/unpause the agent
            case proplists:get_value(<<"Paused">>, Props) of
                <<"0">> ->
                    acdc_agent_fsm:resume(AgentFsm),

                    %% Respond with pause/unpause success
                    Payload = [[
                        {<<"Response">>, <<"Success">>},
                        {<<"Message">>, <<"Interface unpaused successfully">>}
                    ],[
                        {<<"Event">>, <<"QueueMemberPaused">>},
                        {<<"Privilege">>, <<"agent,all">>},
                        {<<"Queue">>, proplists:get_value(<<"Queue">>, Props)},
                        {<<"Location">>, Interface},
                        {<<"MemberName">>, AgentName},
                        {<<"Paused">>, 0}
                    ]],
                    {ok, {Payload, n}};
                <<"1">> ->
                    %% For some reason, infinity doesn't work here despite the spec saying so
                    acdc_agent_fsm:pause(AgentFsm, 600000),

                    %% Respond with pause/unpause success
                    Payload = [[
                        {<<"Response">>, <<"Success">>},
                        {<<"Message">>, <<"Interface paused successfully">>}
                    ],[
                        {<<"Event">>, <<"QueueMemberPaused">>},
                        {<<"Privilege">>, <<"agent,all">>},
                        {<<"Queue">>, proplists:get_value(<<"Queue">>, Props)},
                        {<<"Location">>, Interface},
                        {<<"MemberName">>, AgentName},
                        {<<"Paused">>, 1}
                    ]],
                    {ok, {Payload, n}}
            end
    end.

% Existing whapps_call to AMI status translator
ami_channel_status(Call, Schema) ->
    ALegCID = props:get_value(<<"aleg_cid">>, Call),
    BLegCID = props:get_value(<<"bleg_cid">>, Call),
    WhappsCall = props:get_value(<<"call">>, Call),

    lager:debug("Call ~p", [Call]),

    {ChannelState, ChannelStateDesc} = case props:get_value(<<"answered">>, Call) of
        false ->
            {0, <<"Down">>};
        true ->
            {6, <<"Up">>};
        _ ->
            {6, <<"Up">>}
    end,

    AMI_Status_Header = [
        {<<"Event">>, <<"Status">>},
        {<<"Priviledge">>, <<"Call">>}
    ],
    AMI_Status_Body = case Schema of
        bridged_extension -> [
             {<<"Channel">>, props:get_value(<<"aleg_ami_channel">>, Call)},
             {<<"CallerIDNum">>, ALegCID},
             {<<"CallerIDName">>, ALegCID},
             {<<"ConnectedLineNum">>, BLegCID},
             {<<"ConnectedLineName">>, BLegCID},
             {<<"Accountcode">>, whapps_call:account_id(WhappsCall)},
             {<<"ChannelState">>, ChannelState}, % Numeric channel state
             {<<"ChannelStateDesc">>, ChannelStateDesc},
             %{<<"Context">>, proplists:get_value(<<"context">>, Call)},
             {<<"Context">>, <<"from-internal">>},
             {<<"Extension">>, props:get_value(<<"aleg_exten">>, Call)},
             {<<"Priority">>, <<"12">>},
             {<<"Seconds">>, props:get_value(<<"elapsed_s">>, Call)},
             {<<"BridgedChannel">>, props:get_value(<<"bleg_ami_channel">>, Call)},
             {<<"BridgedUniqueid">>, whapps_call:other_leg_call_id(WhappsCall)}
         ];
         extension -> [
             {<<"Accountcode">>, proplists:get_value(<<"account_id">>, Call)}
         ];
         bridged -> [
             {<<"Channel">>, props:get_value(<<"aleg_ami_channel">>, Call)},
             {<<"CallerIDNum">>, ALegCID},
             {<<"CallerIDName">>, ALegCID},
             {<<"ConnectedLineNum">>, BLegCID},
             {<<"ConnectedLineName">>, BLegCID},
             {<<"Account">>, <<"">>},
             {<<"State">>, ChannelStateDesc},
             {<<"BridgedChannel">>, props:get_value(<<"bleg_ami_channel">>, Call)},
             {<<"BridgedUniqueid">>, whapps_call:other_leg_call_id(WhappsCall)}
         ];
         _ -> []
     end,
     AMI_Status_Footer = [
         {<<"Uniqueid">>, whapps_call:call_id(WhappsCall)}
     ],
     AMI_Status_Header ++ AMI_Status_Body ++ AMI_Status_Footer.
