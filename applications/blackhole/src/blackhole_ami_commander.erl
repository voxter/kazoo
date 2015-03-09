-module(blackhole_ami_commander).

-export([handle/2]).
-export([queue_stats/2]).

-include("blackhole.hrl").

-define(AMI_DB, <<"ami">>).
    
%% Handle a payload sent as an AMI command
handle(Payload, AccountId) ->
    Props = blackhole_ami_util:parse_payload(Payload),
    UpdatedProps = [{<<"Account">>, AccountId}] ++ Props,
    handle_event(UpdatedProps).
    
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
    AMIDoc = couch_mgr:open_doc(?AMI_DB, Username),
    %case wh_json:get_value(<<"Secret">>, AMIDoc) of
    case Secret of
        undefined ->
            %% Successful login
            lager:debug("AMI: successful login, starting event listener"),
            gen_server:cast(self(), {login, wh_json:get_value(<<"account_id">>, AMIDoc)}),
            blackhole_ami_amqp:start_link(self()),
            
            Payload = [
                {<<"Response">>, <<"Success">>},
                {<<"ActionID">>, ActionID},
                {<<"Message">>, <<"Authentication accepted">>}
            ],
            {ok, {Payload, broken}};
            
            % TODO trigger a FullyBooted event
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
   %lager:debug("AMI: Timestamp ~p", [Timestamp]),
   Payload = [
       {<<"Response">>, <<"Success">>},
       {<<"Ping">>, <<"Pong">>},
       {<<"Timestamp">>, Timestamp}
   ],
   {ok, {Payload, n}};

%% Handle AMI Status action - INCOMPLETE
% handle_event("status", Props) ->
%     AccountId = proplists:get_value(<<"Account">>, Props),
%     AllCalls = ecallmgr_maintenance:channel_details(),
%     FilteredCalls = lists:filter(fun(Call) ->  
%         case proplists:get_value(<<"account_id">>, Call) of
%             AccountId -> true;
%             _ -> false
%         end
%     end, AllCalls),
%     FormattedCalls = lists:foldl(fun(Call, List) -> 
%         case {proplists:is_defined(<<"other_leg">>), proplists:is_defined(<<"username">>)} of
%             {true, true} -> List ++ ami_channel_status(Call, bridged_extension);
%             {true, false} -> List ++ ami_channel_status(Call, bridged);
%             {false, true} -> List ++ ami_channel_status(Call, extension);
%             {_, _} -> lager:debug("AMI: undefined channel status format for call ~p", [Call]),
%         end
%     end, [], FilteredCalls),
%     Payload = [[
%         {<<"Response">>, <<"Success">>},
%         {<<"Message">>, <<"Channel status will follow">>}
%         ]] ++ FormattedCalls ++ [[
%         {<<"Event">>, <<"StatusComplete">>},
%         {<<"Items">>, length(FormattedCalls)}
%     ]],
%     {ok, {Payload, n}};
handle_event("queuestatus", Props) ->
    Payload = [[
        {<<"Response">>, <<"Success">>},
        {<<"Message">>, <<"Queue status will follow">>}
    ]] ++ queues_status(Props) ++
    [[
        {<<"Event">>, <<"QueueStatusComplete">>}
    ]],
    {ok, {Payload, n}};
handle_event("originate", Props) ->
    case proplists:get_value(<<"Channel">>, Props) of
        undefined ->
            {error, channel_not_specified};
        _ ->
            originate(Props)
    end;
handle_event(Event, _Props) ->
    lager:debug("AMI: no handler defined for event ~p", [Event]),
    {error, no_action}.
    
queues_status(Props) ->
    AcctId = proplists:get_value(<<"Account">>, Props),
    AcctSupers = acdc_queues_sup:find_acct_supervisors(AcctId),
    lists:foldl(fun(Super, Results) ->
        [queue_status(Super)] ++ Results
        end, [], AcctSupers).
    
queue_status(Super) ->
    Manager = acdc_queue_sup:manager(Super),
    {AcctId, QueueId} = acdc_queue_manager:config(Manager),
    QueueDetails = queue_details(QueueId, AcctId),
    Agents = acdc_queue_manager:status(Manager),
    AgentsStatus = lists:foldl(fun(Agent, Results) ->
        [agent_status(Agent, AcctId)] ++ Results
        end, [], Agents),
    format_queue_status(QueueDetails, AgentsStatus).
    
format_queue_status(QueueDetails, AgentsStatus) ->
    {Calls, Holdtime, TalkTime, Completed, Abandoned} = proplists:get_value(<<"QueueStats">>, QueueDetails),
    CompletedCalls = Completed - Abandoned,
    AverageHold = Holdtime / CompletedCalls,
    [[
        {<<"Event">>, <<"QueueParams">>},
        % TODO make this the callflow #
        {<<"Queue">>, proplists:get_value(<<"Queue">>, QueueDetails)},
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
    AcctDb = wh_util:format_account_mod_id(AcctId),
    QueueDoc = couch_mgr:open_doc(AcctDb, QueueId),
    [
        {<<"Queue">>, wh_json:get_value(<<"name">>, QueueDoc)},
        {<<"Max">>, wh_json:get_value(<<"max_queue_size">>, QueueDoc)},
        {<<"Strategy">>, translate_strat(wh_json:get_value(<<"strategy">>, QueueDoc))},
        {<<"QueueStats">>, queue_stats(QueueId, AcctId)}
    ].
        
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
        
agent_status(AgentId, AcctId) ->
    AcctDb = wh_util:format_account_mod_id(AcctId),
    UserDoc = couch_mgr:open_doc(AcctDb, AgentId),
    FirstName = wh_json:get_value(<<"first_name">>, UserDoc),
    LastName = wh_json:get_value(<<"last_name">>, UserDoc),
    Username = wh_json:get_binary_value(<<"username">>, UserDoc),
    
    %% Ripped from cb_agents
    Now = wh_util:current_tstamp(),
    Yday = Now - ?SECONDS_IN_DAY,

    Opts = props:filter_undefined(
             [{<<"Status">>, undefined}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Start-Range">>, Yday}
              ,{<<"End-Range">>, Now}
              ,{<<"Most-Recent">>, true}
             ]),

    {'ok', Status} = acdc_agent_util:most_recent_statuses(AcctId, Opts),
    % TODO: properly assigned paused based on status
    Paused = case Status of
        undefined ->
            0;
        _ ->
            1
    end,
    
    AgentListener = acdc_agent_sup:listener(acdc_agents_sup:find_agent_supervisor(AcctId, AgentId)),
    LastCall = gen_listener:call(AgentListener, last_connect),
    
    %% Agent status payload
    [
        {<<"Event">>, <<"QueueMember">>},
        %{<<"Queue">>, proplists:get_value(<<"Queue">>, QueueDetails)},
        {<<"Name">>, <<FirstName/binary, " ", LastName/binary>>},
        {<<"Location">>, <<"Local/", Username/binary, "@from-queue/n">>},
        %% Membership static is also possible
        {<<"Membership">>, <<"dynamic">>},
        {<<"Penalty">>, 0},
        %% CallsTaken handled by count_stats function
        {<<"LastCall">>, LastCall},
        {<<"Status">>, translate_status(Status)},
        {<<"Paused">>, Paused}
    ].
    
translate_status(_Status) ->
    % TODO: properly translate statuses
    3.
    
originate(Props) ->
    CCVs = [{<<"Account-ID">>, proplists:get_value(<<"Account">>, Props)}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            %,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
            %,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
           ],
    MsgId = case proplists:get_value(<<"ActionID">>, Props) of
                undefined -> wh_util:rand_hex_binary(16);
                ActionID -> ActionID
            end,
    Request = [{<<"Application-Name">>, <<"blackhole-ami">>}
               %,{<<"Application-Data">>, get_application_data(Context)}
               ,{<<"Msg-ID">>, MsgId}
               %,{<<"Endpoints">>, maybe_auto_answer(Endpoints)}
               %,{<<"Timeout">>, get_timeout(Context)}
               ,{<<"Timeout">>, <<"30">>}
               %,{<<"Ignore-Early-Media">>, get_ignore_early_media(Context)}
               ,{<<"Ignore-Early-Media">>, <<"true">>}
               %,{<<"Media">>, get_media(Context)}
               ,{<<"Outbound-Caller-ID-Name">>, proplists:get_value(<<"Channel">>, Props)}
               %,{<<"Outbound-Caller-ID-Number">>, whapps_call:request_user(Call)}
               %,{<<"Outbound-Callee-ID-Name">>, get_caller_id_name(Context)}
               %,{<<"Outbound-Callee-ID-Number">>, get_caller_id_number(Context)}
               ,{<<"Outbound-Caller-ID-Number">>, proplists:get_value(<<"Channel">>, Props)}
               ,{<<"Outbound-Callee-ID-Name">>, proplists:get_value(<<"Exten">>, Props)}
               ,{<<"Outbound-Callee-ID-Number">>, proplists:get_value(<<"Exten">>, Props)}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
               %,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>]}
               | wh_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
              
    lager:debug("AMI: originate request ~p", [Request]),
    
    wapi_resource:publish_originate_req(props:filter_undefined(Request)).

%% Kazoo chan. status to AMI status translator - INCOMPLETE
% ami_channel_status(Call, Schema) ->
%     AMI_Status_Header = [
%         {<<"Event">>, <<"Status">>},
%         {<<"Priviledge">>, <<"Call">>}
%     ],
%     AMI_Status_Body = case Schema of
%         bridged_extension -> [
%             {<<"Channel">>, <<"SIP/", proplists:get_value(<<"username">>, Call)/binary, "@", proplists:get_value(<<"context">>, Call)/binary, ";1">>}
%             {<<"CallerIDNum">>, proplists:get_value(<<"destination">>, Call)},
%             {<<"CallerIDName">>, proplists:get_value(<<"account_id">>, Call)},
%             {<<"ConnectedLineNum">>, proplists:get_value(<<"account_id">>, Call)},
%             {<<"ConnectedLineName">>, proplists:get_value(<<"account_id">>, Call)},
%             {<<"Accountcode">>, proplists:get_value(<<"account_id">>, Call)},
%             {<<"ChannelState">>, <<"6">>}, % Numeric channel state
%             {<<"ChannelStateDesc">>, <<"Up">>}, % Check for "answered": true/false
%             {<<"Context">>, proplists:get_value(<<"context">>, Call)},
%             {<<"Extension">>, proplists:get_value(<<"username">>, Call)},
%             {<<"Priority">>, <<"12">>},
%             {<<"Seconds">>, proplists:get_value(<<"elapsed_s">>, Call)},
%             {<<"BridgedChannel">>, proplists:get_value(<<"other_leg">>, Call)},
%             {<<"BridgedUniqueid">>, proplists:get_value(<<"other_leg">>, Call)}
%         ];
%         extension -> [
%         [
%             {<<"Accountcode">>, proplists:get_value(<<"account_id">>, Call)}
%         ];
%         bridged -> [
%             {<<"Account">>, proplists:get_value(<<"account_id">>, Call)},
%             {<<"BridgedChannel">>, proplists:get_value(<<"uuid">>, Call)},
%             {<<"BridgedUniqueid">>, proplists:get_value(<<"other_leg">>, Call)}
%         ];
%         _ -> []
%     end,
%     AMI_Status_Footer = [
%         {<<"Uniqueid">>, proplists:get_value(<<"uuid">>, Call)}
%     ],
%     AMI_Status_Header ++ AMI_Status_Body ++ AMI_Status_Footer.