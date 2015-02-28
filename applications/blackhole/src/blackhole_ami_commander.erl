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
    _AMIDoc = couch_mgr:open_doc(?AMI_DB, Username),
    %case wh_json:get_value(<<"Secret">>, AMIDoc) of
    case Secret of
        undefined ->
            %% Successful login
            %{ok, wh_json:get_value(<<"account_id">>, AMIDoc)};
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
handle_event(Event, Props) ->
    lager:debug("AMI: commander no ~p action handler with props ~p", [Event, Props]),
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