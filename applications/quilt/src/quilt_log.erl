%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey, Daniel Finke
%%%-------------------------------------------------------------------
-module(quilt_log).

-export([handle_event/2]).

-include("quilt.hrl").

handle_event(JObj, _Props) ->
    file:write_file(<<"/tmp/queue_log_raw">>, io_lib:fwrite("~p\n", [JObj]), ['append']),
    Event = {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)},
    handle_specific_event(Event, JObj).

get_common_props(JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    CallId = case wh_json:get_value(<<"Call-ID">>, JObj) of
        'undefined' -> wh_json:get_value(<<"Msg-ID">>, JObj);
        _ -> wh_json:get_value(<<"Call-ID">>, JObj) end,
    QueueId = case wh_json:get_value(<<"Queue-ID">>, JObj) of
        'undefined' -> "NONE";
        _ -> wh_json:get_value(<<"Queue-ID">>, JObj) end,
    QueueName = case QueueId of
        "NONE" -> "NONE";
        _ -> lookup_queue_name(AccountId, QueueId) end,
    BridgedChannel = case wh_json:get_value(<<"Agent-ID">>, JObj) of
        'undefined' -> "NONE";
        _ -> lookup_agent_name(AccountId, wh_json:get_value(<<"Agent-ID">>, JObj)) end,
    {AccountId, CallId, QueueId, QueueName, BridgedChannel}.

handle_specific_event({<<"acdc_call_stat">>, <<"waiting">>}, JObj) ->
    EventName = "ENTERQUEUE", % ENTERQUEUE(url|callerid)
    {AccountId, CallId, _QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    CallerIdName = wh_json:get_value(<<"Caller-ID-Name">>, Call),
    EventParams = {<<"">>, CallerIdName},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);
    
handle_specific_event({<<"acdc_call_stat">>, <<"exited-position">>}, JObj) ->
    {AccountId, CallId, QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    lager:debug("acdc call stats: ~p", [Call]),
    Ev = {wh_json:get_value(<<"Status">>, Call), wh_json:get_value(<<"Abandoned-Reason">>, Call)},
    case Ev of
        {<<"abandoned">>, <<"member_hangup">>} -> 
            % Check for agent count in queue (could be empty)
            case maybe_queue_empty(AccountId, QueueId) of
                'true' -> EventName = "EXITEMPTY"; % EXITEMPTY(position|origposition|waittime)
                _ -> EventName = "ABANDON" % ABANDON(position|origposition|waittime)
            end,
            WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, Call)),
            OriginalPos = integer_to_list(wh_json:get_value(<<"Entered-Position">>, Call)),
            Position = integer_to_list(wh_json:get_value(<<"Exited-Position">>, JObj)),
            EventParams = {Position, OriginalPos, WaitTime},
            lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
            write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

        {<<"abandoned">>, <<"member_timeout">>} -> 
            EventName = "EXITWITHTIMEOUT", % EXITWITHTIMEOUT(position)
            EventParams = {integer_to_list(wh_json:get_value(<<"Exited-Position">>, JObj))},
            lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
            write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

        {<<"abandoned">>, <<"dtmf_exit">>} ->
            EventName = "EXITWITHKEY", % EXITWITHKEY(key|position)
            EventParams = {<<"#">>, integer_to_list(wh_json:get_value(<<"Exited-Position">>, JObj))},
            lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
            write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);
            
        {_, _} ->
            lager:debug("unhandled exited status/reason: ~p", [Ev])
    end;

handle_specific_event({<<"acdc_call_stat">>, <<"missed">>}, JObj) ->
    EventName = "RINGNOANSWER", % RINGNOANSWER(ringtime)
    {AccountId, CallId, _QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    [Missed|Misses] = wh_json:get_value(<<"Misses">>, Call),
    MissedTimestamp = wh_json:get_integer_value(<<"timestamp">>, Missed),
    RingStartTimestamp = case length(Misses) of
        % Get ring time from start of call
        0 -> wh_json:get_integer_value(<<"Entered-Timestamp">>, Call);
        % Ring time is difference of current missed timestamp and last missed timestamp
        _ -> wh_json:get_integer_value(<<"timestamp">>, hd(Misses))
    end,
    RingTime = integer_to_list( (MissedTimestamp - RingStartTimestamp) * 1000 ),
    EventParams = {RingTime},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

handle_specific_event({<<"acdc_call_stat">>, <<"handled">>}, JObj) ->
    EventName = "CONNECT", % CONNECT(holdtime|bridgedchanneluniqueid)
    {AccountId, CallId, _QueueId, QueueName, _BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    AgentId = wh_json:get_value(<<"Agent-ID">>, Call),
    AgentName = lookup_agent_name(AccountId, AgentId),
    WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, Call)),
    lager:debug("updating call state to: ~p", [{EventName, CallId}]),
    quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
    quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]), {EventName, CallId}),
    EventParams = {WaitTime, AgentId, CallId},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, AgentName, EventName, EventParams);

handle_specific_event({<<"call_event">>,<<"CHANNEL_BRIDGE">>}, JObj) ->
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Call = acdc_stats:find_call(CallId),
    AgentId = case Call of
        undefined -> wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], JObj);
        _ -> wh_json:get_value(<<"Agent-ID">>, Call)
    end,
    lager:debug("detected bridge, checking for transfer (account: ~p, agent: ~p, call-id: ~p)", [AccountId, AgentId, CallId]),
    StoredState = quilt_store:get(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
    case StoredState of
        undefined -> lager:debug("unable to find any existing stored state for this call, ignoring...", []);
        {"TRANSFERRED", CallId} -> % Call-ID matches a transferred call, log TRANSFER event
            EventName = "TRANSFER", % TRANSFER(extension|context|holdtime|calltime|position)
            {_, _, _, QueueName, BridgedChannel} = get_common_props(Call),
            quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
            Extension = wh_json:get_value(<<"Callee-ID-Number">>, JObj),
            WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, Call)),
            TalkTime = integer_to_list(wh_json:get_value(<<"Talk-Time">>, Call)),
            Position = integer_to_list(wh_json:get_value(<<"Exited-Position">>, Call)),
            EventParams = {Extension, <<"from-queue">>, WaitTime, TalkTime, Position},
            lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
            write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);
        {"CONNECT", StoredCallId} -> % Agent is connected to a queue member/caller, transition to OUTBOUND state
            case wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj) of
                StoredCallId -> % Member to agent bridge
                    lager:debug("ignoring member to agent bridge: ~p", [StoredState]);
                _ ->
                    lager:debug("updating call state to: ~p", [{"OUTBOUND", StoredCallId}]),
                    quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
                    quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]), {"OUTBOUND", StoredCallId})
            end;
        {"OUTBOUND", StoredCallId} ->
            case wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj) of
                StoredCallId -> % Member to agent bridge
                    lager:debug("ignoring member to agent bridge: ~p", [StoredState]);
                _ -> % Unexpected state detected
                    lager:debug("unexpected state detected: ~p", [StoredState])
            end;
        _ -> % Orphaned state detected
            lager:debug("orphaned state detected, removing: ~p", [StoredState]),
            quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]))
    end;

handle_specific_event({<<"acdc_status_stat">>, <<"wrapup">>}, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    StoredState = quilt_store:get(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
    lager:debug("agent wrapup, checking state for call transfer: ~p", [StoredState]),
    case StoredState of
        {"OUTBOUND", StoredCallId} ->
            lager:debug("acdc call stats: ~p", [acdc_stats:find_call(StoredCallId)]),
            lager:debug("updating state to: ~p", [{"TRANSFERRED", StoredCallId}]),
            quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
            quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]), {"TRANSFERRED", StoredCallId});
        _ -> 
            lager:debug("unhandled wrapup state: ~p", [StoredState])
    end;

handle_specific_event({<<"acdc_call_stat">>, <<"processed">>}, JObj) ->
    {AccountId, CallId, _QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    HungUpBy = wh_json:get_value(<<"Hung-Up-By">>, Call),
    EventName = case HungUpBy of
        <<"member">> -> "COMPLETECALLER"; % COMPLETECALLER(holdtime|calltime|origposition)
        _ -> "COMPLETEAGENT" % COMPLETECALLER(holdtime|calltime|origposition)
    end,
    WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, Call)),
    TalkTime = integer_to_list(wh_json:get_value(<<"Talk-Time">>, Call)),
    OriginalPos = integer_to_list(wh_json:get_value(<<"Entered-Position">>, Call)),
    EventParams = {WaitTime, TalkTime, OriginalPos},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

handle_specific_event({<<"agent">>, <<"login_queue">>}, JObj) ->
    EventName = "ADDMEMBER", % ADDMEMBER
    {AccountId, CallId, _QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    EventParams = {<<"">>},         
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

handle_specific_event({<<"agent">>, <<"logout_queue">>}, JObj) ->
    EventName = "REMOVEMEMBER", % REMOVEMEMBER
    {AccountId, CallId, _QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    EventParams = {<<"">>},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

handle_specific_event({<<"acdc_status_stat">>, <<"paused">>}, JObj) ->
    EventName = "PAUSEALL", 
    {AccountId, CallId, _QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    % There is no pause for a single queue, pause all by default
    lager:debug("writing event to queue_log: ~s", [EventName]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName);

handle_specific_event({<<"acdc_status_stat">>, <<"resume">>}, JObj) ->
    EventName = "UNPAUSEALL", 
    {AccountId, CallId, _QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    % There is no resume for a single queue, unpause all by default
    lager:debug("writing event to queue_log: ~s", [EventName]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName);

handle_specific_event({<<"acdc_status_stat">>, <<"logged_in">>}, JObj) ->
    EventName = "AGENTLOGIN", % AGENTLOGIN(channel)
    {AccountId, CallId, _QueueId, _QueueName, BridgedChannel} = get_common_props(JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    ChannelName = lookup_agent_name(AccountId, AgentId),
    lists:foreach(fun(Q) -> % Agent will log in to all queues that they are a member of
        QueueName2 = case Q of
            "NONE" -> "NONE";
            _ -> lookup_queue_name(AccountId, Q)
        end,
        quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId, <<"-">>, Q])),
        EventParams = {ChannelName}, 
        lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
        write_log(AccountId, CallId, QueueName2, BridgedChannel, EventName, EventParams)
    end, get_queue_list_by_agent_id(AccountId, AgentId));

handle_specific_event({<<"acdc_status_stat">>, <<"logged_out">>}, JObj) ->
    EventName = "AGENTLOGOFF", % AGENTLOGOFF(channel|logintime)
    {AccountId, CallId, _QueueId, _QueueName, BridgedChannel} = get_common_props(JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    ChannelName = lookup_agent_name(AccountId, AgentId),
    LogoutTimestamp = wh_json:get_value(<<"Timestamp">>, JObj),
    LoginTimestamp = list_to_integer(binary_to_list(get_agent_login_timestamp(AccountId, AgentId, LogoutTimestamp))),
    LoginTime = list_to_binary(integer_to_list(LogoutTimestamp - LoginTimestamp)),
    lists:foreach(fun(Q) -> % Agent will log out of all queues that they are a member of
        QueueName2 = case Q of
            "NONE" -> "NONE";
            _ -> lookup_queue_name(AccountId, Q)
        end,
        case quilt_store:get(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId, <<"-">>, Q])) of 
            'undefined' -> % No previous record of logout
                quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId, <<"-">>, Q]), EventName),
                EventParams = {ChannelName, LoginTime},
                lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
                write_log(AccountId, CallId, QueueName2, BridgedChannel, EventName, EventParams);
            _ -> lager:debug("suppressing duplicate agent logoff event", [])
        end
    end, get_queue_list_by_agent_id(AccountId, AgentId), JObj);

handle_specific_event(Event, _JObj) -> lager:debug("unhandled event: ~p", [Event]).

maybe_queue_empty(AccountId, QueueId) ->
    LoggedIn = lists:filter(fun(Agent) -> 
        case acdc_agent_util:most_recent_status(AccountId, Agent) of
            {'ok', <<"ready">>} -> 'true';
            {'ok', <<"logged_in">>} -> 'true';
            _ -> 'false'
        end
    end, acdc_util:agents_in_queue(wh_util:format_account_id(AccountId, 'encoded'), QueueId)),
    case length(LoggedIn) of
        0 -> 'true';
        _ -> 'false'
    end.

get_agent_login_timestamp(AccountId, AgentId, Default) ->
    Request = props:filter_undefined(
        [{<<"Account-ID">>, AccountId}
         ,{<<"Agent-ID">>, AgentId}
         ,{<<"Status">>, <<"logged_in">>}
         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
        ]),
    case whapps_util:amqp_pool_request(Request, fun wapi_acdc_stats:publish_status_req/1, fun wapi_acdc_stats:status_resp_v/1) of
        {'ok', Result} ->
            lager:debug("got agent logged in status response: ~p", [Result]),
            Keys = wh_json:get_keys(wh_json:get_value([<<"Agents">>, AgentId], Result)),
            SortedKeys = lists:sort(fun(A, B) -> list_to_integer(binary_to_list(A)) =< list_to_integer(binary_to_list(B)) end, Keys),
            lists:last(SortedKeys);
        {'error', E} ->
            lager:debug("agent login timestamp query failed: ~p", [E]),
            list_to_binary(integer_to_list(Default))
    end.

get_queue_list_by_agent_id(AccountId, AgentId) ->
    {'ok', Result} = couch_mgr:get_results(wh_util:format_account_id(AccountId, 'encoded'), <<"agents/crossbar_listing">>, [{'key', AgentId}]),
    wh_json:get_value([<<"value">>, <<"queues">>], hd(Result)).

% lookup_agent_name(AccountId, AgentId) ->
%     AccountDb = wh_util:format_account_id(AccountId, encoded),
%     {ok, Result} = couch_mgr:open_doc(AccountDb, AgentId),
%     <<"Local/", (wh_json:get_value(<<"username">>, Result))/binary, "@from-queue/n">>.

lookup_agent_name(AccountId, AgentId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', Result} = couch_mgr:open_doc(AccountDb, AgentId),
    <<(wh_json:get_value(<<"first_name">>, Result))/binary, " ", (wh_json:get_value(<<"last_name">>, Result))/binary>>.

lookup_queue_name(AccountId, QueueId) ->
    case couch_mgr:get_results(wh_util:format_account_id(AccountId, 'encoded'), <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
        {'error', E} ->
            lager:debug("could not find queue number for queue ~p (~p)", [QueueId, E]),
            "NONE";
        {'ok', Results} when length(Results) =:= 1 ->
            Value = wh_json:get_value(<<"value">>, hd(Results)),
            lager:debug("found queue number ~p for queue id ~p", [Value, QueueId]),
            hd(Value)
    end.

%% queue_log format: epoch_timestamp|unique_id_of_call|queue_name|bridged_channel|event_name|event_param_1|event_param_2|event_param_3
write_log(AccountId, CallId, QueueName, BridgedChannel, Event) ->
    LogFile = "/tmp/queue_log." ++ binary_to_list(AccountId),
    {MegaSec, Sec, _} = os:timestamp(),
    Timestamp = MegaSec * 1000000 + Sec,    
    lager:debug("queue_log event: ~p|~s|~s|~s|~s|", [Timestamp, CallId, QueueName, BridgedChannel, Event]),
    file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|\n", [Timestamp, CallId, QueueName, BridgedChannel, Event]), ['append']).

write_log(AccountId, CallId, QueueName, BridgedChannel, Event, EventParams) ->
    LogFile = "/tmp/queue_log." ++ binary_to_list(AccountId),
    {MegaSec, Sec, _} = os:timestamp(),
    Timestamp = MegaSec * 1000000 + Sec,
    case EventParams of
        {Param1, Param2, Param3, Param4, Param5} -> 
            lager:debug("fwrite params ~p|~s|~s|~s|~s|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3, Param4, Param5]),
            file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3, Param4, Param5]), ['append']);
        {Param1, Param2, Param3, Param4} -> 
            lager:debug("fwrite params ~p|~s|~s|~s|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3, Param4]),
            file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3, Param4]), ['append']);
        {Param1, Param2, Param3} -> 
            lager:debug("fwrite params ~p|~s|~s|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3]),
            file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3]), ['append']);
        {Param1, Param2} -> 
            lager:debug("fwrite params ~p|~s|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2]),
            file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2]), ['append']);
        {Param1} -> 
            lager:debug("fwrite params ~p|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1]),
            file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1]), ['append']);
        _ -> 
            lager:debug("unexpected event parameters: ~p", [EventParams])
    end.
