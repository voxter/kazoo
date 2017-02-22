%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey, Daniel Finke
%%%-------------------------------------------------------------------
-module(quilt_log).

-export([handle_event/1]).

-include("quilt.hrl").

%%
%% public functions
%%

handle_event(JObj) ->
    Event = {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)},
    lager:debug("processing event: ~p, ~p", [Event, JObj]),
    handle_specific_event(Event, JObj).

%%
%% event-specific handlers
%%

handle_specific_event({<<"acdc_call_stat">>, <<"waiting">>}, JObj) ->
    EventName = "ENTERQUEUE", % ENTERQUEUE(url|callerid)
    {AccountId, CallId, _, QueueName, BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    CallerIdName = wh_json:get_value(<<"Caller-ID-Name">>, Call),
    EventParams = {<<"">>, CallerIdName},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);
    
handle_specific_event({<<"acdc_call_stat">>, <<"exited-position">>}, JObj) ->
    {AccountId, CallId, QueueId, QueueName, BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    lager:debug("exited queue call lookup: ~p", [Call]),
    Ev = {wh_json:get_value(<<"Status">>, Call), wh_json:get_value(<<"Abandoned-Reason">>, Call)},
    case Ev of
        {<<"abandoned">>, <<"member_hangup">>} -> 
            quilt_sup:stop_member_fsm(CallId),
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
            quilt_sup:stop_member_fsm(CallId),
            EventName = "EXITWITHTIMEOUT", % EXITWITHTIMEOUT(position)
            EventParams = {integer_to_list(wh_json:get_value(<<"Exited-Position">>, JObj))},
            lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
            write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);
        {<<"abandoned">>, <<"dtmf_exit">>} ->
            quilt_sup:stop_member_fsm(CallId),
            EventName = "EXITWITHKEY", % EXITWITHKEY(key|position)
            EventParams = {<<"#">>, integer_to_list(wh_json:get_value(<<"Exited-Position">>, JObj))},
            lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
            write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);
        {<<"handled">>, _} ->
            lager:debug("member was connected to an agent", []);
        {_, _} ->
            quilt_sup:stop_member_fsm(CallId),
            lager:debug("unhandled exited status/reason: ~p", [Ev])
    end;

handle_specific_event({<<"acdc_call_stat">>, <<"missed">>}, JObj) ->
    EventName = "RINGNOANSWER", % RINGNOANSWER(ringtime)
    {AccountId, CallId, _, QueueName, BridgedChannel} = get_common_props(JObj),
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
    {AccountId, CallId, _, QueueName, _BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    AgentId = wh_json:get_value(<<"Agent-ID">>, Call),
    AgentName = lookup_agent_name(AccountId, AgentId),
    WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, Call)),
    EventParams = {WaitTime, AgentId, CallId},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, AgentName, EventName, EventParams);

handle_specific_event({<<"call_event">>, <<"transfer">>}, JObj) ->
    EventName = "TRANSFER", % TRANSFER(extension|context|holdtime|calltime|position)
    Extension = wh_json:get_value(<<"Callee-ID-Number">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Call = acdc_stats:find_call(CallId),
    {AccountId, CallId, _, QueueName, BridgedChannel} = get_common_props(Call),
    WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, Call)),
    HandledTime = wh_json:get_integer_value(<<"Handled-Timestamp">>, Call),
    ProcessedTime = wh_json:get_integer_value(<<"Processed-Timestamp">>, Call),
    TalkTime = [ProcessedTime - HandledTime], %integer_to_list(wh_json:get_value(<<"Talk-Time">>, Call)), %% Talk-Time is not always available
    Position = integer_to_list(wh_json:get_value(<<"Exited-Position">>, Call)),
    EventParams = {Extension, <<"from-queue">>, WaitTime, TalkTime, Position},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

handle_specific_event({<<"acdc_call_stat">>, <<"processed">>}, JObj) ->
    {AccountId, CallId, _, QueueName, BridgedChannel} = get_common_props(JObj),
    Call = acdc_stats:find_call(CallId),
    HungUpBy = wh_json:get_value(<<"Hung-Up-By">>, Call),
    EventName = case HungUpBy of
        <<"member">> -> "COMPLETECALLER"; % COMPLETECALLER(holdtime|calltime|origposition)
        _ -> "COMPLETEAGENT" % COMPLETECALLER(holdtime|calltime|origposition)
    end,
    WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, Call)),
    HandledTime = wh_json:get_integer_value(<<"Handled-Timestamp">>, Call),
    ProcessedTime = wh_json:get_integer_value(<<"Processed-Timestamp">>, Call),
    TalkTime = [ProcessedTime - HandledTime], %integer_to_list(wh_json:get_value(<<"Talk-Time">>, Call)), %% Talk-Time is not always available
    OriginalPos = integer_to_list(wh_json:get_value(<<"Entered-Position">>, Call)),
    EventParams = {WaitTime, TalkTime, OriginalPos},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

handle_specific_event({<<"agent">>, <<"login_queue">>}, JObj) ->
    EventName = "ADDMEMBER", % ADDMEMBER
    {AccountId, CallId, _, QueueName, BridgedChannel} = get_common_props(JObj),
    EventParams = {<<"">>},         
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

handle_specific_event({<<"agent">>, <<"logout_queue">>}, JObj) ->
    EventName = "REMOVEMEMBER", % REMOVEMEMBER
    {AccountId, CallId, _, QueueName, BridgedChannel} = get_common_props(JObj),
    EventParams = {<<"">>},
    lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
    write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

handle_specific_event({<<"acdc_status_stat">>, <<"paused">>}, JObj) ->
    EventName = "PAUSE",
    {AccountId, CallId, _, _, BridgedChannel} = get_common_props(JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    lists:foreach(fun(Q) ->
        QueueName = lookup_queue_name(AccountId, Q),
        lager:debug("writing event to queue_log: ~s", [EventName]),
        write_log(AccountId, CallId, QueueName, BridgedChannel, EventName)
    end, get_queue_list_by_agent_id(AccountId, AgentId));

handle_specific_event({<<"acdc_status_stat">>, <<"resume">>}, JObj) ->
    EventName = "UNPAUSE",
    {AccountId, CallId, _, _, BridgedChannel} = get_common_props(JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    lists:foreach(fun(Q) ->
        QueueName = lookup_queue_name(AccountId, Q),
        lager:debug("writing event to queue_log: ~s", [EventName]),
        write_log(AccountId, CallId, QueueName, BridgedChannel, EventName)
    end, get_queue_list_by_agent_id(AccountId, AgentId));

handle_specific_event({<<"acdc_status_stat">>, <<"logged_in">>}, JObj) ->
    EventName = "AGENTLOGIN", % AGENTLOGIN(channel)
    {AccountId, CallId, _, _, BridgedChannel} = get_common_props(JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    ChannelName = lookup_agent_name(AccountId, AgentId),
    lists:foreach(fun(Q) -> % Agent will log in to all queues that they are a member of
        QueueName = lookup_queue_name(AccountId, Q),
        EventParams = {ChannelName}, 
        lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
        write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams)
    end, get_queue_list_by_agent_id(AccountId, AgentId));

handle_specific_event({<<"acdc_status_stat">>, <<"logged_out">>}, JObj) ->
    EventName = "AGENTLOGOFF", % AGENTLOGOFF(channel|logintime)
    {AccountId, CallId, _, _, BridgedChannel} = get_common_props(JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    ChannelName = lookup_agent_name(AccountId, AgentId),
    LogoutTimestamp = wh_json:get_value(<<"Timestamp">>, JObj),
    LoginTimestamp = list_to_integer(binary_to_list(get_agent_login_timestamp(AccountId, AgentId, LogoutTimestamp))),
    LoginTime = list_to_binary(integer_to_list(LogoutTimestamp - LoginTimestamp)),
    lists:foreach(fun(Q) -> % Agent will log out of all queues that they are a member of
        QueueName = lookup_queue_name(AccountId, Q),
        EventParams = {ChannelName, LoginTime},
        lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
        write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams)
    end, get_queue_list_by_agent_id(AccountId, AgentId));

handle_specific_event(Event, _JObj) -> lager:debug("unhandled event: ~p", [Event]).

%%
%% private functions
%%

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

maybe_queue_empty(AccountId, QueueId) ->
    LoggedIn = lists:filter(fun(Agent) -> 
        case acdc_agent_util:most_recent_status(AccountId, Agent) of
            {'ok', <<"logged_out">>} -> 'false';
            _ -> 'true'
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
            lager:debug("could not find queue number for queue ~p (~p), account ~p", [QueueId, E, AccountId]),
            "NONE";
        {'ok', []} ->
            lager:debug("could not find queue number for queue ~p (ok), account ~p", [QueueId, AccountId]),
            "NONE";
        {'ok', Results} when length(Results) =:= 1 ->
            Value = wh_json:get_value(<<"value">>, hd(Results)),
            lager:debug("found queue number ~p for queue id ~p, account ~p", [Value, QueueId, AccountId]),
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
