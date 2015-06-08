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
	file:write_file(<<"/tmp/queue_log_raw">>, io_lib:fwrite("~p\n", [JObj]), [append]),
	Event = {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)},
	AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
	CallId = case wh_json:get_value(<<"Call-ID">>, JObj) of
		undefined -> wh_json:get_value(<<"Msg-ID">>, JObj);
		_ -> wh_json:get_value(<<"Call-ID">>, JObj) end,
	QueueId = case wh_json:get_value(<<"Queue-ID">>, JObj) of
		undefined -> "NONE";
		_ -> wh_json:get_value(<<"Queue-ID">>, JObj) end,
	QueueName = case QueueId of
		"NONE" -> "NONE";
		_ -> lookup_queue_name(AccountId, QueueId) end,
	BridgedChannel = case wh_json:get_value(<<"Agent-ID">>, JObj) of
		undefined -> "NONE";
		_ -> lookup_agent_name(AccountId, wh_json:get_value(<<"Agent-ID">>, JObj)) end,
	case Event of
		{<<"acdc_call_stat">>, <<"waiting">>} ->
			EventName = "ENTERQUEUE", % ENTERQUEUE(url|callerid)
			EventParams = {<<"">>, wh_json:get_value(<<"Caller-ID-Name">>, JObj)},
			lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
			write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

		{<<"acdc_call_stat">>, <<"abandoned">>} ->
			EventName = "ABANDON", % ABANDON(position|origposition|waittime)
			WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, acdc_stats:find_call(CallId))),
			OriginalPos = integer_to_list(wh_json:get_value(<<"Entered-Position">>, acdc_stats:find_call(CallId))),
			Position = integer_to_list(wh_json:get_value(<<"Exited-Position">>, acdc_stats:find_call(CallId))),
			EventParams = {Position, OriginalPos, WaitTime}, 
			lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
			write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

		{<<"acdc_call_stat">>, <<"missed">>} ->
			EventName = "RINGNOANSWER", % RINGNOANSWER(ringtime)
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

		{<<"acdc_call_stat">>, <<"handled">>} ->
			EventName = "CONNECT", % CONNECT(holdtime|bridgedchanneluniqueid)
			WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, acdc_stats:find_call(CallId))),
			EventParams = {WaitTime, wh_json:get_value(<<"Agent-ID">>, JObj)},
			lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
			write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

		{<<"acdc_call_stat">>, <<"processed">>} ->
			HungUpBy = wh_json:get_value(<<"Hung-Up-By">>, acdc_stats:find_call(CallId)),
			EventName = case HungUpBy of
				<<"member">> -> "COMPLETECALLER"; % COMPLETECALLER(holdtime|calltime|origposition)
				_ -> "COMPLETEAGENT" % COMPLETECALLER(holdtime|calltime|origposition)
			end,
			WaitTime = integer_to_list(wh_json:get_value(<<"Wait-Time">>, acdc_stats:find_call(CallId))),
			TalkTime = integer_to_list(wh_json:get_value(<<"Talk-Time">>, acdc_stats:find_call(CallId))),
			OriginalPos = integer_to_list(wh_json:get_value(<<"Entered-Position">>, acdc_stats:find_call(CallId))),
			EventParams = {WaitTime, TalkTime, OriginalPos},
			lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
			write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

		% {<<"">>, <<"">>} ->
			%EventName = "ADDMEMBER", % ADDMEMBER

		% {<<"">>, <<"">>} ->
			%EventName = "REMOVEMEMBER", % REMOVEMEMBER

		% {<<"">>, <<"">>} ->
			%EventName = "EXITEMPTY", % EXITEMPTY(position|origposition|waittime)

		% {<<"">>, <<"">>} ->
			%EventName = "EXITWITHTIMEOUT", % EXITWITHTIMEOUT(position)

		% {<<"">>, <<"">>} ->
			%EventName = "EXITWITHKEY", % EXITWITHKEY(key|position)

		% {<<"">>, <<"">>} ->
			%EventName = "TRANSFER", % TRANSFER(extension|context|holdtime|calltime)

		{<<"acdc_status_stat">>, <<"paused">>} -> 
			EventName = "PAUSEALL", 
			% There is no pause for a single queue, pause all by default
			lager:debug("writing event to queue_log: ~s", [EventName]),
			write_log(AccountId, CallId, QueueName, BridgedChannel, EventName);

		{<<"acdc_status_stat">>, <<"resume">>} -> 
			EventName = "UNPAUSEALL", 
			% There is no resume for a single queue, unpause all by default
			lager:debug("writing event to queue_log: ~s", [EventName]),
			write_log(AccountId, CallId, QueueName, BridgedChannel, EventName);

		{<<"acdc_status_stat">>, <<"logged_in">>} ->
			EventName = "AGENTLOGIN", % AGENTLOGIN(channel)
			AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
			ChannelName = lookup_agent_name(AccountId, AgentId),
			lists:foreach(fun(Q) -> % Agent will log in to all queues that they are a member of
				QueueName2 = case Q of
					"NONE" -> "NONE";
					_ -> lookup_queue_name(AccountId, Q)
				end,
				EventParams = {ChannelName}, 
				lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
				write_log(AccountId, CallId, QueueName2, BridgedChannel, EventName, EventParams)
			end, get_queue_list_by_agent_id(AccountId, AgentId));

		{<<"acdc_status_stat">>, <<"logged_out">>} ->
			EventName = "AGENTLOGOFF", % AGENTLOGOFF(channel|logintime)
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
				EventParams = {ChannelName, LoginTime},
				lager:debug("writing event to queue_log: ~s, ~p", [EventName, EventParams]),
				write_log(AccountId, CallId, QueueName2, BridgedChannel, EventName, EventParams)
			end, get_queue_list_by_agent_id(AccountId, AgentId));

		{_, _} ->
			lager:debug("unhandled event: ~p", [Event])
	end.

get_agent_login_timestamp(AccountId, AgentId, Default) ->
	Request = props:filter_undefined(
		[{<<"Account-ID">>, AccountId}
		 ,{<<"Agent-ID">>, AgentId}
		 ,{<<"Status">>, <<"logged_in">>}
		 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
		]),
	case whapps_util:amqp_pool_request(Request, fun wapi_acdc_stats:publish_status_req/1, fun wapi_acdc_stats:status_resp_v/1) of
		{ok, Result} ->
			lager:debug("got agent logged in status response: ~p", [Result]),
			Keys = wh_json:get_keys(wh_json:get_value([<<"Agents">>, AgentId], Result)),
			SortedKeys = lists:sort(fun(A, B) -> list_to_integer(binary_to_list(A)) =< list_to_integer(binary_to_list(B)) end, Keys),
			lists:last(SortedKeys);
		{error, E} ->
			lager:debug("agent login timestamp query failed: ~p", [E]),
			list_to_binary(integer_to_list(Default))
	end.

get_queue_list_by_agent_id(AccountId, AgentId) ->
	{ok, Result} = couch_mgr:get_results(wh_util:format_account_id(AccountId, encoded), <<"agents/crossbar_listing">>, [{'key', AgentId}]),
	wh_json:get_value([<<"value">>, <<"queues">>], hd(Result)).

lookup_agent_name(AccountId, AgentId) ->
	AccountDb = wh_util:format_account_id(AccountId, encoded),
	{ok, Result} = couch_mgr:open_doc(AccountDb, AgentId),
	<<"Local/", (wh_json:get_value(<<"username">>, Result))/binary, "@from-queue/n">>.

lookup_queue_name(AccountId, QueueId) ->
	case couch_mgr:get_results(wh_util:format_account_id(AccountId, encoded), <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
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
	lager:debug("queue_log event: ~p~p|~s|~s|~s|~s|", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event]),
	file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event]), [append]).

write_log(AccountId, CallId, QueueName, BridgedChannel, Event, EventParams) ->
	LogFile = "/tmp/queue_log." ++ binary_to_list(AccountId),
	{MegaSec, Sec, _} = os:timestamp(),
	case EventParams of
		{Param1, Param2, Param3} -> 
			lager:debug("fwrite params ~p~p|~s|~s|~s|~s|~s|~s|~s", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3]),
			file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|~s|~s|~s\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3]), [append]);
		{Param1, Param2} -> 
			lager:debug("fwrite params ~p~p|~s|~s|~s|~s|~s|~s", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1, Param2]),
			file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|~s|~s\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1, Param2]), [append]);
		{Param1} -> 
			lager:debug("fwrite params ~p~p|~s|~s|~s|~s|~s", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1]),
			file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|~s\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1]), [append]);
		_ -> 
			lager:debug("unexpected event parameters: ~p", [EventParams])
	end.
