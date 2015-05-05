%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_log).

-export([handle_event/2]).

-include("quilt.hrl").

handle_event(JObj, _Props) ->
	case wh_json:get_value(<<"App-Name">>, JObj) of
		<<"crossbar">> ->
			ok; % ignore crossbar
		_ ->
			case wh_json:get_value(<<"Event-Name">>, JObj) of
				<<"status_req">> -> ok;
				_ ->
					% lager:debug("QUILT: processing event object: ~p", [JObj]),
					% lager:debug("QUILT: associated event props: ~p", [Props]),
					file:write_file(<<"/tmp/queue_log_raw">>, io_lib:fwrite("~p\n", [JObj]), [append]),
					Event = {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)},
					AccountId = binary_to_list(wh_json:get_value(<<"Account-ID">>, JObj)),
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
							lager:debug("QUILT: writing event to queue_log: ~s, ~p", [EventName, EventParams]),
							write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

						{<<"member">>, <<"call_cancel">>} ->
							EventName = "ABANDON", % ABANDON(position|origposition|waittime)
							EventParams = {<<"1">>, <<"5">>, <<"45">>},
							lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
							write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

						{<<"acdc_call_stat">>, <<"missed">>} ->
							EventName = "RINGNOANSWER", % RINGNOANSWER(ringtime)
							lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
							write_log(AccountId, CallId, QueueName, BridgedChannel, EventName);

						{<<"acdc_call_stat">>, <<"handled">>} ->
							EventName = "CONNECT", % CONNECT(holdtime|bridgedchanneluniqueid)
							EventParams = {<<"10">>, wh_json:get_value(<<"Agent-ID">>, JObj)},
							lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
							write_log(AccountId, CallId, QueueName, BridgedChannel, EventName, EventParams);

						% {<<"">>, <<"">>} ->
							%EventName = "COMPLETEAGENT", % COMPLETEAGENT(holdtime|calltime|origposition)

						% {<<"">>, <<"">>} ->
							%EventName = "COMPLETECALLER", % COMPLETECALLER(holdtime|calltime|origposition)

						% {<<"">>, <<"">>} ->
							%EventName = "ADDMEMBER", % ADDMEMBER(?)

						% {<<"">>, <<"">>} ->
							%EventName = "REMOVEMEMBER", % REMOVEMEMBER(?)

						% {<<"">>, <<"">>} ->
							%EventName = "EXITEMPTY", % EXITEMPTY(position|origposition|waittime)

						% {<<"">>, <<"">>} ->
							%EventName = "TRANSFER", % TRANSFER(extension|context|holdtime|calltime)

						{<<"agent">>, <<"pause">>} -> 
							EventName = "PAUSEALL", 
							% There is no pause for a single queue, pause all by default
							lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
							write_log(AccountId, CallId, QueueName, BridgedChannel, EventName);

						{<<"agent">>, <<"resume">>} -> 
							EventName = "UNPAUSEALL", 
							% There is no resume for a single queue, unpause all by default
							lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
							write_log(AccountId, CallId, QueueName, BridgedChannel, EventName);

						{<<"acdc_status_stat">>, <<"logged_in">>} ->
							EventName = "AGENTLOGIN", % AGENTLOGIN(channel)
							% Agent will log into all queues that they are a member of
							AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
							lists:foreach(fun(Q) ->
								QueueName2 = case Q of
									"NONE" -> "NONE";
									_ -> lookup_queue_name(AccountId, Q)
								end,
								EventParams = {AgentId}, % TODO: add logintime param
								lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
								write_log(AccountId, CallId, QueueName2, BridgedChannel, EventName, EventParams)
							end, get_queue_list_by_agent_id(AccountId, AgentId));

						{<<"acdc_status_stat">>, <<"logged_out">>} ->
							EventName = "AGENTLOGOFF", % AGENTLOGOFF(channel|logintime)
							% Agent will log into all queues that they are a member of
							AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
							lists:foreach(fun(Q) ->
								QueueName2 = case Q of
									"NONE" -> "NONE";
									_ -> lookup_queue_name(AccountId, Q)
								end,
								EventParams = {AgentId}, % TODO: add logintime param
								lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
								write_log(AccountId, CallId, QueueName2, BridgedChannel, EventName, EventParams)
							end, get_queue_list_by_agent_id(AccountId, AgentId));

						{_, _} ->
							lager:debug("QUILT: unhandled event: ~p", [Event])
					end
			end
	end.

get_queue_list_by_agent_id(AccountId, AgentId) ->
	{ok, Result} = couch_mgr:get_results(wh_util:format_account_id(AccountId, encoded), <<"agents/crossbar_listing">>, [{'key', AgentId}]),
	wh_json:get_value([<<"value">>, <<"queues">>], hd(Result)).

lookup_agent_name(AccountId, AgentId) ->
	{ok, Result} = couch_mgr:get_results(wh_util:format_account_id(AccountId, encoded), <<"agents/crossbar_listing">>, [{'key', AgentId}]),
	FirstName = wh_json:get_value([<<"value">>, <<"first_name">>], hd(Result)),
	LastName = wh_json:get_value([<<"value">>, <<"last_name">>], hd(Result)),
	iolist_to_binary([FirstName, <<" ">>, LastName]).

lookup_queue_name(AccountId, QueueId) ->
	case couch_mgr:get_results(wh_util:format_account_id(AccountId, encoded), <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
		{'error', E} ->
			lager:debug("Could not find queue number for queue ~p (~p)", [QueueId, E]),
			"NONE";
		{'ok', Results2} when length(Results2) =:= 1 ->
			Value = wh_json:get_value(<<"value">>, hd(Results2)),
			hd(Value)
	end.

%% queue_log format: epoch_timestamp|unique_id_of_call|queue_name|bridged_channel|event_name|event_param_1|event_param_2|event_param_3
write_log(AccountId, CallId, QueueName, BridgedChannel, Event) ->
	LogFile = "/tmp/queue_log." ++ AccountId,
	{MegaSec, Sec, _} = os:timestamp(),
	lager:debug("QUILT: queue_log event: ~p~p|~s|~s|~s|~s|", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event]),
	file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event]), [append]).

write_log(AccountId, CallId, QueueName, BridgedChannel, Event, EventParams) ->
	LogFile = "/tmp/queue_log." ++ AccountId,
	{MegaSec, Sec, _} = os:timestamp(),
	case EventParams of
		{Param1, Param2, Param3} -> 
			file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|~s|~s|~s\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3]), [append]);
		{Param1, Param2} -> 
			file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|~s|~s\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1, Param2]), [append]);
		{Param1} -> 
			file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|~s\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1]), [append]);
		_ -> 
			lager:debug("QUILT: unexpected event parameters: ~p", [EventParams])
	end.
