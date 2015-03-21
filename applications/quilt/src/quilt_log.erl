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

handle_event(JObj, Props) ->
	lager:debug("QUILT: processing event object: ~p", [JObj]),
	lager:debug("QUILT: associated event props: ~p", [Props]),
	file:write_file(<<"/tmp/queue_log_raw">>, io_lib:fwrite("~p\n", [JObj])),
	Event = {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)},
	AccountId = binary_to_list(wh_json:get_value(<<"Account-ID">>, JObj)),
	CallId = case wh_json:get_value(<<"Call-ID">>, JObj) of
		undefined -> "NONE";
		% Call/Key-Value-Store/Call-ID ->
		_ -> binary_to_list(wh_json:get_value(<<"Call-ID">>, JObj)) end,
	QueueId = case wh_json:get_value(<<"Queue-ID">>, JObj) of
		undefined -> "NONE";
		_ -> binary_to_list(wh_json:get_value(<<"Queue-ID">>, JObj)) end,
	BridgedChannel = undefined,
	case Event of
		{<<"member">>, <<"call">>} ->
			EventName = "ENTERQUEUE",
			lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
			write_log(AccountId, CallId, QueueId, BridgedChannel, EventName);
		{<<"member">>, <<"call_cancel">>} ->
			EventName = "RINGNOANSWER",
			lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
			write_log(AccountId, CallId, QueueId, BridgedChannel, EventName);
		{<<"agent">>, <<"pause">>} -> 
			EventName = "PAUSEALL", % There is no pause for a single queue, pause all by default
			
			lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
			write_log(AccountId, CallId, QueueId, BridgedChannel, EventName);
		{<<"agent">>, <<"resume">>} -> 
			EventName = "UNPAUSEALL", % There is no resume for a single queue, unpause all by default
			lager:debug("QUILT: raw event object: ~p", [JObj]),
			lager:debug("QUILT: writing event to queue_log: ~s", [EventName]),
			write_log(AccountId, CallId, QueueId, BridgedChannel, EventName);
		% {<<"queue">>, <<"agent_change"} ->
		{_, _} ->
			lager:debug("QUILT: unhandled event", [])
	end.

write_log(AccountId, CallId, QueueName, BridgedChannel, Event) ->
	LogFile = "/tmp/queue_log." ++ AccountId,
	{MegaSec, Sec, _} = os:timestamp(),
	lager:debug("QUILT: queue_log event: ~p~p|~s|~s|~s|~s|", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event]),
	file:write_file(LogFile, io_lib:fwrite("~p~p|~s|~s|~s|~s|\n", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event]), [append]).

% write_log(AccountId, CallId, QueueName, BridgedChannel, Event, EventParams) ->
% 	LogFile = "/tmp/queue_log.", AccountId,
% 	{MegaSec, Sec, _} = os:timestamp(),
% 	case EventParams of
% 		{Param1, Param2, Param3} -> 
% 			file:write_file(LogFile, io_lib:fwrite("~p~p|~p|~p|~p|~p|~p|~p|~p", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3]), [append]);
% 		{Param1, Param2} -> 
% 			file:write_file(LogFile, io_lib:fwrite("~p~p|~p|~p|~p|~p|~p|~p", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1, Param2]), [append]);
% 		{Param1} -> 
% 			file:write_file(LogFile, io_lib:fwrite("~p~p|~p|~p|~p|~p|~p", [MegaSec, Sec, CallId, QueueName, BridgedChannel, Event, Param1]), [append]);
% 		_ -> 
% 			lager:debug("QUILT: unexpected event parameters: ~p", [EventParams])
% 	end.
