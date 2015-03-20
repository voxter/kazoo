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
			lager:debug("QUILT: writing event tp queue_log: ~s", [EventName]),
			write_log(AccountId, CallId, QueueId, BridgedChannel, EventName);
		{<<"member">>, <<"call_cancel">>} ->
			EventName = "RINGNOANSWER",
			lager:debug("QUILT: writing event tp queue_log: ~s", [EventName]),
			write_log(AccountId, CallId, QueueId, BridgedChannel, EventName);
		% {<<"queue">>, <<"agent_change"} ->
		{_, _} ->
			lager:debug("QUILT: unhandled event: ~p", [JObj]),
			lager:debug("QUILT: unhandled event props: ~p", [Props])
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
