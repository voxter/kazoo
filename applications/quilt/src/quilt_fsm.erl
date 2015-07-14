%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_fsm).

-behaviour(gen_fsm).

-include("quilt.hrl").

% -record(state, {}).

-export([start_link/1]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%
%% Public functions
%%

start_link(Call) ->
    gen_fsm:start_link(?MODULE, Call, []).

%%
%% gen_fsm callbacks
%%

init(CallId) ->
    {'ok', 'started', CallId}.

handle_event(Event, StateName, State) ->
    lager:debug("unhandled event in state ~s: ~p", [StateName, Event]),
    {'next_state', StateName, State}.

handle_sync_event({'enterqueue', JObj}, _From, 'started', State) ->
    quilt_log:handle_event(JObj),
    {'reply', 'ok', 'waiting', State};
handle_sync_event({'connected', JObj}, _From, 'waiting', State) ->
    quilt_log:handle_event(JObj),
    {'reply', 'ok', 'connected', State};
handle_sync_event(Event, _From, StateName, State) ->
    lager:debug("unhandled sync event in state ~s: ~p", [StateName, Event]),
    {'reply', 'ok', StateName, State}.

handle_info({'$gen_cast', _}, StateName, State) ->
    {'next_state', StateName, State};
handle_info(Info, StateName, State) ->
    lager:debug("unhandled info in state ~s: ~p", [StateName, Info]),
    {'next_state', StateName, State}.

terminate(Reason, StateName, _) ->
    lager:debug("terminating in state ~s (~s)", [StateName, Reason]).

code_change(_, StateName, State, _) ->
    {'ok', StateName, State}.

%%
%% gen_fsm states
%%



%%
%% private functions
%%

% maybe_queue_empty(AccountId, QueueId) ->
%     LoggedIn = lists:filter(fun(Agent) -> 
%         case acdc_agent_util:most_recent_status(AccountId, Agent) of
%             {'ok', <<"ready">>} -> 'true';
%             {'ok', <<"logged_in">>} -> 'true';
%             _ -> 'false'
%         end
%     end, acdc_util:agents_in_queue(wh_util:format_account_id(AccountId, 'encoded'), QueueId)),
%     case length(LoggedIn) of
%         0 -> 'true';
%         _ -> 'false'
%     end.

% get_agent_login_timestamp(AccountId, AgentId, Default) ->
%     Request = props:filter_undefined(
%         [{<<"Account-ID">>, AccountId}
%          ,{<<"Agent-ID">>, AgentId}
%          ,{<<"Status">>, <<"logged_in">>}
%          | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
%         ]),
%     case whapps_util:amqp_pool_request(Request, fun wapi_acdc_stats:publish_status_req/1, fun wapi_acdc_stats:status_resp_v/1) of
%         {'ok', Result} ->
%             lager:debug("got agent logged in status response: ~p", [Result]),
%             Keys = wh_json:get_keys(wh_json:get_value([<<"Agents">>, AgentId], Result)),
%             SortedKeys = lists:sort(fun(A, B) -> list_to_integer(binary_to_list(A)) =< list_to_integer(binary_to_list(B)) end, Keys),
%             lists:last(SortedKeys);
%         {'error', E} ->
%             lager:debug("agent login timestamp query failed: ~p", [E]),
%             list_to_binary(integer_to_list(Default))
%     end.

% get_queue_list_by_agent_id(AccountId, AgentId) ->
%     {'ok', Result} = couch_mgr:get_results(wh_util:format_account_id(AccountId, 'encoded'), <<"agents/crossbar_listing">>, [{'key', AgentId}]),
%     wh_json:get_value([<<"value">>, <<"queues">>], hd(Result)).

% % lookup_agent_name(AccountId, AgentId) ->
% %     AccountDb = wh_util:format_account_id(AccountId, encoded),
% %     {ok, Result} = couch_mgr:open_doc(AccountDb, AgentId),
% %     <<"Local/", (wh_json:get_value(<<"username">>, Result))/binary, "@from-queue/n">>.

% lookup_agent_name(AccountId, AgentId) ->
%     AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
%     {'ok', Result} = couch_mgr:open_doc(AccountDb, AgentId),
%     <<(wh_json:get_value(<<"first_name">>, Result))/binary, " ", (wh_json:get_value(<<"last_name">>, Result))/binary>>.

% lookup_queue_name(AccountId, QueueId) ->
%     case couch_mgr:get_results(wh_util:format_account_id(AccountId, 'encoded'), <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
%         {'error', E} ->
%             lager:debug("could not find queue number for queue ~p (~p)", [QueueId, E]),
%             "NONE";
%         {'ok', Results} when length(Results) =:= 1 ->
%             Value = wh_json:get_value(<<"value">>, hd(Results)),
%             lager:debug("found queue number ~p for queue id ~p", [Value, QueueId]),
%             hd(Value)
%     end.

% %% queue_log format: epoch_timestamp|unique_id_of_call|queue_name|bridged_channel|event_name|event_param_1|event_param_2|event_param_3
% write_log(AccountId, CallId, QueueName, BridgedChannel, Event) ->
%     LogFile = "/tmp/queue_log." ++ binary_to_list(AccountId),
%     {MegaSec, Sec, _} = os:timestamp(),
%     Timestamp = MegaSec * 1000000 + Sec,    
%     lager:debug("queue_log event: ~p|~s|~s|~s|~s|", [Timestamp, CallId, QueueName, BridgedChannel, Event]),
%     file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|\n", [Timestamp, CallId, QueueName, BridgedChannel, Event]), ['append']).

% write_log(AccountId, CallId, QueueName, BridgedChannel, Event, EventParams) ->
%     LogFile = "/tmp/queue_log." ++ binary_to_list(AccountId),
%     {MegaSec, Sec, _} = os:timestamp(),
%     Timestamp = MegaSec * 1000000 + Sec,
%     case EventParams of
%         {Param1, Param2, Param3, Param4, Param5} -> 
%             lager:debug("fwrite params ~p|~s|~s|~s|~s|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3, Param4, Param5]),
%             file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3, Param4, Param5]), ['append']);
%         {Param1, Param2, Param3, Param4} -> 
%             lager:debug("fwrite params ~p|~s|~s|~s|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3, Param4]),
%             file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3, Param4]), ['append']);
%         {Param1, Param2, Param3} -> 
%             lager:debug("fwrite params ~p|~s|~s|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3]),
%             file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2, Param3]), ['append']);
%         {Param1, Param2} -> 
%             lager:debug("fwrite params ~p|~s|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2]),
%             file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1, Param2]), ['append']);
%         {Param1} -> 
%             lager:debug("fwrite params ~p|~s|~s|~s|~s|~s", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1]),
%             file:write_file(LogFile, io_lib:fwrite("~p|~s|~s|~s|~s|~s\n", [Timestamp, CallId, QueueName, BridgedChannel, Event, Param1]), ['append']);
%         _ -> 
%             lager:debug("unexpected event parameters: ~p", [EventParams])
%     end.