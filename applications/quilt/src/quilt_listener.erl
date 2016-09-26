%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_listener).
-behaviour(gen_listener).

-export([start_link/0
         ,handle_quilt_event/2
        ]).
-export([init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,handle_event/2
    ,terminate/2
    ,code_change/3
    ]).

-include("quilt.hrl").

-record(state, {}).

-define(BINDINGS, [
    {'self', []}
    ,{'acdc_agent', []}
    ,{'acdc_stats', [
        {'restrict_to', ['call_stat', 'status_stat']}
    ]}
    ,{'call', [
        {'restrict_to', [<<"CHANNEL_BRIDGE">>, <<"CHANNEL_DESTROY">>]}
    ]}
]).

-define(RESPONDERS, [{
    {?MODULE, 'handle_quilt_event'}, [
        {<<"acdc_call_stat">>, <<"waiting">>}
        ,{<<"acdc_call_stat">>, <<"abandoned">>}
        ,{<<"acdc_call_stat">>, <<"missed">>}
        ,{<<"acdc_call_stat">>, <<"handled">>}
        ,{<<"acdc_call_stat">>, <<"exited-position">>}
        ,{<<"acdc_call_stat">>, <<"processed">>}
        ,{<<"acdc_status_stat">>, <<"wrapup">>}
        ,{<<"acdc_status_stat">>, <<"logged_in">>}
        ,{<<"acdc_status_stat">>, <<"logged_out">>}
        ,{<<"acdc_status_stat">>, <<"paused">>}
        ,{<<"acdc_status_stat">>, <<"resume">>}
        ,{<<"agent">>, <<"login_queue">>}
        ,{<<"agent">>, <<"logout_queue">>}
        ,{<<"call_event">>, <<"CHANNEL_BRIDGE">>}
        ,{<<"call_event">>, <<"CHANNEL_DESTROY">>}
    ]
}]).

start_link() ->
    gen_listener:start_link(?MODULE, [
        {'bindings', ?BINDINGS},
        {'responders', ?RESPONDERS}
    ], []).

-spec handle_quilt_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_quilt_event(JObj, _Props) ->
    handle_specific_event(wh_json:get_value(<<"Event-Name">>, JObj), JObj).

init([]) -> 
    {'ok', #state{}}.

handle_call(Request, _From, State) ->
    lager:debug("unhandled call: ~p", [Request]),
    {'reply', {'error', 'not_implemented'}, State}.

handle_cast(Msg, State) ->
    lager:debug("unhandled cast: ~p", [Msg]),
    {'noreply', State}.

handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, 'shutdown'} ->
            lager:debug("~p shutting down...", [Pid]),
            exit(Pid, "shutting down...");
        _ ->
            lager:debug("unhandled info: ~p", [Info])
    end,
    {'noreply', State}.

handle_event(_JObj, _State) ->
    {'reply', []}.

terminate(Reason, _State) ->
    lager:debug("~p listener on pid ~p terminating: ~p", [?MODULE, self(), Reason]),
    'ok'.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%
%% event-specific handlers
%%

handle_specific_event(<<"waiting">>, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case quilt_sup:retrieve_member_fsm(CallId) of
        {'error', 'not_found'} ->
            {'ok', FSM} = quilt_sup:start_member_fsm(CallId),
            lager:debug("started FSM: ~p for call id: ~p", [FSM, CallId]),
            gen_fsm:sync_send_all_state_event(FSM, {'enterqueue', JObj});
        {'ok', FSM} ->
            lager:debug("FSM ~p already created for this call id: ~p", [FSM, CallId]);
        Else ->
            lager:debug("unexpected return value when looking up FSM: ~p", [Else])
    end;

handle_specific_event(<<"abandoned">>, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case quilt_sup:retrieve_member_fsm(CallId) of
        {'ok', FSM} ->
            gen_fsm:sync_send_all_state_event(FSM, {'abandon', JObj});
        {'error', 'not_found'} ->
            lager:debug("unable to find a running FSM for call id: ~p", [CallId])
    end;

handle_specific_event(<<"missed">>, JObj) ->
    quilt_log:handle_event(JObj);

handle_specific_event(<<"handled">>, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
    quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]), {"CONNECT", CallId}),
    lager:debug("call state updated: ~p", [{"CONNECT", CallId}]),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'error', 'not_found'} ->
            {'ok', FSM_agent} = quilt_sup:start_agent_fsm(AccountId, AgentId),
            lager:debug("started FSM: ~p for account/agent: ~p, ~p", [FSM_agent, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM_agent, {'answer', JObj});
        {'ok', FSM_agent} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM_agent, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM_agent, {'answer', JObj});
        Else ->
            lager:debug("unexpected return value when looking up FSM: ~p", [Else])
    end,
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case quilt_sup:retrieve_member_fsm(CallId) of
        {'ok', FSM_member} ->
            gen_fsm:sync_send_all_state_event(FSM_member, {'connected', JObj});
        {'error', 'not_found'} ->
            lager:debug("unable to find a running FSM for call id: ~p", [CallId])
    end;

handle_specific_event(<<"exited-position">>, JObj) ->
    lager:debug("exited queue at position: ~p", [JObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case quilt_sup:retrieve_member_fsm(CallId) of
        {'ok', FSM} ->
            gen_fsm:sync_send_all_state_event(FSM, {'exitqueue', JObj});
        {'error', 'not_found'} ->
            lager:debug("unable to find a running FSM for call id: ~p", [CallId])
    end;

handle_specific_event(<<"CHANNEL_BRIDGE">>, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    AgentId = case acdc_stats:find_call(CallId) of
        'undefined' -> wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], JObj);
        Call -> wh_json:get_value(<<"Agent-ID">>, Call)
    end,
    lager:debug("detected channel bridge, checking for transfer (account: ~p, agent: ~p, call-id: ~p, other-leg-call-id: ~p)", [AccountId, AgentId, CallId, wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj)]),
    case AgentId of
        'undefined' -> AgentId; %lager:debug("missing agent id");
        _ ->
            %lager:debug("detected channel bridge, checking for transfer (account: ~p, agent: ~p, call-id: ~p, other-leg-call-id: ~p)", [AccountId, AgentId, CallId, wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj)]),
            StoredState = quilt_store:get(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
            lager:debug("stored state: ~p", [StoredState]),
            case StoredState of
                'undefined' ->
                    lager:debug("unable to find any existing stored state for this call, ignoring...", []), 
                    StoredState;
                {"TRANSFERRED", CallId} -> % Call-ID matches a transferred call, log TRANSFER event
                    quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
                    quilt_sup:stop_member_fsm(CallId),
                    quilt_log:handle_event(JObj);
                {"CONNECT", StoredCallId} -> % Agent is connected to a queue member/caller, transition to OUTBOUND state
                    case wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj) of
                        StoredCallId -> % Member to agent bridge
                            lager:debug("ignoring member to agent bridge: ~p", [StoredState]);
                        _ ->
                            lager:debug("updating call state to: ~p", [{"OUTBOUND", StoredCallId}]),
                            quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
                            quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]), {"OUTBOUND", StoredCallId})
                    end;
                {"TRANSFER_CANCELLED", StoredCallId} -> % Agent previously cancelled a transfer and is connected to a queue member/caller, set to OUTBOUND state
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
            end
    end;

handle_specific_event(<<"CHANNEL_DESTROY">>, JObj) ->
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    AgentId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case AgentId of
        'undefined' -> AgentId; %lager:debug("missing agent id");
        _ ->
            lager:debug("detected channel destroy, checking for cancelled transfer (account: ~p, agent: ~p, call-id: ~p)", [AccountId, AgentId, CallId]),
            StoredState = quilt_store:get(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
            lager:debug("stored state: ~p", [StoredState]),
            case StoredState of
                {"OUTBOUND", StoredCallId} ->
                    TransferHistory = wh_json:get_value([<<"Transfer-History">>], JObj),
                    lager:debug("retrieving transfer history from call channel variables: ~p", [TransferHistory]),
                    case TransferHistory of
                        'undefined' ->
                            NewState = {"TRANSFER_CANCELLED", StoredCallId},
                            lager:debug("transfer was cancelled: ~p", [NewState]),
                            quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
                            quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]), NewState);
                        _ -> lager:debug("unable to find any transfer history for this call, ignoring...", [])
                    end;
                _ -> 
                    lager:debug("unable to find any existing stored state for this call, ignoring...", []),
                    StoredState
        end
    end;

handle_specific_event(<<"wrapup">>, JObj) ->
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

handle_specific_event(<<"processed">>, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    StoredState = quilt_store:get(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
    case StoredState of
        {"OUTBOUND", CallId} -> lager:debug("ignoring COMPLETE event when agent was in OUTBOUND state...", []);
        {"TRANSFERRED", CallId} -> lager:debug("ignoring COMPLETE event when agent was in TRANSFERRED state...", []);
        _ ->
            quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
            quilt_sup:stop_member_fsm(CallId),
            case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
                {'ok', FSM} ->
                    lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
                    gen_fsm:sync_send_all_state_event(FSM, {'hangup', JObj});
                Else ->
                    lager:debug("unable to find FSM to record processed call: ~p", [Else])
            end
    end;

handle_specific_event(<<"logged_in">>, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'error', 'not_found'} ->
            {'ok', FSM} = quilt_sup:start_agent_fsm(AccountId, AgentId),
            lager:debug("started FSM: ~p for account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'agentlogin', JObj});
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'agentlogin', JObj});
        Else ->
            lager:debug("unexpected return value when looking up FSM: ~p", [Else])
    end;

handle_specific_event(<<"logged_out">>, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'agentlogoff', JObj});
        Else ->
            lager:debug("unable to find FSM to record agent logoff: ~p", [Else])
    end;

handle_specific_event(<<"paused">>, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'error', 'not_found'} ->
            {'ok', FSM} = quilt_sup:start_agent_fsm(AccountId, AgentId),
            lager:debug("started FSM: ~p for account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'pauseall', JObj});
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'pauseall', JObj});
        Else ->
            lager:debug("unexpected return value when looking up FSM: ~p", [Else])
    end;

handle_specific_event(<<"resume">>, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'error', 'not_found'} ->
            {'ok', FSM} = quilt_sup:start_agent_fsm(AccountId, AgentId),
            lager:debug("started FSM: ~p for account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'unpauseall', JObj});
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'unpauseall', JObj});
        Else ->
            lager:debug("unexpected return value when looking up FSM: ~p", [Else])
    end.
