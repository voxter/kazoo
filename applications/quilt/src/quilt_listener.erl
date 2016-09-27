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
        %{'restrict_to', [<<"CHANNEL_BRIDGE">>, <<"CHANNEL_DESTROY">>]}
        {'restrict_to', [<<"CHANNEL_DESTROY">>]}
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
        %,{<<"acdc_status_stat">>, <<"wrapup">>}
        ,{<<"acdc_status_stat">>, <<"logged_in">>}
        ,{<<"acdc_status_stat">>, <<"logged_out">>}
        ,{<<"acdc_status_stat">>, <<"paused">>}
        ,{<<"acdc_status_stat">>, <<"resume">>}
        ,{<<"agent">>, <<"login_queue">>}
        ,{<<"agent">>, <<"logout_queue">>}
        %,{<<"call_event">>, <<"CHANNEL_BRIDGE">>}
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

handle_specific_event(<<"exited-position">>, JObj) ->
    lager:debug("exited queue at position: ~p", [JObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case quilt_sup:retrieve_member_fsm(CallId) of
        {'ok', FSM} ->
            gen_fsm:sync_send_all_state_event(FSM, {'exitqueue', JObj});
        {'error', 'not_found'} ->
            lager:debug("unable to find a running FSM for call id: ~p", [CallId])
    end;

handle_specific_event(<<"handled">>, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
    quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]), {"CONNECTED", CallId}),
    lager:debug("call state updated: ~p", [{"CONNECTED", CallId}]),
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

handle_specific_event(<<"CHANNEL_DESTROY">>, JObj) ->
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    AgentId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    OtherLegCallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
    EndpointDisposition = wh_json:get_value(<<"Endpoint-Disposition">>, JObj),
    lager:debug("CHANNEL_DESTROY, checking for transfer (account: ~p, agent: ~p, call-id: ~p, other-leg-call-id: ~p, endpoint-disposition: ~p)", [AccountId, AgentId, CallId, OtherLegCallId, EndpointDisposition]),
    case {AgentId, EndpointDisposition} of
        {'undefined', _} -> 'undefined'; %% Undefined agent ID
        {_, 'undefined'} -> 'undefined'; %% Undefined endpoint disposition
        {_, <<"ATTENDED_TRANSFER">>} -> %% Agent performed a successful transfer
            {"CONNECTED", StoredCallId} = quilt_store:get(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
            lager:debug("current member call ID in store: ~p", [StoredCallId]),
            quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
            quilt_store:put(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId]), {"TRANSFERRED", {StoredCallId, wh_json:get_value(<<"Callee-ID-Number">>, JObj)}});
        Else -> lager:debug("Unhandled agent endpoint disposition in channel destroy, ~p", [Else]) %% Unhandled endpoint disposition
    end;

handle_specific_event(<<"processed">>, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    StoredState = quilt_store:get(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
    lager:debug("processing agent hangup: ~p", [StoredState]),
    JObj1 = case StoredState of
        {"TRANSFERRED", {CallId, Extension}} ->
            J1 = wh_json:set_value(<<"Event-Category">>, <<"call_event">>, JObj),
            J2 = wh_json:set_value(<<"Event-Name">>, <<"transfer">>, J1),
            wh_json:set_value(<<"Callee-ID-Number">>, Extension, J2);
        _ ->
            JObj
    end,
    quilt_store:delete(erlang:iolist_to_binary([AccountId, <<"-">>, AgentId])),
    quilt_sup:stop_member_fsm(CallId),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'hangup', JObj1});
        Else ->
            lager:debug("unable to find FSM to record processed call: ~p", [Else])
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
