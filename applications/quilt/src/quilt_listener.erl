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
                                                %,{<<"acdc_status_stat">>, <<"logged_out">>}
                                                        ,{<<"acdc_status_stat">>, <<"paused">>}
                                                        ,{<<"acdc_status_stat">>, <<"resume">>}
                                                        ,{<<"agent">>, <<"login_queue">>}
                                                        ,{<<"agent">>, <<"logout_queue">>}
                                                %,{<<"call_event">>, <<"CHANNEL_BRIDGE">>}
                                                        ,{<<"call_event">>, <<"CHANNEL_DESTROY">>}
                                                        ]
                     }]).

-type state() :: [].

-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE, [
                                      {'bindings', ?BINDINGS},
                                      {'responders', ?RESPONDERS}
                                     ], []).

-spec handle_quilt_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_quilt_event(JObj, _Props) ->
    handle_specific_event(kz_json:get_value(<<"Event-Name">>, JObj), JObj).

-spec init([]) -> {'ok', []}.
init([]) ->
    {'ok', []}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(Request, _From, State) ->
    lager:debug("unhandled call: ~p", [Request]),
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast(Msg, State) ->
    lager:debug("unhandled cast: ~p", [Msg]),
    {'noreply', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, 'shutdown'} ->
            lager:debug("~p shutting down...", [Pid]),
            exit(Pid, "shutting down...");
        _ ->
            lager:debug("unhandled info: ~p", [Info])
    end,
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, _State) ->
    lager:debug("~p listener on pid ~p terminating: ~p", [?MODULE, self(), Reason]),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%
%% event-specific handlers
%%

-spec handle_specific_event(ne_binary(), kz_json:object()) -> 'ok'.
handle_specific_event(<<"waiting">>, JObj) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
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
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
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
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case quilt_sup:retrieve_member_fsm(CallId) of
        {'ok', FSM} ->
            gen_fsm:sync_send_all_state_event(FSM, {'exitqueue', JObj});
        {'error', 'not_found'} ->
            lager:debug("unable to find a running FSM for call id: ~p", [CallId])
    end;

handle_specific_event(<<"handled">>, JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
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
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case quilt_sup:retrieve_member_fsm(CallId) of
        {'ok', FSM_member} ->
            gen_fsm:sync_send_all_state_event(FSM_member, {'connected', JObj});
        {'error', 'not_found'} ->
            lager:debug("unable to find a running FSM for call id: ~p", [CallId])
    end;

handle_specific_event(<<"CHANNEL_DESTROY">>, JObj) ->
    AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    AgentId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], JObj),
    EndpointDisposition = kz_json:get_value(<<"Endpoint-Disposition">>, JObj),
    lager:debug("CHANNEL_DESTROY, checking for transfer (account: ~p, agent: ~p, endpoint-disposition: ~p)", [AccountId, AgentId, EndpointDisposition]),
    case EndpointDisposition of
        <<"BLIND_TRANSFER">> -> %% Agent performed a successful blind transfer
            CallId = kz_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
            quilt_sup:stop_member_fsm(CallId),
            case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
                {'ok', FSM} ->
                    lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
                    gen_fsm:sync_send_all_state_event(FSM, {'transfer', JObj});
                Else ->
                    lager:debug("unable to find FSM to record transferred call: ~p", [Else])
            end;
        <<"ATTENDED_TRANSFER">> -> %% Agent performed a successful transfer
            CallId = kz_json:get_value(<<"Call-ID">>, JObj),
            quilt_sup:stop_member_fsm(CallId),
            case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
                {'ok', FSM} ->
                    lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
                    gen_fsm:sync_send_all_state_event(FSM, {'transfer', JObj});
                Else ->
                    lager:debug("unable to find FSM to record transferred call: ~p", [Else])
            end;
        Else ->
            Else
    end;

handle_specific_event(<<"processed">>, JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    quilt_sup:stop_member_fsm(CallId),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'hangup', JObj});
        Else ->
            lager:debug("unable to find FSM to record processed call: ~p", [Else])
    end;

handle_specific_event(<<"login_queue">>, JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'error', 'not_found'} ->
            {'ok', FSM} = quilt_sup:start_agent_fsm(AccountId, AgentId),
            lager:debug("started FSM: ~p for account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'queuelogin', JObj});
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'queuelogin', JObj});
        Else ->
            lager:debug("unexpected return value when looking up FSM: ~p", [Else])
    end;

handle_specific_event(<<"logout_queue">>, JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'queuelogoff', JObj});
        Else ->
            lager:debug("unexpected return value when looking up FSM: ~p", [Else])
    end;

handle_specific_event(<<"logged_in">>, JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
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
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    case quilt_sup:retrieve_agent_fsm(AccountId, AgentId) of
        {'ok', FSM} ->
            lager:debug("found FSM: ~p this account/agent: ~p, ~p", [FSM, AccountId, AgentId]),
            gen_fsm:sync_send_all_state_event(FSM, {'agentlogoff', JObj});
        Else ->
            lager:debug("unable to find FSM to record agent logoff: ~p", [Else])
    end;

handle_specific_event(<<"paused">>, JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
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
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
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
