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

-export([start_link/0]).
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
    {?MODULE, 'handle_event'}, [
        {<<"acdc_call_stat">>, <<"*">>}
        ,{<<"acdc_status_stat">>, <<"*">>}
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

handle_event(JObj, _State) ->
    handle_specific_event(wh_json:get_value(<<"Event-Name">>, JObj), JObj),
    {'noreply', []}.

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
    case quilt_sup:retrieve_fsm(CallId) of
        {'error', 'not_found'} ->
            {'ok', FSM} = quilt_sup:start_fsm(CallId),
            lager:debug("started FSM: ~p for call id: ~p", [FSM, CallId]),
            gen_fsm:sync_send_all_state_event(FSM, {'enterqueue', JObj});
        {'ok', FSM} ->
            lager:debug("FSM ~p already created for this call id: ~p", [FSM, CallId]);
        Else ->
            lager:debug("unexpected return value when looking up FSM: ~p", [Else])
    end;
    
handle_specific_event(<<"exited-position">>, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    quilt_sup:stop_fsm(CallId),
    quilt_log:handle_event(JObj);

handle_specific_event(<<"handled">>, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case quilt_sup:retrieve_fsm(CallId) of
        {'ok', FSM} ->
            gen_fsm:sync_send_all_state_event(FSM, {'connected', JObj});
        {'error', 'not_found'} ->
            lager:debug("unable to find a running FSM for call id: ~p", [CallId])
    end;

handle_specific_event(_, JObj) ->
    quilt_log:handle_event(JObj).