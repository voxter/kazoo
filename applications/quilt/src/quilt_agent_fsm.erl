%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_agent_fsm).

-behaviour(gen_fsm).

-include("quilt.hrl").

-export([start_link/1]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%
%% Public functions
%%

start_link(Agent) ->
    gen_fsm:start_link(?MODULE, Agent, []).

%%
%% gen_fsm callbacks
%%

init(Agent) ->
    process_flag('trap_exit', 'true'),
    {'ok', 'started', Agent}.

handle_event(Event, StateName, State) ->
    lager:debug("unhandled event ~p from current state of ~s with state record: ~p", [Event, StateName, State]),
    {'next_state', StateName, State}.

handle_sync_event({'agentlogin', JObj}, _From, 'started', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'agentlogin', JObj}, _From, 'loggedout', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'agentlogoff', JObj}, _From, 'paused', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'loggedout', #state{member_call_id='undefined'}};
handle_sync_event({'agentlogoff', JObj}, _From, 'ready', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'loggedout', #state{member_call_id='undefined'}};
handle_sync_event({'unpauseall', JObj}, _From, 'started', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'unpauseall', JObj}, _From, 'paused', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'pauseall', JObj}, _From, 'started', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'paused', #state{member_call_id='undefined'}};
handle_sync_event({'pauseall', JObj}, _From, 'ready', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'paused', #state{member_call_id='undefined'}};
handle_sync_event({'answer', JObj}, _From, 'started', State) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("agent answered call with ID: ~p", [CallId]),
    State1 = State#state{member_call_id=CallId},
    {'reply', 'ok', 'incall', State1};
handle_sync_event({'answer', JObj}, _From, 'ready', State) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("agent answered call with ID: ~p", [CallId]),
    State1 = State#state{member_call_id=CallId},
    {'reply', 'ok', 'incall', State1};
handle_sync_event({'transfer', JObj}, _From, 'incall', State) ->
    CallId = State#state.member_call_id,
    lager:debug("agent transferred call with ID: ~p to callee ID number: ~p", [CallId, wh_json:get_value(<<"Callee-ID-Number">>, JObj)]),
    J1 = wh_json:set_value(<<"Event-Category">>, <<"call_event">>, JObj),
    J2 = wh_json:set_value(<<"Event-Name">>, <<"transfer">>, J1),
    JObj1 = wh_json:set_value(<<"Call-ID">>, CallId, J2),
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj1]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'hangup', JObj}, _From, 'incall', _State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event(Event, _From, StateName, State) ->
    lager:debug("unhandled sync event ~p from current state of ~s with state record: ~p", [Event, StateName, State]),
    {'reply', 'ok', StateName, State}.

handle_info({'$gen_cast', _}, StateName, State) ->
    {'next_state', StateName, State};
handle_info({'EXIT', _Pid, _Reason}, StateName, State) ->
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

