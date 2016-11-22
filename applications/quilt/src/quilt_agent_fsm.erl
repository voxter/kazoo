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

-type state() :: #state{}.

%%
%% Public functions
%%

-spec start_link(state()) -> startlink_ret().
start_link(Agent) ->
    gen_fsm:start_link(?MODULE, Agent, []).

%%
%% gen_fsm callbacks
%%

-spec init(state()) -> {'ok', 'started', state()}.
init(Agent) ->
    process_flag('trap_exit', 'true'),
    {'ok', 'started', Agent}.

-spec handle_event(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_event(Event, StateName, State) ->
    lager:debug("unhandled event ~p from current state of ~s with state record: ~p", [Event, StateName, State]),
    {'next_state', StateName, State}.

-spec handle_sync_event(any(), {pid(),any()}, atom(), state()) -> handle_sync_event_ret(state()).
handle_sync_event({'agentlogin', JObj}, _From, 'started', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'agentlogin', JObj}, _From, 'loggedout', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'agentlogoff', JObj}, _From, 'paused', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'loggedout', #state{member_call_id='undefined'}};
handle_sync_event({'agentlogoff', JObj}, _From, 'ready', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'loggedout', #state{member_call_id='undefined'}};
handle_sync_event({'unpauseall', JObj}, _From, 'started', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'unpauseall', JObj}, _From, 'paused', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'pauseall', JObj}, _From, 'started', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'paused', #state{member_call_id='undefined'}};
handle_sync_event({'pauseall', JObj}, _From, 'ready', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'paused', #state{member_call_id='undefined'}};
handle_sync_event({'answer', JObj}, _From, 'started', State) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("agent answered call with ID: ~p", [CallId]),
    State1 = State#state{member_call_id=CallId},
    {'reply', 'ok', 'incall', State1};
handle_sync_event({'answer', JObj}, _From, 'ready', State) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("agent answered call with ID: ~p", [CallId]),
    State1 = State#state{member_call_id=CallId},
    {'reply', 'ok', 'incall', State1};
handle_sync_event({'transfer', JObj}, _From, 'incall', State) ->
    CallId = State#state.member_call_id,
    lager:debug("agent transferred call with ID: ~p to callee ID number: ~p", [CallId, kz_json:get_value(<<"Callee-ID-Number">>, JObj)]),
    J1 = kz_json:set_value(<<"Event-Category">>, <<"call_event">>, JObj),
    J2 = kz_json:set_value(<<"Event-Name">>, <<"transfer">>, J1),
    JObj1 = kz_json:set_value(<<"Call-ID">>, CallId, J2),

    %% If the transfer tries to read Talk-Time before stat processed, queue_log
    %% proc will crash. But we must block so that the hangup event does not
    %% occur before this one or we get COMPLETEAGENT instead of TRANSFER
    sleep_until_call_stat_processed(CallId),

    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj1]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event({'hangup', JObj}, _From, 'incall', _State) ->
    _ = kz_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', #state{member_call_id='undefined'}};
handle_sync_event(Event, _From, StateName, State) ->
    lager:debug("unhandled sync event ~p from current state of ~s with state record: ~p", [Event, StateName, State]),
    {'reply', 'ok', StateName, State}.

-spec handle_info(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_info({'$gen_cast', _}, StateName, State) ->
    {'next_state', StateName, State};
handle_info({'EXIT', _Pid, _Reason}, StateName, State) ->
    {'next_state', StateName, State};
handle_info(Info, StateName, State) ->
    lager:debug("unhandled info in state ~s: ~p", [StateName, Info]),
    {'next_state', StateName, State}.

-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(Reason, StateName, _) ->
    lager:debug("terminating in state ~s (~s)", [StateName, Reason]).

-spec code_change(any(), atom(), state(), any()) -> {'ok', atom(), state()}.
code_change(_, StateName, State, _) ->
    {'ok', StateName, State}.

%%
%% gen_fsm states
%%

%%
%% Private functions
%%
-spec sleep_until_call_stat_processed(ne_binary()) -> 'ok'.
sleep_until_call_stat_processed(CallId) ->
    Stat = acdc_stats:find_call(CallId),
    case kz_json:get_value(<<"Status">>, Stat) of
        <<"processed">> -> 'ok';
        _ ->
            timer:sleep(1000),
            sleep_until_call_stat_processed(CallId)
    end.
