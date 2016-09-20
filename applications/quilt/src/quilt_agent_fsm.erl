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

% -record(state, {}).

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
    lager:debug("unhandled event in state ~s: ~p", [StateName, Event]),
    {'next_state', StateName, State}.

handle_sync_event({'agentlogin', JObj}, _From, 'started', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', State};
handle_sync_event({'agentlogin', JObj}, _From, 'loggedout', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', State};
handle_sync_event({'agentlogoff', JObj}, _From, 'paused', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'loggedout', State};
handle_sync_event({'agentlogoff', JObj}, _From, 'ready', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'loggedout', State};
handle_sync_event({'unpauseall', JObj}, _From, 'started', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', State};
handle_sync_event({'unpauseall', JObj}, _From, 'paused', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', State};
handle_sync_event({'pauseall', JObj}, _From, 'started', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'paused', State};
handle_sync_event({'pauseall', JObj}, _From, 'ready', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'paused', State};
handle_sync_event({'answer', _JObj}, _From, 'started', State) ->
    {'reply', 'ok', 'incall', State};
handle_sync_event({'answer', _JObj}, _From, 'ready', State) ->
    {'reply', 'ok', 'incall', State};
handle_sync_event({'hangup', JObj}, _From, 'incall', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'ready', State};
handle_sync_event(Event, _From, StateName, State) ->
    lager:debug("unhandled sync event in state ~s: ~p", [StateName, Event]),
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

