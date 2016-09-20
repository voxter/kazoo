%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_member_fsm).

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
    put('callid', CallId),
    process_flag('trap_exit', 'true'),
    {'ok', 'started', CallId}.

handle_event(Event, StateName, State) ->
    lager:debug("unhandled event in state ~s: ~p", [StateName, Event]),
    {'next_state', StateName, State}.

handle_sync_event({'enterqueue', JObj}, _From, 'started', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'waiting', State};
handle_sync_event({'connected', JObj}, _From, 'waiting', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'incall', State};
handle_sync_event({'connected', JObj}, _From, 'incall', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'incall', State};
handle_sync_event({'exitqueue', JObj}, _From, 'waiting', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'incall', State};
handle_sync_event({'exitqueue', JObj}, _From, 'connected', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'incall', State};
handle_sync_event({'exitqueue', JObj}, _From, 'abandoned', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'hangup', State};
handle_sync_event({'abandon', JObj}, _From, 'waiting', State) ->
    _ = wh_util:spawn('quilt_log', 'handle_event', [JObj]),
    {'reply', 'ok', 'abandoned', State};
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

