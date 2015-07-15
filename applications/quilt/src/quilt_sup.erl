%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_sup).
-behaviour(supervisor).

-export([start_link/0, get_pid/0, start_fsm/1, retrieve_fsm/1, stop_fsm/1]).
-export([init/1]).

-include("quilt.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [
	?WORKER_TYPE('quilt_listener', 'transient')
	,?WORKER_TYPE('quilt_store', 'transient')
	]).

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

start_fsm(CallId) ->
	FSM = erlang:iolist_to_binary([<<"quilt_fsm-">>, CallId]),
	supervisor:start_child(?MODULE, ?WORKER_NAME_ARGS_TYPE(FSM, 'quilt_fsm', [CallId], 'transient')).

retrieve_fsm(CallId) ->
	Fsms = [Pid || {Name, Pid, 'worker', ['quilt_fsm']} <- supervisor:which_children(?MODULE), Name == erlang:iolist_to_binary([<<"quilt_fsm-">>, CallId])],
	case length(Fsms) of
		0 -> {'error', 'not_found'};
		_ -> {'ok', hd(Fsms)}
	end.

stop_fsm(CallId) ->
	case retrieve_fsm(CallId) of
		{'ok', FSM} -> supervisor:terminate_child(?MODULE, FSM);
		_ -> lager:debug("could not terminate FSM, not found...", [])
	end.

get_pid() ->
	self().