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

-export([start_link/0, get_pid/0]).
-export([start_member_fsm/1, retrieve_member_fsm/1, stop_member_fsm/1]).
-export([start_agent_fsm/2, retrieve_agent_fsm/2, stop_agent_fsm/2]).
-export([init/1]).

-include("quilt.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [
	?WORKER_TYPE('quilt_listener', 'transient')
	%,?WORKER_TYPE('quilt_store', 'transient')
	]).

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

%%
%% Queue member fsm
%%

start_member_fsm(CallId) ->
	FSM = erlang:iolist_to_binary([<<"quilt_member_fsm-">>, CallId]),
	supervisor:start_child(?MODULE, ?WORKER_NAME_ARGS_TYPE(FSM, 'quilt_member_fsm', [CallId], 'transient')).

retrieve_member_fsm(CallId) ->
	Fsms = [Pid || {Name, Pid, 'worker', ['quilt_member_fsm']} <- supervisor:which_children(?MODULE), Name == erlang:iolist_to_binary([<<"quilt_member_fsm-">>, CallId])],
	case length(Fsms) of
		0 -> {'error', 'not_found'};
		_ -> {'ok', hd(Fsms)}
	end.

stop_member_fsm(CallId) ->
  Name = erlang:iolist_to_binary([<<"quilt_member_fsm-">>, CallId]),
  case retrieve_member_fsm(CallId) of
    {'ok', _FSM} -> 
        lager:debug("terminating member FSM: ~p", [Name]),
        case supervisor:terminate_child(?MODULE, Name) of
            'ok' -> lager:debug("member fsm terminated");
            {'error', E} -> lager:debug("member fsm not terminated (~s)", [E])
        end,
        supervisor:delete_child(?MODULE, Name);
    _ -> 
        lager:debug("could not find member FSM to terminate: ~p", [Name])
  end.

%% 
%% Queue agent fsm
%%

start_agent_fsm(AccountId, AgentId) ->
	FSM = erlang:iolist_to_binary([<<"quilt_agent_fsm-">>, AccountId, <<"-">>, AgentId]),
	supervisor:start_child(?MODULE, ?WORKER_NAME_ARGS_TYPE(FSM, 'quilt_agent_fsm', [#state{member_call_id='undefined'}], 'transient')).

retrieve_agent_fsm(AccountId, AgentId) ->
	Fsms = [Pid || {Name, Pid, 'worker', ['quilt_agent_fsm']} <- supervisor:which_children(?MODULE), Name == erlang:iolist_to_binary([<<"quilt_agent_fsm-">>, AccountId, <<"-">>, AgentId])],
	case length(Fsms) of
		0 -> {'error', 'not_found'};
		_ -> {'ok', hd(Fsms)}
	end.

stop_agent_fsm(AccountId, AgentId) ->
  Name = erlang:iolist_to_binary([<<"quilt_agent_fsm-">>, AccountId, <<"-">>, AgentId]),
	case retrieve_agent_fsm(AccountId, AgentId) of
		{'ok', FSM} -> 
      lager:debug("terminating agent FSM: ~p", [Name]),
      supervisor:terminate_child(?MODULE, FSM),
      supervisor:delete_child(?MODULE, Name);
		_ -> 
      lager:debug("could not find agent FSM to terminate: ~p", [Name])
	end.

get_pid() ->
	self().
