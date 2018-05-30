%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc Asterisk queue_log translator for Kazoo
%%%
%%% @author Lucas Bussey
%%% @end
%%%-----------------------------------------------------------------------------
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

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec init([]) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

%%
%% Queue member fsm
%%

-spec start_member_fsm(kz_term:ne_binary()) -> kz_types:sup_startchild_ret().
start_member_fsm(CallId) ->
    FSM = erlang:iolist_to_binary([<<"quilt_member_fsm-">>, CallId]),
    supervisor:start_child(?MODULE, ?WORKER_NAME_ARGS_TYPE(FSM, 'quilt_member_fsm', [CallId], 'transient')).

-spec retrieve_member_fsm(kz_term:ne_binary()) -> {'ok', pid()} | {'error', 'not_found'}.
retrieve_member_fsm(CallId) ->
    Fsms = [Pid || {Name, Pid, 'worker', ['quilt_member_fsm']} <- supervisor:which_children(?MODULE), Name == erlang:iolist_to_binary([<<"quilt_member_fsm-">>, CallId])],
    case length(Fsms) of
        0 -> {'error', 'not_found'};
        _ -> {'ok', hd(Fsms)}
    end.

-spec stop_member_fsm(kz_term:ne_binary()) -> 'ok' | {'error', atom()}.
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

-spec start_agent_fsm(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:sup_startchild_ret().
start_agent_fsm(AccountId, AgentId) ->
    FSM = erlang:iolist_to_binary([<<"quilt_agent_fsm-">>, AccountId, <<"-">>, AgentId]),
    supervisor:start_child(?MODULE
                          ,?WORKER_NAME_ARGS_TYPE(FSM
                                                 ,'quilt_agent_fsm'
                                                 ,[#state{account_id=AccountId
                                                         ,agent_id=AgentId
                                                         ,member_call_id='undefined'
                                                         }
                                                  ]
                                                 ,'transient'
                                                 )).

-spec retrieve_agent_fsm(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                {'ok', pid()} | {'error', 'not_found'}.
retrieve_agent_fsm(AccountId, AgentId) ->
    Fsms = [Pid || {Name, Pid, 'worker', ['quilt_agent_fsm']} <- supervisor:which_children(?MODULE), Name == erlang:iolist_to_binary([<<"quilt_agent_fsm-">>, AccountId, <<"-">>, AgentId])],
    case length(Fsms) of
        0 -> {'error', 'not_found'};
        _ -> {'ok', hd(Fsms)}
    end.

-spec stop_agent_fsm(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | {'error', atom()}.
stop_agent_fsm(AccountId, AgentId) ->
    Name = erlang:iolist_to_binary([<<"quilt_agent_fsm-">>, AccountId, <<"-">>, AgentId]),
    case retrieve_agent_fsm(AccountId, AgentId) of
        {'ok', FSM} ->
            lager:debug("terminating agent FSM: ~p", [Name]),
            _ = supervisor:terminate_child(?MODULE, FSM),
            supervisor:delete_child(?MODULE, Name);
        _ ->
            lager:debug("could not find agent FSM to terminate: ~p", [Name])
    end.

-spec get_pid() -> pid().
get_pid() ->
    self().
