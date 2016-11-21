%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(konami_call_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([handle_metaflow/2]).

-include("konami.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec handle_metaflow(kz_json:object(), kz_proplist()) ->
                             sup_startchild_ret() | 'ok'.
handle_metaflow(JObj, Props) ->
    CallId = kz_json:get_value([<<"Call">>, <<"Call-ID">>], JObj),
    create_or_update_call(pid_for_callid(CallId), JObj, Props).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?WORKER_TYPE('konami_call', 'transient')]}}.

pid_for_callid(CallId) ->
    pid_for_callid(CallId, [Pid || {_, Pid, 'worker', ['konami_call']} <- supervisor:which_children(?MODULE)]).

pid_for_callid(_, []) ->
    'undefined';
pid_for_callid(CallId, [Pid|Pids]) ->
    case konami_call:call_id(Pid) of
        CallId -> Pid;
        _ -> pid_for_callid(CallId, Pids)
    end.

create_or_update_call('undefined', JObj, Props) ->
    supervisor:start_child(?MODULE, [JObj, Props]);
create_or_update_call(Pid, JObj, Props) ->
    konami_call:update(Pid, JObj, Props).
