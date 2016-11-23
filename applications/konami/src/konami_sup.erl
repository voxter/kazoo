%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(konami_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_konami_code_fsm/3]).
-export([init/1]).

-include("konami.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER('konami_listener')
                  ,?WORKER('konami_event_listener')
                  ,?WORKER('konami_init')
                  ,?SUPER('konami_call_sup')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_konami_code_fsm(kapps_call:call(), kz_json:object(), pid()) ->
                                   sup_startchild_ret().
start_konami_code_fsm(Call, JObj, KonamiCallPid) ->
    supervisor:start_child(?SERVER
                          ,?WORKER_NAME_ARGS_TYPE("konami_code_fsm_" ++ pid_to_list(KonamiCallPid)
                                                 ,'konami_code_fsm'
                                                 ,[Call, JObj, KonamiCallPid]
                                                 ,'transient'
                                                 )).

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
-spec init(any()) -> sup_init_ret().
init([]) ->
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
