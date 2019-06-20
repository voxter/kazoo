%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc PostgreSQL kazoo storage driver
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kazoo_postgresql_sup).

-behaviour(supervisor).

-include("kz_postgresql.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0
        ,init/1
        ]).

-define(QUERIES_CACHE_PROPS, []).
-define(SCHEMA_CACHE_PROPS, []).
-define(CHILDREN, [?CACHE_ARGS(?KAZOO_POSTGRESQL_QUERIES_CACHE, ?QUERIES_CACHE_PROPS)
                  ,?CACHE_ARGS(?KAZOO_POSTGRESQL_SCHEMA_CACHE, ?SCHEMA_CACHE_PROPS)
                  ]).
%% ===================================================================
%% API functions
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
