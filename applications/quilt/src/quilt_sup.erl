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

-export([start_link/0]).
-export([init/1]).

-include("quilt.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER_TYPE('quilt_listener', temporary)]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    supervisor:start_child(?MODULE, []).

init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    {'ok', {SupFlags, ?CHILDREN}}.