-module(quilt_sup).
-behaviour(supervisor).

-export([start_link/1, start_listeners/0, start_listener/0]).
-export([init/1]).

-include("quilt.hrl").

start_link(ListenSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSocket]).

start_listeners() ->
    start_listeners(20).

start_listeners(Count) ->
    lager:debug("Starting ~p listener processes", [Count]),
    [start_listener() || _ <- lists:seq(1, Count)],
    ok.
    
start_listener() ->
    supervisor:start_child(?MODULE, []).
        
init([ListenSocket]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 60,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    {'ok', {SupFlags, [?WORKER_ARGS_TYPE(quilt_comm, [ListenSocket], temporary)]}}.