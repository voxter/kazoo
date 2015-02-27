-module(blackhole_ami_sup).
-behaviour(supervisor).

-export([start_link/1, start_ami_listener/0]).
-export([init/1]).

-include("blackhole.hrl").

start_link(ListenSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSocket]).

start_ami_listeners() ->
    start_ami_listeners(20).

start_ami_listeners(Count) ->
    lager:debug("AMI: Starting ~p AMI listener processes", [Count]),
    [start_ami_listener() || _ <- lists:seq(1, Count)],
    ok.
    
start_ami_listener() ->
    supervisor:start_child(?MODULE, []).
        
init([ListenSocket]) ->
    spawn_link(fun start_ami_listeners/0),
    {'ok', {
        {simple_one_for_one, 60, 3600}, [{
            blackhole_ami_comm, {
                blackhole_ami_comm, start_link, [ListenSocket]
            },
            temporary,
            1000,
            worker,
            [blackhole_ami_comm]
        }]
    }}.