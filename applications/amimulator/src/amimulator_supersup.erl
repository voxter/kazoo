-module(amimulator_supersup).
-behaviour(supervisor).

-export([start_link/0, start_listener_sup/0, before_stop/0, init/1]).

-include("amimulator.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec start_listener_sup() -> supervisor:startchild_ret().
start_listener_sup() ->
	ChildSpec = {'amimulator_sup', {'amimulator_sup', 'start_link', []}, 'permanent', 'infinity', 'supervisor', ['amimulator_sup']},
	supervisor:start_child(?MODULE, ChildSpec).

-spec before_stop() -> 'ok'.
before_stop() ->
	lager:debug("Running pre-stop cleanup"),
	amimulator_serv:close_listen_socket().

%%
%% supervisor callbacks
%%

init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpec = {'amimulator_serv', {'amimulator_serv', 'start_link', []}, 'permanent', 'infinity', 'worker', ['amimulator_serv']},

    {'ok', {SupFlags, [ChildSpec]}}.
