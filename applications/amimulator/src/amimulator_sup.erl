-module(amimulator_sup).
-behaviour(supervisor).

-export([start_link/0, start_listeners/1, start_listener/2, find_ev/1, start_ev/1, pause_ev/1,
    do_pause_check/2]).
-export([init/1]).

-include("amimulator.hrl").

-define(EV_TIMEOUT, 60000).

%%
%% Public functions
%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_listeners(ListenSocket) ->
    start_listeners(ListenSocket, 20).

start_listeners(ListenSocket, Count) ->
    lager:debug("Starting ~p listener processes", [Count]),
    [start_listener(ListenSocket, Num) || Num <- lists:seq(1, Count)],
    ok.
    
%% Launch a client handler process listening on the given socket
start_listener(ListenSocket, Num) ->
    supervisor:start_child(?MODULE, {
        "amimulator_comm-" ++ wh_util:to_list(Num),
        {amimulator_comm, start_link, [ListenSocket]}, permanent, 2000, worker, [amimulator_comm]
    }).

%% Find an event consumer for a kazoo account
find_ev(AccountId) ->
    find_ev("ami_ev-" ++ wh_util:to_list(AccountId), workers()).

%% Launch an event consumer for a specific kazoo account
start_ev(AccountId) ->
    supervisor:start_child(?MODULE, {
        "ami_ev-" ++ wh_util:to_list(AccountId),
        {ami_ev, start_link, [AccountId]}, permanent, 2000, worker, [ami_ev]
    }).

pause_ev(AccountId) ->
    % lager:debug("Soon to reap the zombied ami_ev"),
    Timestamp = wh_util:current_tstamp(),
    ami_sm:ev_going_down(AccountId, Timestamp),
    timer:apply_after(?EV_TIMEOUT, ?MODULE, do_pause_check, [AccountId, Timestamp]).

do_pause_check(AccountId, Timestamp) ->
    case ami_sm:is_ev_down(AccountId, Timestamp) of
        true ->
            lager:debug("IT'S REAPING TIME"),
            EvName = "ami_ev-" ++ wh_util:to_list(AccountId),
            case supervisor:terminate_child(?MODULE, EvName) of
                ok ->
                    supervisor:delete_child(?MODULE, EvName),
            		ami_sm:purge_state(AccountId);
                {error, Error} ->
                    lager:debug("error when reaping event consumer ~p", [Error])
            end;
        _ ->
            ok
    end.

%%
%% supervisor callbacks
%%
        
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    StateMaster = {
        ami_sm, {ami_sm, start_link, []}, permanent, infinity, worker, [ami_sm]
    },
    
    {'ok', {
        SupFlags, [StateMaster]
    }}.

%%
%% Private functions
%%

workers() ->
    [{WorkerName, Pid} || {WorkerName, Pid, worker, _} <- supervisor:which_children(?MODULE), is_pid(Pid)].

find_ev(_EvName, []) ->
    undefined;
find_ev(EvName, [{EvName, Pid}|_Workers]) ->
    Pid;
find_ev(EvName, [_Worker|Workers]) ->
    find_ev(EvName, Workers).














