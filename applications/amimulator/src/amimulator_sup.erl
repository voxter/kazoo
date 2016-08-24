-module(amimulator_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_listeners/1, start_listener/2
         ,register_event_listener/2
         ,unregister_event_listener/2
         ,start_event_listener/1
         ,stop_event_listener/2
        ]).
-export([init/1]).

-include("amimulator.hrl").

%%
%% Public functions
%%

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

start_listeners(ListenSocket) ->
    start_listeners(ListenSocket, 20).

start_listeners(ListenSocket, Count) ->
    lager:debug("Starting ~p socket listener processes", [Count]),
    [start_listener(ListenSocket, Num) || Num <- lists:seq(1, Count)],
    'ok'.
    
%% Launch a client handler process listening on the given socket
start_listener(ListenSocket, Num) ->
    supervisor:start_child(?MODULE, {"amimulator_socket_listener-" ++ kz_util:to_list(Num)
                                     ,{'amimulator_socket_listener', 'start_link', [ListenSocket]}
                                     ,'permanent'
                                     ,2000
                                     ,'worker'
                                     ,['amimulator_socket_listener']
                                    }).

-spec register_event_listener(ne_binary(), pid()) -> 'ok'.
register_event_listener(AccountId, Consumer) ->
    ListenerPid = case find_event_listener(AccountId) of
        'undefined' ->
            {'ok', Pid} = start_event_listener(AccountId),
            Pid;
        {Pid, _} -> Pid
    end,
    amimulator_event_listener:register(ListenerPid, Consumer).

-spec unregister_event_listener(ne_binary(), pid()) -> 'ok'.
unregister_event_listener(AccountId, Consumer) ->
    case find_event_listener(AccountId) of
        'undefined' -> 'ok';
        {Pid, _} -> amimulator_event_listener:unregister(Pid, Consumer)
    end.

%% Launch an event consumer for a specific kazoo account
-spec start_event_listener(ne_binary()) -> supervisor:startchild_ret().
start_event_listener(AccountId) ->
    supervisor:start_child(?MODULE, {"amimulator_event_listener-" ++ kz_util:to_list(AccountId)
                                     ,{'amimulator_event_listener', 'start_link', [AccountId]}
                                     ,'transient'
                                     ,2000
                                     ,'worker'
                                     ,['amimulator_event_listener']
                                    }).

-spec stop_event_listener(ne_binary(), atom()) -> 'ok' | tuple().
-spec stop_event_listener(ne_binary(), api_pid(), atom()) -> 'ok' | tuple().
stop_event_listener(AccountId, Reason) ->
    ami_sm:purge_state(AccountId),
    stop_event_listener(AccountId, find_event_listener(AccountId), Reason).

stop_event_listener(AccountId, 'undefined', _) ->
    lager:debug("could not find event listener for account ~p to prune", [AccountId]),
    {'error', 'not_found'};
stop_event_listener(AccountId, {Pid, WorkerName}, 'pruned') ->
    lager:debug("pruning event listener for account ~p", [AccountId]),
    case supervisor:terminate_child(?MODULE, WorkerName) of
        'ok' -> supervisor:delete_child(?MODULE, WorkerName);
        {'error', Reason}=E ->
            lager:debug("could not terminate event listener ~p for account ~p (~p)", [Pid, AccountId, Reason]),
            E
    end;
stop_event_listener(_, {_, WorkerName}, _) ->
    supervisor:restart_child(?MODULE, WorkerName).

%%
%% supervisor callbacks
%%
        
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    StateMaster = {'ami_sm', {'ami_sm', 'start_link', []}, 'permanent', 'infinity', 'worker', ['ami_sm']},
    Originator = {'amimulator_originator', {'amimulator_originator', 'start_link', []}, 'permanent', 2000, 'worker', ['amimulator_originator']},
    CallSup = {'amimulator_call_sup', {'amimulator_call_sup', 'start_link', []}, 'permanent', 5000, 'supervisor', ['amimulator_call_sup']},
    
    {'ok', {SupFlags, [StateMaster, Originator, CallSup]}}.

%%
%% Private functions
%%

-spec event_listeners() -> [{pid(), term()},...] | [].
event_listeners() -> 
    [{Pid, WorkerName} || {WorkerName, Pid, 'worker', ['amimulator_event_listener']} <- supervisor:which_children(?MODULE)].

-spec find_event_listener(ne_binary()) -> {pid(), term()} | 'undefined'.
-spec find_event_listener(ne_binary(), [{pid(), term()},...] | []) -> {pid(), term()} | 'undefined'.
find_event_listener(AccountId) ->
    find_event_listener(AccountId, event_listeners()).

find_event_listener(_, []) ->
    'undefined';
find_event_listener(AccountId, [{Pid, WorkerName}|Listeners]) ->
    case amimulator_event_listener:account_id(Pid) of
        AccountId -> {Pid, WorkerName};
        _ -> find_event_listener(AccountId, Listeners)
    end.
