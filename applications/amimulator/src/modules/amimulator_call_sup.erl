-module(amimulator_call_sup).

-behaviour(supervisor).

-include("../amimulator.hrl").

-export([start_link/0]).
-export([relay_new_call/1
         ,relay_answer/1
         ,relay_bridge/2
         ,relay_destroy/2
         ,relay_join_queue/2
         ,initial_call/1
        ]).
-export([init/1]).

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec relay_new_call(amimulator_call:call()) -> 'ok'.
relay_new_call(Call) ->
    FSM2 = case find_call_handler('fsm', Call) of
        'undefined' -> try_create_call_handlers(Call);
        FSM -> FSM
    end,
    relay_new_call(FSM2, Call).

-spec relay_answer(amimulator_call:call()) -> 'ok'.
relay_answer(Call) ->
    amimulator_call_fsm:answer(find_call_handler('fsm', Call), Call).

-spec relay_bridge(amimulator_call:call(), amimulator_call:call()) -> 'ok'.
relay_bridge(Call, OtherCall) ->
    amimulator_call_fsm:bridge(find_call_handler('fsm', Call), Call, OtherCall).

-spec relay_destroy(api_binary(), amimulator_call:call()) -> 'ok'.
relay_destroy(Reason, Call) ->
    amimulator_call_fsm:destroy(find_call_handler('fsm', Call), Reason, Call).

-spec relay_join_queue(api_binary(), amimulator_call:call()) -> 'ok'.
relay_join_queue(QueueId, Call) ->
    amimulator_call_fsm:join_queue(find_call_handler('fsm', Call), QueueId, Call).

-spec initial_call(amimulator_call:call()) -> 'ok'.
initial_call(Call) ->
    create_call_fsm(Call, 'initial').

%%
%% supervisor callbacks
%%

init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?WORKER_ARGS_TYPE('amimulator_call_fsm', [self()], 'transient')]}}.

%%
%% private functions
%%

-spec find_call_handler(atom(), amimulator_call:call()) -> api_pid().
find_call_handler('fsm', Call) ->
    find_typed_call_handler(Call, fsms()).

-spec find_typed_call_handler(amimulator_call:call(), [{pid(), atom()},...] | []) -> api_pid().
find_typed_call_handler(_, []) ->
    'undefined';
find_typed_call_handler(Call, [{Pid, Mod}|Handlers]) ->
    case catch Mod:monitoring(Pid, Call) of
        'true' -> Pid;
        'false' -> find_typed_call_handler(Call, Handlers);
        _E -> find_typed_call_handler(Call, Handlers)
    end.

-spec workers() -> pids().
workers() -> [Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(?MODULE)].

-spec fsms() -> [{pid(), atom()},...] | [].
fsms() -> 
    % lager:debug("children ~p", [supervisor:which_children(?MODULE)]),
    [{Pid, 'amimulator_call_fsm'} || {_, Pid, 'worker', ['amimulator_call_fsm']} <- supervisor:which_children(?MODULE)].

-spec try_create_call_handlers(amimulator_call:call()) -> api_pid().
try_create_call_handlers(Call) ->
    case create_call_fsm(Call) of
        'undefined' -> 'undefined';
        FSM -> FSM
    end.

-spec create_call_fsm(amimulator_call:call()) -> api_pid().
-spec create_call_fsm(amimulator_call:call(), 'undefined' | 'initial')
create_call_fsm(Call) ->
    create_call_fsm(Call, 'undefined').

create_call_fsm(Call, 'undefined') ->
    case supervisor:start_child(?MODULE, [Call]) of
        {'ok', Pid} -> Pid;
        {'ok', Pid, _} -> Pid;
        {'error', E} ->
            lager:debug("could not create fsm for id ~p (~p)", [amimulator_call:call_id(Call), E]),
            'undefined'
    end;
create_call_fsm(Call, 'initial') ->
    case supervisor:start_child(?MODULE, [Call, 'initial']) of
        {'ok', Pid} -> Pid;
        {'ok', Pid, _} -> Pid;
        {'error', E} ->
            lager:debug("could not create fsm for id ~p (~p)", [amimulator_call:call_id(Call), E]),
            'undefined'
    end;

-spec relay_new_call(api_pid(), amimulator_call:call()) -> 'ok'.
relay_new_call('undefined', _) ->
    'ok';
relay_new_call(FSM, Call) ->
    amimulator_call_fsm:new_call(FSM, Call).
