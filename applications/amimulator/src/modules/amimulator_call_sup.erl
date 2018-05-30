%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(amimulator_call_sup).

-behaviour(supervisor).

-include("../amimulator.hrl").

-export([start_link/0]).
-export([relay_new_call/1
        ,relay_answer/1
        ,relay_bridge/1
        ,relay_destroy/2
        ,initial_call/1
        ]).
-export([init/1]).

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec relay_new_call(amimulator_call:call()) -> 'ok'.
relay_new_call(Call) ->
    FSM2 = case find_call_handler('fsm', Call) of
               'undefined' -> try_create_call_handlers(Call);
               FSM -> FSM
           end,
    relay_new_call(FSM2, Call).

-spec relay_answer(kz_term:ne_binary()) -> 'ok'.
relay_answer(CallId) ->
    lists:foreach(fun(Pid) ->
                          amimulator_call_fsm:answer(Pid, CallId)
                  end, find_call_handlers(CallId)).

-spec relay_bridge(kz_json:object()) -> 'ok'.
relay_bridge(EventJObj) ->
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    lists:foreach(fun(Pid) ->
                          amimulator_call_fsm:bridge(Pid, EventJObj)
                  end, find_call_handlers(CallId)).

-spec relay_destroy(kz_term:api_binary(), kz_term:ne_binary()) -> 'ok'.
relay_destroy(Reason, CallId) ->
    lists:foreach(fun(Pid) ->
                          amimulator_call_fsm:destroy(Pid, Reason, CallId)
                  end, find_call_handlers(CallId)).

-spec initial_call(amimulator_call:call()) -> 'ok'.
initial_call(Call) ->
    case find_call_handler('fsm', Call) of
        'undefined' -> create_call_fsm(Call, 'initial');
        FSM -> amimulator_call_fsm:add_initial(FSM, Call)
    end.

%%
%% supervisor callbacks
%%

-spec init([]) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?WORKER_ARGS_TYPE('amimulator_call_fsm', [self()], 'transient')]}}.

%%
%% private functions
%%

find_call_handlers(CallId) ->
    lists:foldl(fun({Pid, _}, Pids) ->
                        case catch amimulator_call_fsm:accepts(Pid, CallId) of
                            'true' -> [Pid | Pids];
                            'false' -> Pids;
                            _E ->
                                                % lager:debug("couldn't get call handler (~p)", [E]),
                                Pids
                        end
                end, [], fsms()).

-spec find_call_handler(atom(), amimulator_call:call()) -> kz_term:api_pid().
find_call_handler('fsm', Call) ->
    find_typed_call_handler(Call, fsms()).

-spec find_typed_call_handler(amimulator_call:call(), [{pid(), atom()},...] | []) -> kz_term:api_pid().
find_typed_call_handler(_, []) ->
    'undefined';
find_typed_call_handler(Call, [{Pid, Mod}|Handlers]) ->
    case catch Mod:monitoring(Pid, Call) of
        'true' -> Pid;
        'false' -> find_typed_call_handler(Call, Handlers);
        _E -> find_typed_call_handler(Call, Handlers)
    end.

                                                % -spec workers() -> kz_term:pids().
                                                % workers() -> [Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(?MODULE)].

-spec fsms() -> [{pid(), atom()},...] | [].
fsms() -> [{Pid, 'amimulator_call_fsm'} || {_, Pid, 'worker', ['amimulator_call_fsm']} <- supervisor:which_children(?MODULE)].

-spec try_create_call_handlers(amimulator_call:call()) -> kz_term:api_pid().
try_create_call_handlers(Call) ->
    case create_call_fsm(Call) of
        'undefined' -> 'undefined';
        FSM -> FSM
    end.

-spec create_call_fsm(amimulator_call:call()) -> kz_term:api_pid().
create_call_fsm(Call) ->
    create_call_fsm(Call, 'undefined').

-spec create_call_fsm(amimulator_call:call(), 'undefined' | 'initial') -> kz_term:api_pid().
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
    end.

-spec relay_new_call(kz_term:api_pid(), amimulator_call:call()) -> 'ok'.
relay_new_call('undefined', _) ->
    'ok';
relay_new_call(FSM, Call) ->
    amimulator_call_fsm:new_call(FSM, Call).
