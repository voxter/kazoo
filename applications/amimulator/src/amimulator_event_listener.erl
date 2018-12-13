%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(amimulator_event_listener).

-behaviour(gen_listener).

-export([start_link/1
        ,register/2, unregister/2
        ,account_id/1
        ,handle_amqp_event/2, publish_amqp_event/2
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-define(BINDINGS(AccountId), [{'self', []}
                             ]).
-define(RESPONDERS, [{{?MODULE, 'handle_amqp_event'}
                     ,[{<<"amimulator">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME(AccountId), <<"amimulator-queue", AccountId/binary>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(EXCHANGE_AMI, <<"ami">>).
-define(TYPE_AMI, <<"topic">>).

-define(PRUNE_TIMEOUT, 60000).

-define(MODULES, ['amimulator_call_hook'
                 ,'amimulator_acdc'
                 ,'amimulator_reg'
                 ,'amimulator_conf'
                 ,'amimulator_presence'
                 ,'amimulator_vm'
                 ,'amimulator_originate_resp'
                 ]).

-record(state, {account_id :: kz_term:ne_binary()
               ,pids = [] :: kz_term:pids()
               ,prune_timer_ref :: timer:tref() | 'undefined'
               ,extra_props = [] :: kz_term:proplist()
               ,server_id :: kz_term:api_ne_binary()
               }).
-type state() :: #state{}.

%%
%% Public functions
%%

-spec start_link(kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(AccountId) ->
    Props = [{"AccountId", AccountId}],
    {Bindings, Responders} = load_bindings(Props),

    gen_listener:start_link(?MODULE
                           ,[{'bindings', ?BINDINGS(AccountId) ++ Bindings}
                            ,{'responders', ?RESPONDERS ++ Responders}
                            ,{'queue_name', ?QUEUE_NAME(AccountId)}
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ]
                           ,[AccountId]
                           ).

-spec register(pid(), pid()) -> 'ok'.
register(Listener, Consumer) ->
    gen_listener:cast(Listener, {'register', Consumer}).

-spec unregister(pid(), pid()) -> 'ok'.
unregister(Listener, Consumer) ->
    gen_listener:cast(Listener, {'unregister', Consumer}).

-spec account_id(pid()) -> kz_term:ne_binary().
account_id(Listener) ->
    gen_listener:call(Listener, 'account_id').

-spec handle_amqp_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_amqp_event(EventJObj, Props) ->
    ParsedEvents = case kz_json:get_value(<<"Events">>, EventJObj) of
                       [{Event}] ->
                           Event;
                       Events ->
                           lists:foldl(fun({Event}, Acc) ->
                                               Acc ++ [Event]
                                       end, [], Events)
                   end,

    case kz_json:get_value(<<"RequestType">>, EventJObj) of
        <<"publish">> ->
            lists:foreach(fun(Pid) ->
                                  gen_server:cast(Pid, {'publish', {ParsedEvents, 'n'}})
                          end, props:get_value(<<"Pids">>, Props));
        _ ->
            'ok'
    end.

-spec publish_amqp_event({atom(), list()}, kz_term:ne_binary()) -> 'ok'.
publish_amqp_event({_, []}, _) ->
    lager:debug("not publishing empty payload"),
    'ok';
publish_amqp_event({'publish', Events}=_Req, AccountId) ->
    {'ok', Payload} = kz_api:prepare_api_payload(
                        [{<<"RequestType">>, <<"publish">>},
                         {<<"Events">>, amimulator_util:format_json_events(Events)} |
                         kz_api:default_headers(<<"amimulator">>, <<"events">>, ?APP_NAME, ?APP_VERSION)],
                        [], fun amqp_event/1),
    kz_amqp_util:basic_publish(?EXCHANGE_AMI, <<"amimulator.events.", AccountId/binary>>, Payload).

-define(OPTIONAL_HEADERS, [<<"RequestType">>, <<"Events">>]).
amqp_event(Prop) when is_list(Prop) ->
    kz_api:build_message(Prop, [], ?OPTIONAL_HEADERS).

%%
%% gen_listener callbacks
%%

-spec init([kz_term:ne_binary()]) -> {'ok', state()}.
init([AccountId]) ->
    lager:debug("event listener started with pid ~p", [self()]),
    kz_amqp_util:new_exchange(?EXCHANGE_AMI, ?TYPE_AMI),
    ami_sm:init_state(AccountId),

    gen_listener:cast(self(), {'get_module_extra_props', AccountId}),
    gen_listener:cast(self(), {'init_modules', AccountId}),
    {'ok', #state{account_id=AccountId}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('account_id', _From, #state{account_id=AccountId}=State) ->
    {'reply', AccountId, State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call"),
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'get_module_extra_props', AccountId}, State) ->
    Props = lists:foldl(fun(Module, Acc) ->
                                case erlang:function_exported(Module, 'get_extra_props', 1) of
                                    'true' -> Module:get_extra_props(AccountId) ++ Acc;
                                    'false' -> Acc
                                end
                        end, [], ?MODULES),
    {'noreply', State#state{extra_props=Props}};
%% Register bindings of handler modules for varying event types
handle_cast({'init_modules', AccountId}, State) ->
    lists:foreach(fun(Module) ->
                          Module:init(AccountId) end,
                  ?MODULES
                 ),
    {'noreply', State};
handle_cast({'register', Pid}, #state{pids=Pids
                                     ,prune_timer_ref=PruneRef
                                     ,server_id=ServerId
                                     }=State) ->
    stop_prune_timer(PruneRef),
    Pids1 = [Pid | Pids],
    notify_of_server_id(ServerId, Pids1),
    {'noreply', State#state{pids=Pids1
                           ,prune_timer_ref='undefined'
                           }};
handle_cast({'unregister', Pid}, #state{account_id=AccountId
                                       ,pids=Pids
                                       ,prune_timer_ref=PruneRef
                                       }=State) ->
    stop_prune_timer(PruneRef),
    NewPids = lists:delete(Pid, Pids),
    NewPruneRef = maybe_start_prune_timer(length(NewPids), AccountId),
    {'noreply', State#state{pids=NewPids, prune_timer_ref=NewPruneRef}};
handle_cast({'gen_listener', {'created_queue', QueueName}}, #state{account_id=AccountId
                                                                  ,pids=Pids
                                                                  }=State) ->
    kz_amqp_util:bind_q_to_exchange(?QUEUE_NAME(AccountId), <<"amimulator.events.", AccountId/binary>>, ?EXCHANGE_AMI),

    %% Send fully booted event to client
    Payload = [
               {<<"Event">>, <<"FullyBooted">>},
               {<<"Privilege">>, <<"system,all">>},
               {<<"Status">>, <<"Fully Booted">>}
              ],
    publish_amqp_event({'publish', Payload}, AccountId),

    notify_of_server_id(QueueName, Pids),

    {'noreply', State#state{server_id=QueueName}};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast"),
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(?HOOK_EVT(_AccountId, _EventType, JObj), State) ->
    amimulator_call_hook:handle_event(JObj),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{account_id=AccountId
                          ,pids=Pids
                          ,extra_props=ExtraProps
                          }) ->
    {'reply', [{<<"AccountId">>, AccountId}
              ,{<<"Pids">>, Pids}
               | ExtraProps
              ]}.

-spec terminate(any(), state()) -> 'ok'.
terminate('normal', #state{prune_timer_ref=PruneRef}) ->
    stop_prune_timer(PruneRef),
    'ok';
terminate(Reason, #state{prune_timer_ref=PruneRef}) ->
    stop_prune_timer(PruneRef),
    lager:debug("terminating: ~p", [Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%
%% private functions
%%

-spec load_bindings(list()) -> {list(), list()}.
load_bindings(Props) ->
    load_bindings(Props, ?MODULES, {[], []}).

-spec load_bindings(list(), list(), {list(), list()}) -> {list(), list()}.
load_bindings(_, [], Acc) ->
    Acc;
load_bindings(Props, [Mod|Mods], {Bindings, Responders}=Acc) ->
    NewBindings = Mod:bindings(Props),
    NewResponders = Mod:responders(Props),
    case NewResponders of
        [] -> load_bindings(Props, Mods, Acc);
        _ ->
            load_bindings(Props
                         ,Mods
                         ,{NewBindings ++ Bindings
                          ,[{{Mod, 'handle_event'}, NewResponders} | Responders]
                          })
    end.

-spec maybe_start_prune_timer(non_neg_integer(), kz_term:ne_binary()) ->
                                     timer:tref() | 'undefined'.
maybe_start_prune_timer(0, AccountId) ->
    case timer:apply_after(?PRUNE_TIMEOUT, 'amimulator_sup', 'stop_event_listener', [AccountId, 'pruned']) of
        {'ok', PruneRef} ->
            lager:debug("event listener for account ~p to be pruned in ~ps", [AccountId, ?PRUNE_TIMEOUT div 1000]),
            PruneRef;
        {'error', E} ->
            lager:debug("could not start prune timer for event listener for account ~p (~p)", [AccountId, E]),
            'undefined'
    end;
maybe_start_prune_timer(_, _) ->
    'undefined'.

-spec stop_prune_timer(timer:tref() | 'undefined') -> 'ok'.
stop_prune_timer('undefined') ->
    'ok';
stop_prune_timer(PruneRef) ->
    case timer:cancel(PruneRef) of
        {'ok', 'cancel'} -> 'ok';
        {'error', E} ->
            lager:debug("could not cancel prune timer ~p (~p)", [PruneRef, E]),
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Notify all processes listening using this event listener of a
%% new AMQP queue (Server-ID) to include in command props
%% @end
%%------------------------------------------------------------------------------
-spec notify_of_server_id(kz_term:api_ne_binary(), kz_term:pids()) -> 'ok'.
notify_of_server_id('undefined', _) -> 'ok';
notify_of_server_id(ServerId, Pids) ->
    lists:foreach(fun(Pid) ->
                          gen_server:cast(Pid, {'set_server_id', ServerId})
                  end
                 ,Pids).
