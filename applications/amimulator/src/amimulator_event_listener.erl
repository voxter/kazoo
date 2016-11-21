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
                  ,'amimulator_vm'
                 ]).

-record(state, {account_id :: ne_binary()
                ,pids = [] :: pids()
                ,prune_timer_pid :: api_pid()
                ,extra_props = [] :: proplist()
               }).
-type state() :: #state{}.

%%
%% Public functions
%%

-spec start_link(ne_binary()) -> startlink_ret().
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

-spec account_id(pid()) -> ne_binary().
account_id(Listener) ->
    gen_listener:call(Listener, 'account_id').

-spec handle_amqp_event(kz_json:object(), kz_proplist()) -> 'ok'.
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

-spec publish_amqp_event({atom(), list()}, ne_binary()) -> 'ok'.
publish_amqp_event({_, []}, _) ->
    lager:debug("not publishing empty payload"),
    'ok';
publish_amqp_event({'publish', Events}=_Req, AccountId) ->
    {'ok', Payload} = kz_api:prepare_api_payload(
        [{<<"RequestType">>, <<"publish">>},
         {<<"Events">>, amimulator_util:format_json_events(Events)} |
         kz_api:default_headers(<<"amimulator">>, <<"events">>, ?APP_NAME, ?APP_VERSION)],
         [], fun amqp_event/1),
    amqp_util:basic_publish(?EXCHANGE_AMI, <<"amimulator.events.", AccountId/binary>>, Payload).

-define(OPTIONAL_HEADERS, [<<"RequestType">>, <<"Events">>]).
amqp_event(Prop) when is_list(Prop) ->
    kz_api:build_message(Prop, [], ?OPTIONAL_HEADERS).

%%
%% gen_listener callbacks
%%

-spec init([ne_binary()]) -> {'ok', state()}.
init([AccountId]) ->
    lager:debug("event listener started with pid ~p", [self()]),
    amqp_util:new_exchange(?EXCHANGE_AMI, ?TYPE_AMI),
    ami_sm:init_state(AccountId),

    gen_listener:cast(self(), {'get_module_extra_props', AccountId}),
    gen_listener:cast(self(), {'init_modules', AccountId}),
    {'ok', #state{account_id=AccountId}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call('account_id', _From, #state{account_id=AccountId}=State) ->
    {'reply', AccountId, State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call"),
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
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
                                      ,prune_timer_pid=PrunePid
                                     }=State) ->
    stop_prune_timer(PrunePid),
    {'noreply', State#state{pids = [Pid | Pids], prune_timer_pid='undefined'}};
handle_cast({'unregister', Pid}, #state{account_id=AccountId
                                        ,pids=Pids
                                        ,prune_timer_pid=PrunePid
                                       }=State) ->
    stop_prune_timer(PrunePid),
    NewPids = lists:delete(Pid, Pids),
    NewPrunePid = maybe_start_prune_timer(length(NewPids), AccountId),
    {'noreply', State#state{pids=NewPids, prune_timer_pid=NewPrunePid}};
handle_cast({'gen_listener', {'created_queue', _QueueName}}, #state{account_id=AccountId}=State) ->
    amqp_util:bind_q_to_exchange(?QUEUE_NAME(AccountId), <<"amimulator.events.", AccountId/binary>>, ?EXCHANGE_AMI),

    %% Send fully booted event to client
    Payload = [
        {<<"Event">>, <<"FullyBooted">>},
        {<<"Privilege">>, <<"system,all">>},
        {<<"Status">>, <<"Fully Booted">>}
    ],
    publish_amqp_event({'publish', Payload}, AccountId),

    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast"),
    {'noreply', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
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
terminate('normal', #state{prune_timer_pid=PrunePid}) ->
    stop_prune_timer(PrunePid),
    'ok';
terminate(Reason, #state{prune_timer_pid=PrunePid}) ->
    stop_prune_timer(PrunePid),
    lager:debug("terminating: ~p", [Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%
%% private functions
%%

-spec load_bindings(list()) -> {list(), list()}.
-spec load_bindings(list(), list(), {list(), list()}) -> {list(), list()}.
load_bindings(Props) ->
    load_bindings(Props, ?MODULES, {[], []}).

load_bindings(_, [], Acc) ->
    Acc;
load_bindings(Props, [Mod|Mods], {Bindings, Responders}=Acc) ->
    case Mod:bindings(Props) of
        [] -> load_bindings(Props, Mods, Acc);
        NewBindings ->
            NewResponders = Mod:responders(Props),
            load_bindings(Props, Mods, {NewBindings ++ Bindings, [{{Mod, 'handle_event'}, NewResponders} | Responders]})
    end.

-spec maybe_start_prune_timer(non_neg_integer(), ne_binary()) -> api_pid().
maybe_start_prune_timer(0, AccountId) ->
    case timer:apply_after(?PRUNE_TIMEOUT, 'amimulator_sup', 'stop_event_listener', [AccountId, 'pruned']) of
        {'ok', Pid} ->
            lager:debug("event listener for account ~p to be pruned in ~ps", [AccountId, ?PRUNE_TIMEOUT div 1000]),
            Pid;
        {'error', E} ->
            lager:debug("could not start prune timer for event listener for account ~p (~p)", [AccountId, E]),
            'undefined'
    end;
maybe_start_prune_timer(_, _) ->
    'undefined'.

-spec stop_prune_timer(api_pid()) -> 'ok'.
stop_prune_timer('undefined') ->
    'ok';
stop_prune_timer(Pid) ->
    case timer:cancel(Pid) of
        {'ok', 'cancel'} -> 'ok';
        {'error', E} ->
            lager:debug("could not cancel prune timer ~p (~p)", [Pid, E]),
            'ok'
    end.
