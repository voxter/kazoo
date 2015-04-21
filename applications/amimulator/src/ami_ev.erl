-module(ami_ev).

-behaviour(gen_listener).

-export([maybe_start/1, start_link/1, handle_amqp_event/3, publish_amqp_event/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-define(QUEUE_NAME, <<"amimulator-queue">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, [{exclusive, false}]).

-define(EXCHANGE_AMI, <<"ami">>).
-define(TYPE_AMI, <<"topic">>).

-define(MODULES, [
    amimulator_call,
    amimulator_acdc,
    amimulator_reg,
    amimulator_conf
]).

-record(state, {
    account_id
}).

%%
%% Public functions
%%

%% Start an event consumer for the account, or restart a paused one
maybe_start(AccountId) ->
    case amimulator_sup:find_ev(AccountId) of
        undefined ->
            amimulator_sup:start_ev(AccountId);
        _EvPid ->
            ami_sm:ev_staying_up(AccountId)
    end,

    %% Send fully booted event to client
    Payload = [
        {<<"Event">>, <<"FullyBooted">>},
        {<<"Privilege">>, <<"system,all">>},
        {<<"Status">>, <<"Fully Booted">>}
    ],
    gen_server:cast(self(), {publish, {Payload, n}}).

start_link(AccountId) ->
    Props = [
        {"AccountId", AccountId}
    ],
    {Bindings, Responders} = lists:foldl(fun(Mod, {Acc1, Acc2}) ->
        {Mod:bindings(Props) ++ Acc1, Mod:responders(Props) ++ Acc2}
        end, {[], []}, ?MODULES),

    gen_listener:start_link(
        ?MODULE,
        [
            {'bindings', [{self, []}] ++ Bindings},
            {'responders', [{{?MODULE, handle_amqp_event},
                [{<<"amimulator">>, <<"*">>}] ++ Responders}]},
            {'queue_name', ?QUEUE_NAME},       % optional to include
            {'queue_options', ?QUEUE_OPTIONS}, % optional to include
            {'consume_options', ?CONSUME_OPTIONS} % optional to include
        ],
        [AccountId]
    ).

handle_amqp_event(EventJObj, Props, #'basic.deliver'{routing_key=RoutingKey}) ->
    handle_amqp_event_type(EventJObj, Props, RoutingKey).

handle_amqp_event_type(EventJObj, Props, <<"amimulator.events.test">>) ->
    ParsedEvents = case wh_json:get_value(<<"Events">>, EventJObj) of
        [{Event}] ->
            Event;
        Events ->
            lists:foldl(fun({Event}, Acc) ->
                Acc ++ [Event]
            end, [], Events)
    end,

    %lager:debug("AMI published on AMQP ~p", [ParsedEvents]),
    case wh_json:get_value(<<"RequestType">>, EventJObj) of
        <<"publish">> ->
            %% Figure out which comm processes need to be published to
            AccountId = props:get_value(<<"AccountId">>, Props),
            CommPids = ami_sm:account_consumers(AccountId),

            lists:foreach(fun(CommPid) ->
                gen_server:cast(CommPid, {publish, {ParsedEvents, n}})
            end, CommPids);
        _ ->
            ok
    end;

handle_amqp_event_type(EventJObj, _Props, <<"registration.success.", _/binary>>) ->
    amimulator_reg:handle_event(EventJObj);

handle_amqp_event_type(EventJObj, _Props, <<"notifications.sip.deregister">>) ->
    amimulator_reg:handle_event(EventJObj);

handle_amqp_event_type(EventJObj, _Props, <<"acdc.member.call.", _/binary>>) ->
    amimulator_acdc:handle_event(EventJObj);

handle_amqp_event_type(EventJObj, _Props, <<"acdc_stats.call.", _/binary>>) ->
    amimulator_acdc:handle_event(EventJObj);

handle_amqp_event_type(EventJObj, Props, <<"conference.event.", _/binary>>) ->
    AccountId = props:get_value(<<"AccountId">>, Props),
    amimulator_conf:handle_event(wh_json:set_value(<<"Account-ID">>, AccountId, EventJObj));

handle_amqp_event_type(EventJObj, _Props, <<"acdc.agent.action.", _/binary>>) ->
    amimulator_acdc:handle_event(EventJObj);

handle_amqp_event_type(_EventJObj, _Props, _RoutingKey) ->
    ok.
    
publish_amqp_event({_, []}) ->
    lager:debug("Not publishing empty payload"),
    ok;
publish_amqp_event({publish, Events}=_Req) ->
    {ok, Payload} = wh_api:prepare_api_payload(
        [{<<"RequestType">>, <<"publish">>},
         {<<"Events">>, amimulator_util:format_json_events(Events)} |
         wh_api:default_headers(<<"amimulator">>, <<"events">>, ?APP_NAME, ?APP_VERSION)],
         [], fun amqp_event/1),
    amqp_util:basic_publish(?EXCHANGE_AMI, <<"amimulator.events.test">>, Payload).
    
-define(OPTIONAL_HEADERS, [<<"RequestType">>, <<"Events">>]).
amqp_event(Prop) when is_list(Prop) ->
    wh_api:build_message(Prop, [], ?OPTIONAL_HEADERS).

init([AccountId]) ->
    lager:debug("AMQP listener started with pid ~p", [self()]),
    amqp_util:new_exchange(?EXCHANGE_AMI, ?TYPE_AMI),
    amqp_util:new_queue(?QUEUE_NAME),
    amqp_util:bind_q_to_exchange(?QUEUE_NAME, <<"amimulator.events.test">>, ?EXCHANGE_AMI),
    gen_listener:cast(self(), {init_modules, AccountId}),
    {ok, #state{account_id=AccountId}}.

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call"),
    {reply, {error, not_implemented}, State}.

%% Register bindings of handler modules for varying event types
handle_cast({init_modules, AccountId}, State) ->
    lists:foreach(fun(Module) ->
        Module:init(AccountId) end,
        ?MODULES
    ),
    {noreply, State};
handle_cast({handle, Mod, Fun, Params}, State) ->
    Mod:Fun(Params),
    {noreply, State};
handle_cast({gen_listener, {created_queue, _QueueName}}, State) ->
    {noreply, State};
handle_cast({gen_listener, {is_consuming, _IsConsuming}}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast"),
    {noreply, State}.

handle_info(?HOOK_EVT(_AccountId, _EventType, JObj), State) ->
    spawn(amimulator_call, 'handle_event', [JObj]),
    {noreply, State};
handle_info(_Info, State) ->
    %lager:debug("unhandled info"),
    {noreply, State}.
    
handle_event(_JObj, #state{account_id=AccountId}) ->
    {reply, [{<<"AccountId">>, AccountId}]}.

terminate(Reason, _State) ->
    lager:debug("AMQP listener on pid ~p terminating: ~p", [self(), Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
