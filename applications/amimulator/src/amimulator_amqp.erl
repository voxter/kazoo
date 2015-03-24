-module(amimulator_amqp).

-behaviour(gen_listener).

-export([start_link/2, handle_amqp_event/3, publish_amqp_event/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-define(BINDINGS, [
    {self, []},
    {notifications, [
        {restrict_to, [deregister]}
    ]}
]).
-define(BINDINGS(A), [
    {acdc_queue, [
        {restrict_to, [member_call]},
        {account_id, A}
    ]},
    {acdc_stats, [
        {restrict_to, [call_stat]},
        {account_id, A}
    ]},
    {registration, [
        {restrict_to, [reg_success]},
        {realm, get_realm(A)}
    ]}
]).
-define(RESPONDERS, [{
    {amimulator_amqp, handle_amqp_event},
    [
        {<<"amimulator">>, <<"*">>},
        {<<"directory">>, <<"reg_success">>},
        {<<"notification">>, <<"deregister">>},
        {<<"member">>, <<"call">>},
        {<<"member">>, <<"call_cancel">>},
        {<<"acdc_call_stat">>, <<"handled">>}
    ]
}]).
-define(QUEUE_NAME, <<"amimulator-queue">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(EXCHANGE_AMI, <<"ami">>).
-define(TYPE_AMI, <<"topic">>).

-define(HANDLER_MODULES, [
    amimulator_call,
    amimulator_acdc,
    amimulator_reg
]).

-record(state, {
    comm_pid
}).

start_link(AccountId, Pid) ->
    gen_listener:start_link(?MODULE
                           ,[{'bindings', ?BINDINGS ++ ?BINDINGS(AccountId)}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ], [AccountId, Pid]).

get_realm(AccountId) ->
    {ok, AccountDoc} = couch_mgr:open_doc(<<"accounts">>, AccountId),
    wh_json:get_value(<<"realm">>, AccountDoc).

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
            gen_server:cast(props:get_value(<<"comm_pid">>, Props), {publish, {ParsedEvents, n}});
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
    amimulator_acdc:handle_event(EventJObj).
    
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

init([AccountId, Pid]) ->
    lager:debug("AMQP listener started with pid ~p", [self()]),
    amqp_util:new_exchange(?EXCHANGE_AMI, ?TYPE_AMI),
    amqp_util:new_queue(?QUEUE_NAME),
    amqp_util:bind_q_to_exchange(?QUEUE_NAME, <<"amimulator.events.test">>, ?EXCHANGE_AMI),
    gen_listener:cast(self(), {init_modules, AccountId}),
    {ok, #state{comm_pid=Pid}}.

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call"),
    {reply, {error, not_implemented}, State}.

%% Register bindings of handler modules for varying event types
handle_cast({init_modules, AccountId}, State) ->
    lists:foreach(fun(Module) ->
        Module:init(AccountId) end,
        ?HANDLER_MODULES
    ),
    %lager:debug("Registering events in amimulator_hook_map"),
    %amimulator_hook_map:register_all(Responders, AccountId, self()),
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
    
handle_event(_JObj, #state{comm_pid=Pid}) ->
    {reply, [{<<"comm_pid">>, Pid}]}.

terminate(Reason, _State) ->
    lager:debug("AMQP listener on pid ~p terminating: ~p", [self(), Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
