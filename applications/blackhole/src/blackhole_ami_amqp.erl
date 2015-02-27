-module(blackhole_ami_amqp).

-behaviour(gen_listener).

-export([start_link/1
         ,handle_amqp_event/3
         ,handle_event/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("blackhole.hrl").
-include_lib("rabbitmq_server/include/rabbit_framing.hrl").

-define(BINDINGS, []).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(HANDLER_MODULES, [
    blackhole_ami_call,
    blackhole_ami_acdc
]).

-record(state, {
}).

start_link(Socket) ->
    gen_listener:start_link({'local', ?MODULE}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ], [Socket]).

%% Event handlers for calls are registered in blackhole_bindings,
%% mapped to blackhole_ami_call
handle_amqp_event(EventJObj, Props, #'basic.deliver'{routing_key=RoutingKey}) ->
    handle_amqp_event(EventJObj, Props, RoutingKey);
handle_amqp_event(EventJObj, _Props, <<_/binary>> = RoutingKey) ->
    %lager:debug("AMI: handling amqp event of type ~p (routing key ~s)", [wh_util:get_event_type(EventJObj), RoutingKey]),
    blackhole_bindings:map(RoutingKey, EventJObj).
    
%% Possibly legacy handling code, may remove later
handle_event(undefined, EventJObj) ->
    lager:debug("AMI: handling event ~p", [EventJObj]),
    %gen_tcp:send(Socket, io_lib:format("~p", [EventJObj])),
    ok.

init([]) ->
    lager:debug("AMI: amqp sniffer init"),
    gen_listener:cast(?MODULE, register_bindings),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    lager:debug("AMI: unhandled call"),
    {reply, {error, not_implemented}, State}.

%% Register bindings of handler modules for varying event types
handle_cast(register_bindings, State) ->
    lists:foreach(fun(Module) ->
        Module:init_bindings() end, ?HANDLER_MODULES),
    {noreply, State};
handle_cast({add_call_binding, AccountId}, State) ->
    lager:debug("AMI: Registering for call bindings on account: ~p", [AccountId]),
    wh_hooks:register(AccountId),
    {noreply, State};
handle_cast({gen_listener, {created_queue, _QueueName}}, State) ->
    {noreply, State};
handle_cast({gen_listener, {is_consuming, _IsConsuming}}, State) ->
    {noreply, State};
handle_cast(Msg, State) ->
    lager:debug("AMI: unhandled cast msg: ~p", [Msg]),
    {noreply, State}.

handle_info(?HOOK_EVT(_AccountId, EventType, JObj), State) ->
    spawn(?MODULE, handle_amqp_event, [JObj, [], call_routing(EventType, JObj)]),
    {noreply, State};
handle_info(_Info, State) ->
    lager:debug("AMI: unhandled info"),
    {noreply, State}.
    
call_routing(EventType, JObj) ->
    wapi_call:event_routing_key(EventType, wh_json:get_value(<<"Call-ID">>, JObj)).

terminate(_Reason, _State) ->
    lager:debug("AMI: amqp sniffer terminating: ~p", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
