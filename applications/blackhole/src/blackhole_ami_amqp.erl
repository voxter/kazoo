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

-record(state, {
    socket
}).

-include("blackhole.hrl").
-include_lib("rabbitmq_server/include/rabbit_framing.hrl").

-define(BINDINGS, []).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

start_link(Socket) ->
    gen_listener:start_link({'local', ?MODULE}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ], [Socket]).

handle_amqp_event(EventJObj, Props, #'basic.deliver'{routing_key=RoutingKey}) ->
    handle_amqp_event(EventJObj, Props, RoutingKey);
handle_amqp_event(EventJObj, _Props, <<_/binary>> = RoutingKey) ->
    %lager:debug("AMI: handling amqp event of type ~p (routing key ~s)", [wh_util:get_event_type(EventJObj), RoutingKey]),
    blackhole_bindings:map(RoutingKey, EventJObj).
    
handle_event(undefined, EventJObj) ->
    lager:debug("AMI: handling event ~p", [EventJObj]),
    %gen_tcp:send(Socket, io_lib:format("~p", [EventJObj])),
    ok.
    %wh_util:put_callid(EventJObj),
    %lager:debug("AMI: handle_event fired for ~s ~s", [bh_context:account_id(Context), bh_context:websocket_session_id(Context)]),
    %blackhole_data_emitter:emit(bh_context:websocket_pid(Context), event_name(EventJObj), EventJObj).

init([Socket]) ->
    lager:debug("AMI: amqp sniffer init with socket"),
    gen_listener:cast(?MODULE, register_bindings),
    {ok, #state{socket=Socket}}.

handle_call(_Request, _From, State) ->
    lager:debug("AMI: unhandled call"),
    {reply, {error, not_implemented}, State}.

handle_cast(register_bindings, State) ->
    blackhole_ami_translator:init(),
    {noreply, State};
handle_cast({add_call_binding, AccountId}, State) ->
    lager:debug("AMI: Registering for call bindings on account: ~p", [AccountId]),
    wh_hooks:register(AccountId),
    {noreply, State};
handle_cast({out, Message}, #state{socket=Socket}=State) ->
    gen_tcp:send(Socket, Message),
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
