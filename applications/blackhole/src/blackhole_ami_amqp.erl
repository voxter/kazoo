-module(blackhole_ami_amqp).

-behaviour(gen_listener).

-export([start_link/1,
        handle_amqp_event/3,
        publish_amqp_event/1
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("blackhole.hrl").
-include_lib("rabbitmq_server/include/rabbit_framing.hrl").

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{blackhole_ami_amqp, handle_amqp_event}
                      ,[{<<"blackhole-ami">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<"blackhole-ami-test">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(EXCHANGE_AMI, <<"ami">>).
-define(TYPE_AMI, <<"topic">>).

-define(HANDLER_MODULES, [
    blackhole_ami_call,
    blackhole_ami_acdc
]).

-record(state, {
    comm_pid
}).

start_link(Pid) ->
    gen_listener:start_link(?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ], [Pid]).

handle_amqp_event(EventJObj, Props, #'basic.deliver'{routing_key=_RoutingKey}) ->
    ParsedEvents = case wh_json:get_value(<<"Events">>, EventJObj) of
        [{Event}] ->
            Event;
        Events ->
            lists:foldl(fun({Event}, Acc) ->
                Acc ++ [Event]
            end, [], Events)
    end,
    lager:debug("AMI published on AMQP ~p", [ParsedEvents]),
    case wh_json:get_value(<<"RequestType">>, EventJObj) of
        <<"publish">> ->
            gen_server:cast(props:get_value(<<"comm_pid">>, Props), {publish, {ParsedEvents, n}});
        _ ->
            ok
    end.
    
publish_amqp_event({publish, Events}=_Req) ->
    {ok, Payload} = wh_api:prepare_api_payload(
        [{<<"RequestType">>, <<"publish">>},
         {<<"Events">>, blackhole_ami_util:format_json_events(Events)} |
         wh_api:default_headers(<<"blackhole-ami">>, <<"events">>, ?APP_NAME, ?APP_VERSION)],
         [], fun amqp_event/1),
    amqp_util:basic_publish(?EXCHANGE_AMI, <<"blackhole-ami.events.test">>, Payload).
    
-define(OPTIONAL_HEADERS, [<<"RequestType">>, <<"Events">>]).
amqp_event(Prop) when is_list(Prop) ->
    wh_api:build_message(Prop, [], ?OPTIONAL_HEADERS).

init([Pid]) ->
    lager:debug("AMI: AMQP listener started with pid ~p", [self()]),
    amqp_util:new_exchange(?EXCHANGE_AMI, ?TYPE_AMI),
    amqp_util:new_queue(?QUEUE_NAME),
    amqp_util:bind_q_to_exchange(?QUEUE_NAME, <<"blackhole-ami.events.test">>, ?EXCHANGE_AMI),
    gen_listener:cast(self(), register_bindings),
    {ok, #state{comm_pid=Pid}}.

handle_call(_Request, _From, State) ->
    lager:debug("AMI: unhandled call"),
    {reply, {error, not_implemented}, State}.

%% Register bindings of handler modules for varying event types
handle_cast(register_bindings, #state{comm_pid=CommPid}=State) ->
    lists:foreach(fun(Module) ->
        Module:init_bindings(CommPid) end, ?HANDLER_MODULES),
    {noreply, State};
handle_cast({gen_listener, {created_queue, _QueueName}}, State) ->
    {noreply, State};
handle_cast({gen_listener, {is_consuming, _IsConsuming}}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:debug("AMI: unhandled cast"),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:debug("AMI: unhandled info"),
    {noreply, State}.
    
handle_event(_JObj, #state{comm_pid=Pid}) ->
    {reply, [{<<"comm_pid">>, Pid}]}.

terminate(Reason, _State) ->
    lager:debug("AMI: AMQP listener on pid ~p terminating: ~p", [self(), Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
