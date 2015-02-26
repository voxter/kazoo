-module(blackhole_ami_acdc).

-export([init_bindings/0, handle_event/2]).

-include("blackhole.hrl").

init_bindings() ->
    AgentSup = acdc_agents_sup:find_agent_supervisor(<<"c6795c40e9c4d3afa5eb7bcbbc30fcdc">>, <<"14ea55901cfb1d85486867f42589df6c">>),
    AgentListener = acdc_agent_sup:listener(AgentSup),
    
    gen_listener:add_responder(
        AgentListener,
        {'blackhole_ami_acdc', 'handle_event'},
        [{<<"member">>, <<"*">>}]
    ),
    gen_listener:add_responder(
        acdc_agent_manager,
        {'acdc_agent_handler', 'handle_event'},
        [
            {<<"agent">>, <<"login">>},
            {<<"agent">>, <<"logout">>},
            {<<"agent">>, <<"pause">>},
            {<<"agent">>, <<"resume">>}
        ]
    ),
    %blackhole_bindings:bind(<<"member.*">>, ?MODULE, handle_event),
    ok.

handle_event(EventJObj, _Props) ->
    %lager:debug("AMI: got event ~p", [EventJObj]),
    %lager:debug("AMI: got props ~p", [Props]),
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).
    
handle_specific_event(<<"connect_req">>, EventJObj) ->
    AgentChannelId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    %CallerId = wh_json:get_value(<<"Caller-ID-Number">>, EventJObj),
    %MsgId = wh_json:get_value(<<"Msg-ID">>, EventJObj),

    Payload = <<"Event: AgentCalled",
        "\nAgentCalled: ",
        AgentChannelId/binary,
        "\nChannelCalling: ",
        "\nCallerID: ",
        %CallerId/binary,
        "\nContext: ",
        %MsgId/binary,
        "\nExtension: ",
        "\nPriority: ",
        "\n\n">>,
    
    gen_listener:cast(blackhole_ami_amqp, {out, Payload});
handle_specific_event(<<"login">>, _EventJObj) ->
%
%
%
%
    ok;
handle_specific_event(_, _EventJObj) ->
    lager:debug("AMI: unhandled acdc event").