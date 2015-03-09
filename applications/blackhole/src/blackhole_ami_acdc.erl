-module(blackhole_ami_acdc).

-export([init_bindings/1, handle_event/2]).

-include("blackhole.hrl").

init_bindings(CommPid) ->
    AccountId = gen_server:call(CommPid, account_id),
    QueueSups = acdc_queues_sup:find_acct_supervisors(AccountId),

    lists:foreach(fun(QueueSup) ->
        init_queue_bindings(QueueSup) end, QueueSups
    ),
    %AgentSup = acdc_agents_sup:find_agent_supervisor(<<"c6795c40e9c4d3afa5eb7bcbbc30fcdc">>, <<"14ea55901cfb1d85486867f42589df6c">>),
    %AgentListener = acdc_agent_sup:listener(AgentSup),
    
    %gen_listener:add_responder(
    %    AgentListener,
    %    {'blackhole_ami_acdc', 'handle_event'},
    %    [{<<"member">>, <<"*">>}]
    %),
    %gen_listener:add_responder(
    %    acdc_agent_manager,
    %    {'blackhole_ami_acdc', 'handle_event'},
    %    [
    %        {<<"agent">>, <<"login">>},
    %        {<<"agent">>, <<"logout">>},
    %        {<<"agent">>, <<"pause">>},
    %        {<<"agent">>, <<"resume">>}
    %    ]
    %),
    %blackhole_bindings:bind(<<"member.*">>, ?MODULE, handle_event),
    ok.

init_queue_bindings(QueueSup) ->
    Manager = acdc_queue_sup:manager(QueueSup),
    gen_listener:add_responder(
        Manager,
        {blackhole_ami_acdc, handle_event},
        [
            {<<"member">>, <<"call">>},
            {<<"member">>, <<"call_cancel">>}
        ]
    ).

handle_event(EventJObj, _Props) ->
    %lager:debug("AMI: got event ~p", [EventJObj]),
    %lager:debug("AMI: got props ~p", [Props]),
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).
    
handle_specific_event(<<"call">>, EventJObj) ->
    EventData = wh_json:get_value(<<"Call">>, EventJObj),
    %lager:debug("AMI: member call event ~p", [EventData]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventData),
    ToUser = wh_json:get_value(<<"To-User">>, EventData),
    CallerIdNum = wh_json:get_value(<<"Caller-ID-Number">>, EventData),
    CallerIdName = wh_json:get_value(<<"Caller-ID-Name">>, EventData),

    Call = blackhole_ami_util:whapps_call(EventData),
    Payload = case cf_endpoint:get(Call) of
        {error, _E} ->
            [
                {<<"Event">>, <<"Join">>},
                {<<"Privilege">>, <<"call,all">>},
                %%{<<"Channel">>, <<"SIP/101-00000000">>},
                {<<"CallerIDNum">>, CallerIdNum},
                {<<"CallerIDName">>, CallerIdName},
                {<<"ConnectedLineNum">>, <<"unknown">>},
                {<<"ConnectedLineName">>, <<"unknown">>},
                {<<"Queue">>, ToUser},
                {<<"Position">>, 1},
                {<<"Count">>, 1},
                {<<"Uniqueid">>, CallId}
            ];
        {ok, Endpoint} ->
            EndpointName = blackhole_ami_util:endpoint_name(whapps_call:account_db(Call), Endpoint),
            [
                {<<"Event">>, <<"Join">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, <<"SIP/", EndpointName/binary, "-00000004">>},
                {<<"CallerIDNum">>, CallerIdNum},
                {<<"CallerIDName">>, CallerIdName},
                {<<"ConnectedLineNum">>, <<"unknown">>},
                {<<"ConnectedLineName">>, <<"unknown">>},
                {<<"Queue">>, ToUser},
                {<<"Position">>, 1},
                {<<"Count">>, 1},
                {<<"Uniqueid">>, CallId}
            ]
    end,
    blackhole_ami_amqp:publish_amqp_event({publish, Payload});
handle_specific_event(<<"call_cancel">>, EventJObj) ->
    %lager:debug("AMI: member call cancel event ~p", [EventJObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    ToUser = wh_json:get_value(<<"To-User">>, EventJObj),

    Payload = [[
        {<<"Event">>, <<"QueueCallerAbandon">>},
        {<<"Privilege">>, <<"agent,all">>},
        {<<"Queue">>, ToUser},
        {<<"Uniqueid">>, CallId},
        {<<"Position">>, 1},
        {<<"OriginalPosition">>, 1},
        {<<"HoldTime">>, 14}
    ],[
        {<<"Event">>, <<"Leave">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, <<"SIP/101-00000000">>},
        {<<"Queue">>, ToUser},
        {<<"Count">>, 0},
        {<<"Position">>, 1},
        {<<"Uniqueid">>, CallId}
    ]],
    blackhole_ami_amqp:publish_amqp_event({publish, Payload});
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