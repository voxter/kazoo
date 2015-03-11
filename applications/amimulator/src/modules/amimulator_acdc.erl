-module(amimulator_acdc).

-export([init_bindings/1, handle_event/2]).

-include("../amimulator.hrl").

init_bindings(CommPid) ->
    AccountId = gen_server:call(CommPid, account_id),
    QueueSups = acdc_queues_sup:find_acct_supervisors(AccountId),

    lists:foreach(fun(QueueSup) ->
        init_queue_bindings(QueueSup) end, QueueSups
    ),
    init_agent_bindings(),
    ok.

init_queue_bindings(QueueSup) ->
    Manager = acdc_queue_sup:manager(QueueSup),
    gen_listener:add_responder(
        Manager,
        {amimulator_acdc, handle_event},
        [
            {<<"member">>, <<"call">>},
            {<<"member">>, <<"call_cancel">>}
        ]
    ).

init_agent_bindings() ->
    gen_listener:add_responder(
        acdc_agent_manager,
        {amimulator_acdc, handle_event},
        [
            {<<"agent">>, <<"login">>},
            {<<"agent">>, <<"logout">>},
            {<<"agent">>, <<"queue_login">>},
            {<<"agent">>, <<"queue_logout">>}
            %{<<"agent">>, <<"pause">>},
            %{<<"agent">>, <<"resume">>}
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

    Call = amimulator_util:whapps_call(EventData),
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
            EndpointName = amimulator_util:endpoint_name(whapps_call:account_db(Call), Endpoint),
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
    amimulator_amqp:publish_amqp_event({publish, Payload});
handle_specific_event(<<"call_cancel">>, EventJObj) ->
    lager:debug("AMI: member call cancel event ~p", [EventJObj]),
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
    amimulator_amqp:publish_amqp_event({publish, Payload});
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
    
    gen_listener:cast(amimulator_amqp, {out, Payload});
handle_specific_event(<<"login">>, EventJObj) ->
    %lager:debug("Agent logged in to queues ~p", [EventJObj]),
    AgentId = wh_json:get_value(<<"Agent-ID">>, EventJObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    Exten = case amimulator_util:find_id_number(AgentId, AccountDb) of
        {error, not_found} ->
            wh_json:get_value(<<"username">>, AgentDoc);
        {ok, Number} ->
            Number
    end,
    AgentName = <<(wh_json:get_value(<<"first_name">>, AgentDoc))/binary, " ", (wh_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
    WholePayload = lists:foldl(fun(QueueId, Payload) ->
        case amimulator_util:find_id_number(QueueId, AccountDb) of
            {error, _E} -> Payload;
            {ok, Number2} ->
                Payload ++ [[
                    {<<"Event">>, <<"QueueMemberAdded">>},
                    {<<"Privilege">>, <<"agent,all">>},
                    {<<"Queue">>, Number2},
                    {<<"Location">>, <<"Local/", Exten/binary, "@from-queue/n">>},
                    {<<"MemberName">>, AgentName},
                    {<<"Membership">>, <<"dynamic">>},
                    {<<"Penalty">>, 0},
                    {<<"CallsTaken">>, 0},
                    {<<"LastCall">>, 0},
                    {<<"Status">>, 1},
                    {<<"Paused">>, 0}
                ]]
        end end, [], wh_json:get_value(<<"queues">>, AgentDoc, [])
    ),
    amimulator_amqp:publish_amqp_event({publish, WholePayload});
handle_specific_event(<<"logout">>, EventJObj) ->
    %lager:debug("Agent logged out from queues ~p", [EventJObj]),
    AgentId = wh_json:get_value(<<"Agent-ID">>, EventJObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    Exten = case amimulator_util:find_id_number(AgentId, AccountDb) of
        {error, not_found} ->
            wh_json:get_value(<<"username">>, AgentDoc);
        {ok, Number} ->
            Number
    end,
    AgentName = <<(wh_json:get_value(<<"first_name">>, AgentDoc))/binary, " ", (wh_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
    WholePayload = lists:foldl(fun(QueueId, Payload) ->
        case amimulator_util:find_id_number(QueueId, AccountDb) of
            {error, _E} -> Payload;
            {ok, Number2} -> Payload ++ [[
                {<<"Event">>, <<"QueueMemberRemoved">>},
                {<<"Privilege">>, <<"agent,all">>},
                {<<"Queue">>, Number2},
                {<<"Location">>, <<"Local/", Exten/binary, "@from-queue/n">>},
                {<<"MemberName">>, AgentName}
            ]]
        end end, [], wh_json:get_value(<<"queues">>, AgentDoc, [])
    ),
    amimulator_amqp:publish_amqp_event({publish, WholePayload});
handle_specific_event(<<"queue_login">>, EventJObj) ->
    lager:debug("Agent logged into a queue ~p", [EventJObj]);
handle_specific_event(<<"queue_logout">>, EventJObj) ->
    lager:debug("Agent logged out from a queue ~p", [EventJObj]);
handle_specific_event(_, _EventJObj) ->
    lager:debug("AMI: unhandled acdc event").