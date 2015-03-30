-module(amimulator_acdc).

-export([init/1, handle_event/1]).

-include("../amimulator.hrl").

init(_AccountId) ->
    ok.

handle_event(EventJObj) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).

%%
%% Event type handlers
%%
    
handle_specific_event(<<"call">>, EventJObj) ->
    EventData = wh_json:get_value(<<"Call">>, EventJObj),

    Call = amimulator_util:create_call(EventData),
    WhappsCall = props:get_value(<<"call">>, Call),
    CallId = whapps_call:call_id(WhappsCall),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),
    CallerId = props:get_value(<<"aleg_cid">>, Call),

    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = wh_json:get_value(<<"Queue-ID">>, EventJObj),
    {Position} = amimulator_store:enqueue(<<"acdc-", QueueId/binary>>, CallId),

    {ok, Number} = amimulator_util:find_id_number(
        wh_json:get_value(<<"Queue-ID">>, EventJObj),
        wh_util:format_account_id(AccountId, encoded)
    ),

    Payload = [
        {<<"Event">>, <<"Join">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"CallerIDNum">>, CallerId},
        {<<"CallerIDName">>, CallerId},
        {<<"ConnectedLineNum">>, <<"unknown">>},
        {<<"ConnectedLineName">>, <<"unknown">>},
        {<<"Queue">>, Number},
        {<<"Position">>, Position+1},
        {<<"Count">>, Position+1},
        {<<"Uniqueid">>, CallId}
    ],
    amimulator_amqp:publish_amqp_event({publish, Payload});
handle_specific_event(<<"call_cancel">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = amimulator_store:get(<<"call-", CallId/binary>>),

    QueueId = wh_json:get_value(<<"Queue-ID">>, EventJObj),
    Position = amimulator_store:reverse_lookup(<<"acdc-", QueueId/binary>>, CallId),
    
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    {ok, Number} = amimulator_util:find_id_number(
        wh_json:get_value(<<"Queue-ID">>, EventJObj),
        wh_util:format_account_id(AccountId, encoded)
    ),

    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    Payload = [[
        {<<"Event">>, <<"QueueCallerAbandon">>},
        {<<"Privilege">>, <<"agent,all">>},
        {<<"Queue">>, Number},
        {<<"Uniqueid">>, CallId},
        {<<"Position">>, Position},
        {<<"OriginalPosition">>, 1},
        {<<"HoldTime">>, 14}
    ],[
        {<<"Event">>, <<"Leave">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"Queue">>, Number},
        {<<"Count">>, 0},
        {<<"Position">>, Position},
        {<<"Uniqueid">>, CallId}
    ]],
    amimulator_amqp:publish_amqp_event({publish, Payload});
handle_specific_event(<<"handled">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = amimulator_store:get(<<"call-", CallId/binary>>),

    QueueId = wh_json:get_value(<<"Queue-ID">>, EventJObj),
    Position = amimulator_store:reverse_lookup(<<"acdc-", QueueId/binary>>, CallId),
    
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    {ok, Number} = amimulator_util:find_id_number(
        wh_json:get_value(<<"Queue-ID">>, EventJObj),
        wh_util:format_account_id(AccountId, encoded)
    ),

    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),

    Payload = [
        {<<"Event">>, <<"Leave">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"Queue">>, Number},
        {<<"Count">>, 0},
        {<<"Position">>, Position},
        {<<"Uniqueid">>, CallId}
    ],
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
