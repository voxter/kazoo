-module(amimulator_acdc).

-export([get_bindings/1, handle_event/1]).

-include("../amimulator.hrl").

get_bindings(AccountId) ->
    queue_bindings(AccountId) ++ [agent_bindings()].

handle_event(EventJObj) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).

%%
%% Private functions
%%

%% Fetch a list of bindings for queues within the specified account
queue_bindings(AccountId) ->
    QueueSups = acdc_queues_sup:find_acct_supervisors(AccountId),
    lists:foldl(fun(QueueSup, Bindings) ->
        [{
            acdc_queue_sup:manager(QueueSup),
            {amimulator_acdc, handle_event},
            [
                {<<"member">>, <<"call">>},
                {<<"member">>, <<"call_cancel">>}
            ]
        } |
        queue_worker_bindings(acdc_queue_workers_sup:workers(acdc_queue_sup:workers_sup(QueueSup)))] ++ Bindings
    end, [], QueueSups).

queue_worker_bindings(QueueWorkerSups) ->
    lists:foldl(fun(QueueWorkerSup, Acc) ->
        [{acdc_queue_worker_sup:listener(QueueWorkerSup), {amimulator_acdc, handle_event},
            {<<"member">>, <<"connect_accepted">>}
        }] ++ Acc end, [], QueueWorkerSups
    ).

agent_bindings() ->
    {acdc_agent_manager, {amimulator_acdc, handle_event}, [
        {<<"agent">>, <<"login">>},
        {<<"agent">>, <<"logout">>},
        {<<"agent">>, <<"queue_login">>},
        {<<"agent">>, <<"queue_logout">>}
        %{<<"agent">>, <<"pause">>},
        %{<<"agent">>, <<"resume">>}
    ]}.

%%
%% Event type handlers
%%
    
handle_specific_event(<<"call">>, EventJObj) ->
    EventData = wh_json:get_value(<<"Call">>, EventJObj),
    %lager:debug("AMI: member call event ~p", [EventData]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventData),
    amimulator_store:store(<<"acdc-", CallId/binary>>, EventData),
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
                {<<"Channel">>, <<"SIP/", EndpointName/binary, "-", (amimulator_util:channel_tail(whapps_call:call_id(Call)))/binary>>},
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
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    %lager:debug("retrieved ~p", [amimulator_store:retrieve(<<"acdc-", CallId/binary>>)]),
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    {ok, Number} = amimulator_util:find_id_number(
        wh_json:get_value(<<"Queue-ID">>, EventJObj),
        wh_util:format_account_id(AccountId, encoded)
    ),

    Payload = [[
        {<<"Event">>, <<"QueueCallerAbandon">>},
        {<<"Privilege">>, <<"agent,all">>},
        {<<"Queue">>, Number},
        {<<"Uniqueid">>, CallId},
        {<<"Position">>, 1},
        {<<"OriginalPosition">>, 1},
        {<<"HoldTime">>, 14}
    ],[
        {<<"Event">>, <<"Leave">>},
        {<<"Privilege">>, <<"call,all">>},
        %% TODO: proper channel here
        {<<"Channel">>, <<"SIP/101-00000000">>},
        {<<"Queue">>, Number},
        {<<"Count">>, 0},
        {<<"Position">>, 1},
        {<<"Uniqueid">>, CallId}
    ]],
    amimulator_amqp:publish_amqp_event({publish, Payload});
handle_specific_event(<<"connect_accepted">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = amimulator_store:retrieve(<<"call-", CallId/binary>>),

    ToUser = hd(binary:split(whapps_call:to(Call), <<"@">>)),
    lager:debug("touser ~p, to ~p", [ToUser, whapps_call:to(Call)]),
    Payload = case cf_endpoint:get(Call) of
        {error, _E} ->
            [
                {<<"Event">>, <<"Leave">>},
                {<<"Privilege">>, <<"call,all">>},
                %{<<"Channel">>, <<"SIP/101-00000000">>},
                {<<"Queue">>, ToUser},
                {<<"Count">>, 0},
                {<<"Position">>, 1},
                {<<"Uniqueid">>, CallId}
            ];
        {ok, Endpoint} ->
            EndpointName = amimulator_util:endpoint_name(whapps_call:account_db(Call), Endpoint),
            [
                {<<"Event">>, <<"Leave">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, <<"SIP/", EndpointName/binary, "-", (amimulator_util:channel_tail(whapps_call:call_id(Call)))/binary>>},
                {<<"Queue">>, ToUser},
                {<<"Count">>, 0},
                {<<"Position">>, 1},
                {<<"Uniqueid">>, CallId}
            ]
    end,
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
