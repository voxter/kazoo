-module(amimulator_acdc).

-export([init/1, bindings/1, responders/1, handle_event/1]).

-include("../amimulator.hrl").

%%
%% Public functions
%%

init(_AccountId) ->
    ok.

bindings(Props) ->
    AccountId = props:get_value("AccountId", Props),
    [
        {acdc_agent, [
            {account_id, AccountId}
        ]},
        {acdc_queue, [
            {restrict_to, [member_call]},
            {account_id, AccountId}
        ]},
        {acdc_stats, [
            {restrict_to, [call_stat, status_stat]},
            {account_id, AccountId}
        ]}
    ].

responders(_Props) ->
    [
        {<<"member">>, <<"call">>},
        {<<"member">>, <<"call_cancel">>},
        {<<"acdc_call_stat">>, <<"handled">>},
        {<<"acdc_status_stat">>, <<"connecting">>},
        {<<"agent">>, <<"login">>},
        {<<"agent">>, <<"logout">>},
        {<<"agent">>, <<"pause">>},
        {<<"agent">>, <<"resume">>},
        {<<"agent">>, <<"login_queue">>},
        {<<"agent">>, <<"logout_queue">>}
    ].

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
    CallerIdNum = props:get_value(<<"aleg_exten">>, Call),
    CallerId = props:get_value(<<"aleg_cid">>, Call),

    %lager:debug("acdc eventdata ~p", [wh_json:delete_key(<<"Key-Value-Store">>, EventData)]),

    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = wh_json:get_value(<<"Queue-ID">>, EventJObj),

    Position = ami_sm:queue_call(QueueId, CallId, Call),

    case amimulator_util:find_id_number(
        wh_json:get_value(<<"Queue-ID">>, EventJObj),
        wh_util:format_account_id(AccountId, encoded)
    ) of
    	{error, E} ->
    		lager:debug("Could not find queue extension ~p", [E]);
    	{ok, Number} ->
		    Payload = [
		        {<<"Event">>, <<"Join">>},
		        {<<"Privilege">>, <<"call,all">>},
		        {<<"Channel">>, EndpointName},
		        {<<"CallerIDNum">>, CallerIdNum},
		        {<<"CallerIDName">>, CallerId},
		        {<<"ConnectedLineNum">>, <<"unknown">>},
		        {<<"ConnectedLineName">>, <<"unknown">>},
		        {<<"Queue">>, Number},
		        {<<"Position">>, Position+1},
		        {<<"Count">>, Position+1},
		        {<<"Uniqueid">>, CallId}
		    ],
		    ami_ev:publish_amqp_event({publish, Payload})
	end;
handle_specific_event(<<"call_cancel">>, EventJObj) ->
    QueueId = wh_json:get_value(<<"Queue-ID">>, EventJObj),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),

    Call = ami_sm:fetch_queue_call_data(QueueId, CallId),
    Position = ami_sm:queue_pos(QueueId, CallId),
    ami_sm:queue_leave(QueueId, CallId),
    
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    case amimulator_util:find_id_number(
        wh_json:get_value(<<"Queue-ID">>, EventJObj),
        wh_util:format_account_id(AccountId, encoded)
    ) of
    	{error, E} ->
    		lager:debug("Could not find queue extension ~p", [E]);
    	{ok, Number} ->
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
		    ami_ev:publish_amqp_event({publish, Payload})
	end;
handle_specific_event(<<"connecting">>, EventJObj) ->
	AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
	AccountDb = wh_util:format_account_id(AccountId, encoded),
	CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
	AgentId = wh_json:get_value(<<"Agent-ID">>, EventJObj),

	{ok, Owner} = couch_mgr:open_doc(AccountDb, AgentId),
	Call = ami_sm:call(CallId),
	Payload = [
		{<<"Event">>, <<"AgentCalled">>},
		{<<"AgentCalled">>, <<"Local/", (wh_json:get_value(<<"username">>, Owner))/binary, "@from-queue/n">>},
		{<<"ChannelCalling">>, props:get_value(<<"aleg_ami_channel">>, Call)},
		{<<"CallerID">>, props:get_value(<<"aleg_cid">>, Call)},
		{<<"Context">>, <<"default">>},
		{<<"Extension">>, props:get_value(<<"aleg_exten">>, Call)},
		{<<"Priority">>, 1}
	],
	ami_ev:publish_amqp_event({publish, Payload});
handle_specific_event(<<"handled">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),

    QueueId = wh_json:get_value(<<"Queue-ID">>, EventJObj),
    Position = ami_sm:queue_pos(QueueId, CallId),

    ami_sm:queue_leave(QueueId, CallId),
    
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    case amimulator_util:find_id_number(
        wh_json:get_value(<<"Queue-ID">>, EventJObj),
        wh_util:format_account_id(AccountId, encoded)
    ) of
    	{error, E} ->
    		lager:debug("Could not find queue extension ~p", [E]);
    	{ok, Number} ->
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
		    ami_ev:publish_amqp_event({publish, Payload})
	end;
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
    
    gen_listener:cast(ami_ev, {out, Payload});
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
    ami_ev:publish_amqp_event({publish, WholePayload});
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
    ami_ev:publish_amqp_event({publish, WholePayload});
handle_specific_event(<<"login_queue">>, EventJObj) ->
    AgentId = wh_json:get_value(<<"Agent-ID">>, EventJObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = wh_json:get_value(<<"Queue-ID">>, EventJObj),

    AccountDb = wh_util:format_account_id(AccountId, encoded),
    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    Exten = case amimulator_util:find_id_number(AgentId, AccountDb) of
        {error, not_found} ->
            wh_json:get_value(<<"username">>, AgentDoc);
        {ok, Number} ->
            Number
    end,
    AgentName = <<(wh_json:get_value(<<"first_name">>, AgentDoc))/binary, " ", (wh_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
    case amimulator_util:find_id_number(QueueId, AccountDb) of
        {error, _E} -> ok;
        {ok, Number2} ->
            Payload = [
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
            ],
            ami_ev:publish_amqp_event({publish, Payload})
    end;
handle_specific_event(<<"logout_queue">>, EventJObj) ->
    AgentId = wh_json:get_value(<<"Agent-ID">>, EventJObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = wh_json:get_value(<<"Queue-ID">>, EventJObj),

    AccountDb = wh_util:format_account_id(AccountId, encoded),
    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    Exten = case amimulator_util:find_id_number(AgentId, AccountDb) of
        {error, not_found} ->
            wh_json:get_value(<<"username">>, AgentDoc);
        {ok, Number} ->
            Number
    end,
    AgentName = <<(wh_json:get_value(<<"first_name">>, AgentDoc))/binary, " ", (wh_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
    case amimulator_util:find_id_number(QueueId, AccountDb) of
        {error, _E} -> ok;
        {ok, Number2} ->
            Payload = [
                {<<"Event">>, <<"QueueMemberRemoved">>},
                {<<"Privilege">>, <<"agent,all">>},
                {<<"Queue">>, Number2},
                {<<"Location">>, <<"Local/", Exten/binary, "@from-queue/n">>},
                {<<"MemberName">>, AgentName}
            ],
            ami_ev:publish_amqp_event({publish, Payload})
    end;
handle_specific_event(<<"pause">>, EventJObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    AgentId = wh_json:get_value(<<"Agent-ID">>, EventJObj),

    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    Interface = <<"Local/", (wh_json:get_value(<<"username">>, AgentDoc))/binary,
        "@from-queue/n">>,
    AgentName = <<(wh_json:get_value(<<"first_name">>, AgentDoc))/binary, " ",
        (wh_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
    Payload = lists:foldl(fun(QueueId, Acc) ->
        case amimulator_util:find_id_number(QueueId, AccountDb) of
            {ok, QueueNumber} ->
                [[
                    {<<"Event">>, <<"QueueMemberPaused">>},
                    {<<"Privilege">>, <<"agent,all">>},
                    {<<"Queue">>, QueueNumber},
                    {<<"Location">>, Interface},
                    {<<"MemberName">>, AgentName},
                    {<<"Paused">>, 1}
                ] | Acc];
            _ ->
                Acc
        end end, [], wh_json:get_value(<<"queues">>, AgentDoc, [])),
    ami_ev:publish_amqp_event({publish, Payload});
handle_specific_event(<<"resume">>, EventJObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    AgentId = wh_json:get_value(<<"Agent-ID">>, EventJObj),

    {ok, AgentDoc} = couch_mgr:open_doc(AccountDb, AgentId),
    Interface = <<"Local/", (wh_json:get_value(<<"username">>, AgentDoc))/binary,
        "@from-queue/n">>,
    AgentName = <<(wh_json:get_value(<<"first_name">>, AgentDoc))/binary, " ",
        (wh_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
    Payload = lists:foldl(fun(QueueId, Acc) ->
        case amimulator_util:find_id_number(QueueId, AccountDb) of
            {ok, QueueNumber} ->
                [[
                    {<<"Event">>, <<"QueueMemberPaused">>},
                    {<<"Privilege">>, <<"agent,all">>},
                    {<<"Queue">>, QueueNumber},
                    {<<"Location">>, Interface},
                    {<<"MemberName">>, AgentName},
                    {<<"Paused">>, 0}
                ] | Acc];
            _ ->
                Acc
        end end, [], wh_json:get_value(<<"queues">>, AgentDoc, [])),
    ami_ev:publish_amqp_event({publish, Payload});
handle_specific_event(_, _EventJObj) ->
    lager:debug("AMI: unhandled acdc event").
