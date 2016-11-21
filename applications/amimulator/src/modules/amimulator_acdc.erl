-module(amimulator_acdc).

-export([init/1, bindings/1, responders/1, handle_event/2]).

-include("../amimulator.hrl").

%%
%% Public functions
%%

-spec init(ne_binary()) -> 'ok'.
init(_AccountId) ->
    'ok'.

-spec bindings(kz_proplist()) -> kz_proplist().
bindings(Props) ->
    AccountId = props:get_value("AccountId", Props),
    [
        {'acdc_agent', [
            {'account_id', AccountId}
        ]},
        {'acdc_queue', [
            {'restrict_to', ['member_call', 'member_call_result']},
            {'account_id', AccountId}
        ]},
        {'acdc_stats', [
            {'restrict_to', ['call_stat', 'status_stat']},
            {'account_id', AccountId}
        ]}
    ].

-spec responders(kz_proplist()) -> kz_proplist().
responders(_Props) ->
    [{<<"member">>, <<"call">>}
     ,{<<"member">>, <<"call_cancel">>}
     ,{<<"acdc_call_stat">>, <<"handled">>}
     ,{<<"acdc_status_stat">>, <<"ready">>}
     ,{<<"acdc_status_stat">>, <<"connected">>}
     ,{<<"acdc_status_stat">>, <<"outbound">>}
     ,{<<"acdc_status_stat">>, <<"wrapup">>}
     ,{<<"acdc_status_stat">>, <<"connecting">>}
     ,{<<"agent">>, <<"login">>}
     ,{<<"agent">>, <<"logout">>}
     ,{<<"agent">>, <<"pause">>}
     ,{<<"agent">>, <<"resume">>}
     ,{<<"agent">>, <<"login_queue">>}
     ,{<<"agent">>, <<"logout_queue">>}
    ].

-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_event(EventJObj, _) ->
    {EventType, EventName} = kz_util:get_event_type(EventJObj),

    case EventType of
        <<"acdc_status_stat">> -> handle_status_event(EventName, EventJObj);
        _ -> handle_specific_event(EventName, EventJObj)
    end.

%%
%% Event type handlers
%%
    
handle_specific_event(<<"call">>, EventJObj) ->
    CallId = kz_json:get_value([<<"Call">>, <<"Call-ID">>], EventJObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, EventJObj),
    Call = amimulator_call:set_acdc_queue_id(QueueId, ami_sm:call(CallId)),
    EndpointName = amimulator_call:channel(Call),
    CallerIdNum = amimulator_call:id_number(Call),
    CallerId = amimulator_call:id_name(Call),
    Position = ami_sm:queue_call(QueueId, CallId, Call),

    case amimulator_util:queue_number(kz_util:format_account_id(AccountId, 'encoded'), QueueId) of
        'undefined' -> 'ok';
        Number ->
            Payload = [
		        {<<"Event">>, <<"Join">>},
		        {<<"Privilege">>, <<"call,all">>},
		        {<<"Channel">>, EndpointName},
		        {<<"CallerIDNum">>, CallerIdNum},
		        {<<"CallerIDName">>, CallerId},
		        {<<"ConnectedLineNum">>, <<"unknown">>},
		        {<<"ConnectedLineName">>, <<"unknown">>},
		        {<<"Queue">>, Number},
		        {<<"Position">>, Position},
		        {<<"Count">>, Position},
		        {<<"Uniqueid">>, CallId}
		    ],
		    amimulator_event_listener:publish_amqp_event({'publish', Payload}, AccountId)
	end;
handle_specific_event(<<"call_cancel">>, EventJObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, EventJObj),
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    maybe_cancel_queue_call(AccountId, QueueId, CallId, kz_json:get_value(<<"Reason">>, EventJObj));
handle_specific_event(<<"handled">>, EventJObj) ->
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, EventJObj),
    Call = ami_sm:call(CallId),
    Position = ami_sm:queue_pos(QueueId, CallId),

    ami_sm:queue_leave(QueueId, CallId),
    
    case amimulator_util:queue_number(kz_util:format_account_id(AccountId, 'encoded'), QueueId) of
        'undefined' -> 'ok';
        Number ->
		    EndpointName = amimulator_call:channel(Call),

		    Payload = [
		        {<<"Event">>, <<"Leave">>},
		        {<<"Privilege">>, <<"call,all">>},
		        {<<"Channel">>, EndpointName},
		        {<<"Queue">>, Number},
		        {<<"Count">>, 0},
		        {<<"Position">>, Position},
		        {<<"Uniqueid">>, CallId}
		    ],
		    amimulator_event_listener:publish_amqp_event({publish, Payload}, AccountId)
	end;
handle_specific_event(<<"login">>, EventJObj) ->
    %lager:debug("Agent logged in to queues ~p", [EventJObj]),
    AgentId = kz_json:get_value(<<"Agent-ID">>, EventJObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    {ok, AgentDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    Exten = kz_json:get_value(<<"username">>, AgentDoc),
    AgentName = <<(kz_json:get_value(<<"first_name">>, AgentDoc))/binary, " ", (kz_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
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
        end end, [], kz_json:get_value(<<"queues">>, AgentDoc, [])
    ),
    amimulator_event_listener:publish_amqp_event({publish, WholePayload}, AccountId);
handle_specific_event(<<"logout">>, EventJObj) ->
    %lager:debug("Agent logged out from queues ~p", [EventJObj]),
    AgentId = kz_json:get_value(<<"Agent-ID">>, EventJObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    {ok, AgentDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    Exten = case amimulator_util:find_id_number(AgentId, AccountDb) of
        {error, not_found} ->
            kz_json:get_value(<<"username">>, AgentDoc);
        {ok, Number} ->
            Number
    end,
    AgentName = <<(kz_json:get_value(<<"first_name">>, AgentDoc))/binary, " ", (kz_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
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
        end end, [], kz_json:get_value(<<"queues">>, AgentDoc, [])
    ),
    amimulator_event_listener:publish_amqp_event({publish, WholePayload}, AccountId);
handle_specific_event(<<"login_queue">>, EventJObj) ->
    AgentId = kz_json:get_value(<<"Agent-ID">>, EventJObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, EventJObj),

    AccountDb = kz_util:format_account_id(AccountId, encoded),
    {ok, AgentDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    Exten = case amimulator_util:find_id_number(AgentId, AccountDb) of
        {error, not_found} ->
            kz_json:get_value(<<"username">>, AgentDoc);
        {ok, Number} ->
            Number
    end,
    AgentName = <<(kz_json:get_value(<<"first_name">>, AgentDoc))/binary, " ", (kz_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
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
            amimulator_event_listener:publish_amqp_event({publish, Payload}, AccountId)
    end;
handle_specific_event(<<"logout_queue">>, EventJObj) ->
    AgentId = kz_json:get_value(<<"Agent-ID">>, EventJObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, EventJObj),

    AccountDb = kz_util:format_account_id(AccountId, encoded),
    {ok, AgentDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    Exten = case amimulator_util:find_id_number(AgentId, AccountDb) of
        {error, not_found} ->
            kz_json:get_value(<<"username">>, AgentDoc);
        {ok, Number} ->
            Number
    end,
    AgentName = <<(kz_json:get_value(<<"first_name">>, AgentDoc))/binary, " ", (kz_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
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
            amimulator_event_listener:publish_amqp_event({publish, Payload}, AccountId)
    end;
handle_specific_event(<<"pause">>, EventJObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    AgentId = kz_json:get_value(<<"Agent-ID">>, EventJObj),

    {ok, AgentDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    Interface = <<"Local/", (kz_json:get_value(<<"username">>, AgentDoc))/binary,
        "@from-queue/n">>,
    AgentName = <<(kz_json:get_value(<<"first_name">>, AgentDoc))/binary, " ",
        (kz_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
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
        end end, [], kz_json:get_value(<<"queues">>, AgentDoc, [])),
    amimulator_event_listener:publish_amqp_event({publish, Payload}, AccountId);
handle_specific_event(<<"resume">>, EventJObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    AgentId = kz_json:get_value(<<"Agent-ID">>, EventJObj),

    {ok, AgentDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    Interface = <<"Local/", (kz_json:get_value(<<"username">>, AgentDoc))/binary,
        "@from-queue/n">>,
    AgentName = <<(kz_json:get_value(<<"first_name">>, AgentDoc))/binary, " ",
        (kz_json:get_value(<<"last_name">>, AgentDoc))/binary>>,
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
        end end, [], kz_json:get_value(<<"queues">>, AgentDoc, [])),
    amimulator_event_listener:publish_amqp_event({publish, Payload}, AccountId);
handle_specific_event(_, _EventJObj) ->
    lager:debug("AMI: unhandled acdc event").

handle_status_event(EventName, EventJObj) ->
    maybe_agent_called(EventName, EventJObj),
    AccountDb = kz_util:format_account_id(kz_json:get_value(<<"Account-ID">>, EventJObj), 'encoded'),
    AgentId = kz_json:get_value(<<"Agent-ID">>, EventJObj),
    case kz_datamgr:open_doc(AccountDb, AgentId) of
        {'error', E} -> lager:debug("error getting agent ~p user doc (~p)", [AgentId, E]);
        {'ok', AgentDoc} -> publish_status_events(translate_status(EventName), AgentDoc, get_queues(AgentDoc))
    end.

maybe_agent_called(<<"connecting">>, EventJObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, EventJObj),

    {'ok', Owner} = kz_datamgr:open_doc(AccountDb, AgentId),
    Call = ami_sm:call(CallId),
    Payload = [
        {<<"Event">>, <<"AgentCalled">>},
        {<<"AgentCalled">>, <<"Local/", (kz_json:get_value(<<"username">>, Owner))/binary, "@from-queue/n">>},
        {<<"ChannelCalling">>, amimulator_call:channel(Call)},
        {<<"CallerID">>, amimulator_call:id_name(Call)},
        {<<"Context">>, <<"default">>},
        {<<"Extension">>, amimulator_call:id_number(Call)},
        {<<"Priority">>, 1}
    ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, AccountId);
maybe_agent_called(_, _) ->
    'ok'.

translate_status(<<"ready">>) -> 1;
translate_status(<<"connected">>) -> 2;
translate_status(<<"outbound">>) -> 2;
translate_status(<<"wrapup">>) -> 1;
translate_status(<<"connecting">>) -> 6.

get_queues(AgentDoc) ->
    case kz_json:get_value(<<"queues">>, AgentDoc) of
        'undefined' ->
            lager:debug("agent ~p doc is missing queues list", [kz_json:get_value(<<"_id">>, AgentDoc)]),
            [];
        Queues -> Queues
    end.

publish_status_events(Status, AgentDoc, Queues) ->
    AccountDb = kz_json:get_value(<<"pvt_account_db">>, AgentDoc),
    Username = kz_json:get_value(<<"username">>, AgentDoc),
    FirstName = kz_json:get_value(<<"first_name">>, AgentDoc),
    LastName = kz_json:get_value(<<"last_name">>, AgentDoc),

    Payload = lists:foldl(fun(QueueId, Acc) ->
        case kz_datamgr:get_results(AccountDb, <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
            {'error', _E} -> Acc;
            {'ok', []} -> Acc;
            {'ok', Results} ->
                Value = kz_json:get_value(<<"value">>, hd(Results)),
                Number = hd(Value),
                %% TODO add the stats in here
                [[
                    {<<"Event">>, <<"QueueMemberStatus">>},
                    {<<"Queue">>, Number},
                    {<<"Location">>, <<"Local/", Username/binary, "@from-queue/n">>},
                    {<<"MemberName">>, <<FirstName/binary, " ", LastName/binary>>},
                    {<<"Membership">>, <<"dynamic">>},
                    {<<"Penalty">>, 0},
                    %{<<"CallsTaken">>, 0},
                    %{<<"LastCall">>, LastCall},
                    {<<"Status">>, Status},
                    {<<"Paused">>, 0}
                ] | Acc]
        end end, [], Queues),

    amimulator_event_listener:publish_amqp_event({'publish', Payload}, kz_json:get_value(<<"pvt_account_id">>, AgentDoc)).

-spec maybe_cancel_queue_call(ne_binary(), ne_binary(), ne_binary(), binary()) -> boolean().
maybe_cancel_queue_call(AccountId, QueueId, CallId, Reason) ->
    maybe_cancel_queue_call(AccountId, QueueId, CallId, Reason, 0).

-spec maybe_cancel_queue_call(ne_binary(), ne_binary(), ne_binary(), binary(), non_neg_integer()) -> boolean().
maybe_cancel_queue_call(_, _, _, _, 3) ->
    'false';
maybe_cancel_queue_call(AccountId, QueueId, CallId, Reason, Attempts) ->
    case ami_sm:fetch_queue_call_data(QueueId, CallId) of
        'undefined' ->
            timer:sleep(200),
            maybe_cancel_queue_call(AccountId, QueueId, CallId, Reason, Attempts+1);
        Call ->
            cancel_queue_call(AccountId, QueueId, CallId, Call, Reason),
            'true'
    end.

-spec cancel_queue_call(ne_binary(), ne_binary(), ne_binary(), kapps_call:call(), binary()) -> 'ok'.
cancel_queue_call(AccountId, QueueId, CallId, Call, Reason) ->
    Position = ami_sm:queue_pos(QueueId, CallId),
    ami_sm:queue_leave(QueueId, CallId),
    
    case amimulator_util:find_id_number(
        QueueId,
        kz_util:format_account_id(AccountId, encoded)
    ) of
        {error, E} ->
            lager:debug("Could not find queue extension ~p", [E]);
        {ok, Number} ->
            EndpointName = amimulator_call:channel(Call),
            amimulator_event_listener:publish_amqp_event({publish, cancel_queue_call_payload(Number, CallId, Position, EndpointName, Reason)}, AccountId)
    end.

-spec cancel_queue_call_payload(binary(), ne_binary(), pos_integer(), binary(), binary()) -> list().
cancel_queue_call_payload(Number, CallId, Position, EndpointName, <<"no agents">>) ->
    [
        {<<"Event">>, <<"Leave">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"Queue">>, Number},
        {<<"Count">>, 0},
        {<<"Position">>, Position},
        {<<"Uniqueid">>, CallId}
    ];
cancel_queue_call_payload(Number, CallId, Position, EndpointName, _) ->
    [[
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
    ]].
