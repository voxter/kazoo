-module(amimulator_call_fsm).

-behaviour(gen_fsm).

-include("../amimulator.hrl").

-export([start_link/2, start_link/3]).
-export([new_call/2
         ,answer/2
         ,bridge/3
         ,destroy/3
         ,join_queue/3
         ,monitoring/2
        ]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([pre_create/2
         ,created/2
         ,answered/2
         ,queued/2
        ]).

-define(STATE_UP, 6).

-record(state, {supervisor :: api_pid()
                ,monitored_channel :: api_binary()
                ,calls = [] :: list()
                ,answered :: amimulator_call:call() | 'undefined'
               }).

%%
%% Public functions
%%

start_link(Super, Call) ->
    gen_fsm:start_link(?MODULE, [Super, Call], []).

start_link(Super, Call, 'initial') ->
    gen_fsm:start_link(?MODULE, [Super, Call, 'initial'], []).

-spec new_call(pid(), amimulator_call:call()) -> 'ok'.
new_call(FSM, Call) ->
    gen_fsm:send_event(FSM, {'new_call', Call}).

-spec answer(pid(), amimulator_call:call()) -> 'ok'.
answer(FSM, Call) ->
    gen_fsm:send_event(FSM, {'answer', Call}).

-spec bridge(pid(), amimulator_call:call(), amimulator_call:call()) -> 'ok'.
bridge(FSM, Call, OtherCall) ->
    gen_fsm:send_event(FSM, {'bridge', Call, OtherCall}).

-spec destroy(pid(), api_binary(), amimulator_call:call()) -> 'ok'.
destroy(FSM, Reason, Call) ->
    gen_fsm:send_event(FSM, {'destroy', Reason, Call}).

-spec join_queue(pid(), api_binary(), amimulator_call:call()) -> 'ok'.
join_queue(FSM, QueueId, Call) ->
    gen_fsm:send_event(FSM, {'join_queue', QueueId, Call}).

-spec monitoring(pid(), amimulator_call:call()) -> boolean().
monitoring(FSM, Call) ->
    gen_fsm:sync_send_all_state_event(FSM, {'monitoring', amimulator_call:channel(Call)}).

%%
%% gen_fsm callbacks
%%

init([Super, Call]) ->
    {'ok', 'pre_create', #state{supervisor=Super
                          ,monitored_channel = amimulator_call:channel(Call)
                         }};
init([Super, Call, 'initial']) ->
    lager:debug("catching up to correct start for call ~p", [amimulator_call:call_id(Call)]),
    initialize(Super, Call).

handle_event({'initialize', Call}, _, #state{calls=Calls
                                             ,answered='undefined'
                                            }=State) ->
    lager:debug("received initialize with call ~p", [Call]),
    case amimulator_call:answered(Call) of
        'true' -> {'next_state', 'answered', State#state{calls = [Call], answered=Call}};
        'false' -> {'next_state', 'created', State#state{calls = [Call | Calls]}}
    end;
handle_event({'initialize', _}, StateName, State) ->
    {'next_state', StateName, State};
handle_event(Event, StateName, State) ->
    lager:debug("unhandled event in state ~s: ~p", [StateName, Event]),
    {'next_state', StateName, State}.

handle_sync_event({'monitoring', Channel}, _From, StateName, #state{monitored_channel=Channel}=State) ->
    {'reply', 'true', StateName, State};
handle_sync_event({'monitoring', _}, _From, StateName, State) ->
    {'reply', 'false', StateName, State};
handle_sync_event(Event, _From, StateName, State) ->
    lager:debug("unhandled sync event in state ~s: ~p", [StateName, Event]),
    {'reply', 'ok', StateName, State}.

handle_info({'$gen_cast', _}, StateName, State) ->
    {'next_state', StateName, State};
handle_info(Info, StateName, State) ->
    lager:debug("unhandled info in state ~s: ~p", [StateName, Info]),
    {'next_state', StateName, State}.

terminate(Reason, StateName, _) ->
    lager:debug("terminating in state ~s (~s)", [StateName, Reason]).

code_change(_, StateName, State, _) ->
    {'ok', StateName, State}.

%%
%% gen_fsm states
%%

pre_create({'new_call', Call}, #state{calls=Calls
                                      ,answered=Answered
                                     }=State) ->
    new_channel_event(amimulator_call:direction(Call), Call),
    maybe_dial_event(Call),
    extension_status(Call),
    new_state(amimulator_call:direction(Call), Call),
    maybe_change_agent_status(Call),

    maybe_already_answered(Answered),

    %% The second leg of a call might update CID of first
    maybe_update_other_call_dest(amimulator_call:call_id(Call), amimulator_call:other_leg_call_id(Call), Call),

    {'next_state', 'created', State#state{calls = [Call | Calls]}};

pre_create({'answer', Call}, State) ->
    CallId = amimulator_call:call_id(Call),
    lager:debug("early answer for call with id ~p", [CallId]),
    ami_sm:flag_early_answer(CallId),
    {'next_state', 'pre_create', State#state{answered=Call}}.

created({'new_call', Call}, #state{calls=Calls}=State) ->
    {'next_state', 'created', State#state{calls = [Call | Calls]}};

created({'answer', Call}, State) ->
    Call2 = amimulator_call:set_answered('true', Call),
    CallId = amimulator_call:call_id(Call2),

    ami_sm:answer(amimulator_call:channel(Call2), CallId),
    ami_sm:update_call(Call2),
    busy_state(Call2, CallId),
    maybe_change_agent_status(Call2),

    {'next_state', 'answered', State#state{answered=Call2}};

created({'destroy', _, Call}, #state{monitored_channel=Channel
                                  ,calls=Calls
                                 }=State) when length(Calls) > 1 ->
    CallId = amimulator_call:call_id(Call),
    Calls2 = lists:keydelete(CallId, 2, Calls),
    ami_sm:delete_call(CallId),
    lager:debug("channel ~p has call ids remaining: ~p", [Channel, [amimulator_call:call_id(Call2) || Call2 <- Calls2]]),
    {'next_state', 'created', State#state{calls=Calls2}};

created({'destroy', Reason, Call}, State) ->
    destroy_channel(Reason, Call),
    {'stop', 'destroyed', State};

created({'join_queue', QueueId, Call}, State) ->
    QueueCall = fork_queue_call(QueueId, Call),
    new_channel_event(amimulator_call:direction(QueueCall), QueueCall),
    ami_sm:update_call(amimulator_call:set_other_leg_call_id(amimulator_call:call_id(QueueCall), Call)),
    {'next_state', 'created', State#state{queue_call=QueueCall}}.

answered({'new_call', Call}, #state{calls=Calls}=State) ->
    {'next_state', 'answered', State#state{calls = [Call | Calls]}};

answered({'answer', Call}, #state{monitored_channel=Channel}=State) ->
    lager:debug("received a second answer for channel ~p on call with id ~p", [Channel, amimulator_call:call_id(Call)]),
    {'next_state', 'answered', State};

answered({'bridge', Call, OtherCall}, State) ->
    Call2 = amimulator_call:update_from_other(OtherCall, Call),
    OtherCall2 = amimulator_call:update_from_other(Call, OtherCall),
    ami_sm:update_call(Call2),
    ami_sm:update_call(OtherCall2),

    maybe_bridge_and_dial(Call2, OtherCall2),

    {'next_state', 'answered', State#state{answered=Call2}};

answered({'destroy', Reason, Call}, #state{answered=Answered}=State) ->
    CallId = amimulator_call:call_id(Call),
    AnsweredCallId = amimulator_call:call_id(Answered),
    if CallId =:= AnsweredCallId ->
        destroy_channel(Reason, Call),
        {'stop', 'destroyed', State};
    'true' ->
        ami_sm:delete_call(CallId),
        {'next_state', 'answered', State}
    end.

%%
%% private functions
%%

% -spec initialize(
initialize(Super, Call) ->
    State = #state{supervisor=Super
                   ,monitored_channel = amimulator_call:channel(Call)
                   ,calls = [Call]
                  },
    maybe_already_answered(Call, State).

% -spec maybe_already_answered(amimulator_call:call() | 'undefined') -> 'ok'.
maybe_already_answered() ->
    case amimulator_call:answered(Call) of
        'true' -> 




    'ok';
maybe_already_answered(Call) ->
    answer(self(), Call).

-spec maybe_update_other_call_dest(api_binary(), api_binary(), amimulator_call:call()) -> 'ok'.
maybe_update_other_call_dest(_, 'undefined', _) ->
    'ok';
maybe_update_other_call_dest(CallId, CallId, _) ->
    'ok';
maybe_update_other_call_dest(_, OtherCallId, Call) ->
    OtherCall = ami_sm:call(OtherCallId),
    ami_sm:update_call(amimulator_call:update_from_other(Call, OtherCall)).

-spec fork_queue_call(api_binary(), amimulator_call:call()) -> amimulator_call:call().
fork_queue_call(QueueId, Call) ->
    Updaters = [fun(Call2) ->
                    QueueCallId = <<(amimulator_call:call_id(Call2))/binary, "-queue">>,
                    amimulator_call:set_call_id(QueueCallId, Call2)
                end
                ,fun(Call2) -> amimulator_call:set_other_leg_call_id(amimulator_call:call_id(Call), Call2) end
                ,fun(Call2) -> amimulator_call:set_acdc_queue_id(QueueId, Call2) end
                ,fun(Call2) -> amimulator_call:set_channel(Call2) end
               ],
    QueueCall = lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters),
    ami_sm:new_call(QueueCall),
    QueueCall.

new_channel_event(<<"inbound">>, Call) ->
    CallId = amimulator_call:call_id(Call),
    SourceExten = amimulator_call:id_number(Call),
    DestExten = amimulator_call:other_id_number(Call),
    EndpointName = amimulator_call:channel(Call),

    SourceCID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
        <<"Voicemail">>;
    'true' ->
        amimulator_call:other_id_name(Call)
    end,

    Payload = new_channel_payload(EndpointName, SourceCID, SourceCID, DestExten, CallId),
    amimulator_event_listener:publish_amqp_event({'publish', Payload});
new_channel_event(<<"outbound">>, Call) ->
    CallId = amimulator_call:call_id(Call),
    SourceCID = amimulator_call:other_id_name(Call),
    EndpointName = amimulator_call:channel(Call),

    % case EndpointName of
    %     undefined ->
    %       lager:debug("Error: Endpoint name undefined"),
    %         lager:debug("Call ~p", [Call]);
    %     _ ->
    %         ok
    % end,

    Payload = new_channel_payload(EndpointName, SourceCID, SourceCID, CallId),
    amimulator_event_listener:publish_amqp_event({'publish', Payload}).

new_channel_payload(Channel, CallerIDNum, CallerIDName, Uniqueid) ->
    new_channel_payload(Channel, CallerIDNum, CallerIDName, <<"">>, Uniqueid).
new_channel_payload(Channel, CallerIDNum, CallerIDName, Exten, Uniqueid) ->
    [
        {<<"Event">>, <<"Newchannel">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, Channel},
        {<<"ChannelState">>, 0},
        {<<"ChannelStateDesc">>, <<"Down">>},
        {<<"CallerIDNum">>, CallerIDNum},
        {<<"CallerIDName">>, CallerIDName},
        {<<"AccountCode">>, <<"">>}, %% Always blank
        {<<"Exten">>, Exten},
        {<<"Context">>, <<"from-internal">>},
        {<<"Uniqueid">>, Uniqueid}
    ].

extension_status(Call) ->
    SourceExten = amimulator_call:id_number(Call),
    Status = case amimulator_call:direction(Call) of
        <<"inbound">> -> 1;
        <<"outbound">> -> 8
    end,

    Payload = [
        {<<"Event">>, <<"ExtensionStatus">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Extension">>, SourceExten},
        {<<"Context">>, <<"ext-local">>},
        {<<"Hint">>, <<"SIP/", SourceExten/binary, ",CustomPresence:", SourceExten/binary>>},
        {<<"Status">>, Status}
    ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}).

maybe_dial_event(Call) ->
    dial_event(amimulator_call:other_leg_call_id(Call), Call).

dial_event('undefined', Call) ->
    CallId = amimulator_call:call_id(Call),
    EndpointName = amimulator_call:channel(Call),
    SourceExten = amimulator_call:id_number(Call),
    DestExten = amimulator_call:other_id_number(Call),
    SourceCID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
        <<"Voicemail">>;
    'true' ->
        amimulator_call:id_name(Call)
    end,
    DestCID = amimulator_call:other_id_name(Call),

    case amimulator_call:direction(Call) of
        <<"inbound">> ->
            Payload = dial(EndpointName, 'undefined', SourceCID, SourceCID, DestCID, DestCID, CallId, 'undefined', DestCID),
            amimulator_event_listener:publish_amqp_event({'publish', Payload});
        _ -> 'ok'
    end;
dial_event(OtherCallId, Call) ->
    CallId = amimulator_call:call_id(Call),
    SourceExten = amimulator_call:id_number(Call),
    DestExten = amimulator_call:other_id_number(Call),
    EndpointName = amimulator_call:channel(Call),
    OtherCall = ami_sm:call(OtherCallId),
    OtherCID = amimulator_call:other_id_name(Call),
    OtherEndpointName = amimulator_call:other_channel(Call),

    CID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
        <<"Voicemail">>;
    'true' ->
        amimulator_call:id_name(Call)
    end,

    % {OtherCID, OtherEndpointName} = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
    %     undefined ->
    %         % case OtherCallId of
    %         %     CallId ->
    %         %         {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
    %         %     undefined ->
    %         %         {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
    %         %     _ ->
    %         %         {props:get_value(<<"aleg_cid">>, OtherCall), props:get_value(<<"aleg_ami_channel">>, OtherCall)}
    %         % end;
    %         {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
    %     QueueId ->
    %         case amimulator_util:find_id_number(
    %             QueueId,
    %             whapps_call:account_db(WhappsCall)
    %         ) of
       %          {error, E} ->
       %            lager:debug("Could not find queue extension ~p", [E]),
       %            case OtherCallId of
          %               CallId ->
          %                   {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
          %               undefined ->
          %                   {props:get_value(<<"bleg_cid">>, Call), props:get_value(<<"bleg_ami_channel">>, Call)};
          %               _ ->
          %                   {props:get_value(<<"aleg_cid">>, OtherCall), props:get_value(<<"aleg_ami_channel">>, OtherCall)}
          %           end;
       %          {ok, Number} ->
       %            {<<"Queue ", Number/binary, " Call">>, <<>>}
    %         end
    % end,

    %% We need to publish only if the exten matches originally dialed one
    OtherDialed = amimulator_call:other_id_number(OtherCall),
    case OtherDialed of
        'undefined' ->
            'ok';
        _ ->
            case EndpointName of
                OtherEndpointName ->
                    'ok';
                _ ->
                    Payload = dial(OtherEndpointName, EndpointName, OtherCID, OtherCID, CID, CID, OtherCallId, CallId, CID),
                    amimulator_event_listener:publish_amqp_event({'publish', Payload})
            end
    end.

maybe_bridge_and_dial(Call, OtherCall) ->
    CallId = amimulator_call:call_id(Call),
    OtherCallId = amimulator_call:call_id(OtherCall),
    Channel1 = amimulator_call:channel(Call),
    Channel2 = amimulator_call:other_channel(Call),

    %% Don't publish bridge and dial if the channels are bridging to themselves, just causes problems
    if Channel1 =:= Channel2 ->
        'ok';
    'true' ->
        SourceCID = amimulator_call:id_name(Call),
        OtherCID = amimulator_call:other_id_name(Call),

        case amimulator_call:direction(Call) of
            <<"inbound">> ->
                bridge_and_dial(Channel1, Channel2, CallId, OtherCallId, SourceCID, OtherCID);
            <<"outbound">> ->
                bridge_and_dial(Channel2, Channel1, OtherCallId, CallId, OtherCID, SourceCID)
        end
    end.

dial(Channel, Destination, CallerIDNum, CallerIDName, ConnectedLineNum, ConnectedLineName, Uniqueid, DestUniqueid, Dialstring) ->
    [
        {<<"Event">>, <<"Dial">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"SubEvent">>, <<"Begin">>},
        {<<"Channel">>, Channel},
        {<<"Destination">>, Destination},
        {<<"CallerIDNum">>, CallerIDNum},
        {<<"CallerIDName">>, CallerIDName},
        {<<"ConnectedLineNum">>, ConnectedLineNum},
        {<<"ConnectedLineName">>, ConnectedLineName},
        {<<"UniqueID">>, Uniqueid},
        {<<"DestUniqueid">>, DestUniqueid},
        {<<"Dialstring">>, Dialstring}
    ].

bridge_and_dial(SourceChannel, DestChannel, SourceCallId, DestCallId, SourceCID, DestCID) ->
    Payload = [[
        {<<"Event">>, <<"Link">>},
        {<<"Channel1">>, SourceChannel},
        {<<"Channel2">>, DestChannel},
        {<<"Uniqueid1">>, SourceCallId},
        {<<"Uniqueid2">>, DestCallId}
    ], [
        {<<"Event">>, <<"Dial">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"SubEvent">>, <<"Begin">>},
        {<<"Channel">>, SourceChannel},
        {<<"Destination">>, DestChannel},
        {<<"CallerIDNum">>, SourceCID},
        {<<"CallerIDName">>, SourceCID},
        {<<"ConnectedLineNum">>, DestCID},
        {<<"ConnectedLineName">>, DestCID},
        {<<"UniqueID">>, SourceCallId},
        {<<"DestUniqueid">>, DestCallId},
        {<<"Dialstring">>, DestCID}
    ]],
    amimulator_event_listener:publish_amqp_event({publish, Payload}).

new_state(<<"inbound">>, Call) ->
    CallId = amimulator_call:call_id(Call),
    SourceExten = amimulator_call:id_number(Call),
    DestExten = amimulator_call:other_id_number(Call),
    EndpointName = amimulator_call:channel(Call),

    OtherCID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
        <<"Voicemail">>;
    'true' ->
        amimulator_call:other_id_name(Call)
    end,

    % OtherCID = case DestExten of
    %     SourceExten ->
    %         <<"Voicemail">>;
    %     _ ->
    %         maybe_internal_cid(WhappsCall,
    %             hd(binary:split(wh_json:get_value(<<"To">>, EventJObj), <<"@">>)))
    % end,

    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, 4},
        {<<"ChannelStateDesc">>, <<"Ring">>},
        {<<"CallerIDNum">>, OtherCID},
        {<<"CallerIDName">>, OtherCID},
        {<<"ConnectedLineNum">>, <<"">>},
        {<<"ConnectedLineName">>, <<"">>},
        {<<"Uniqueid">>, CallId}
    ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload});
new_state(<<"outbound">>, Call) ->
    CallId = amimulator_call:call_id(Call),
    OtherCID = amimulator_call:other_id_name(Call),
    EndpointName = amimulator_call:channel(Call),

    % OtherCallId = whapps_call:other_leg_call_id(WhappsCall),
    % OtherCall = ami_sm:call(OtherCallId),

    % OtherCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
    %     undefined ->
    %         % case OtherCallId of
    %         %     CallId ->
    %         %         props:get_value(<<"bleg_cid">>, Call);
    %         %     undefined ->
    %         %         props:get_value(<<"bleg_cid">>, Call);
    %         %     _ ->
    %         %         props:get_value(<<"aleg_cid">>, OtherCall)
    %         % end;
    %         props:get_value(<<"bleg_cid">>, Call);
    %     QueueId ->
    %         case amimulator_util:find_id_number(
    %             QueueId,
    %             whapps_call:account_db(WhappsCall)
    %         ) of
       %          {error, E} ->
       %            lager:debug("Could not find queue extension ~p", [E]),
       %            props:get_value(<<"bleg_cid">>, Call);
       %          {ok, Number} ->
       %            <<"Queue ", Number/binary, " Call">>
    %         end
    % end,

    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, 5},
        {<<"ChannelStateDesc">>, <<"Ringing">>},
        {<<"CallerIDNum">>, OtherCID},
        {<<"CallerIDName">>, OtherCID},
        {<<"ConnectedLineNum">>, OtherCID},
        {<<"ConnectedLineName">>, OtherCID},
        {<<"Uniqueid">>, CallId}
    ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}).

busy_state(Call, CallId) ->
    EndpointName = amimulator_call:channel(Call),
    OtherCID = amimulator_call:other_id_name(Call),

    % OtherCallId = whapps_call:other_leg_call_id(WhappsCall),
    % OtherCall = ami_sm:call(OtherCallId),

    % OtherCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
    %     undefined ->
    %         % case OtherCallId of
    %         %     CallId ->
    %         %         props:get_value(<<"bleg_cid">>, Call);
    %         %     undefined ->
    %         %         props:get_value(<<"bleg_cid">>, Call);
    %         %     _ ->
    %         %         props:get_value(<<"aleg_cid">>, OtherCall)
    %         % end;
    %         props:get_value(<<"bleg_cid">>, Call);
    %     QueueId ->
    %         {ok, Number} = amimulator_util:find_id_number(
    %             QueueId,
    %             whapps_call:account_db(WhappsCall)
    %         ),
    %         <<"Queue ", Number/binary, " Call">>
    % end,

    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, ?STATE_UP},
        {<<"ChannelStateDesc">>, <<"Up">>},
        {<<"CallerIDNum">>, OtherCID},
        {<<"CallerIDName">>, OtherCID},
        % {<<"ConnectedLineNum">>, OtherCID},
        % {<<"ConnectedLineName">>, OtherCID},
        {<<"Uniqueid">>, CallId}
    ],
    amimulator_event_listener:publish_amqp_event({publish, Payload}).

destroy_channel(Reason, Call) ->
    CallId = amimulator_call:call_id(Call),
    SourceCID = amimulator_call:id_name(Call),
    OtherCID = amimulator_call:other_id_name(Call),
    EndpointName = amimulator_call:channel(Call),

    % DestCID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
    %     undefined ->
    %         props:get_value(<<"bleg_cid">>, Call);
    %     QueueId ->
    %         AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
    %         case amimulator_util:find_id_number(
    %             QueueId,
    %             wh_util:format_account_id(AccountId, encoded)
    %         ) of
       %          {error, E} ->
       %            lager:debug("Could not find queue extension ~p", [E]),
       %            props:get_value(<<"bleg_cid">>, Call);
       %          {ok, Number} ->
       %            <<"Queue ", Number/binary, " Call">>
    %         end
    % end,

    {Cause, CauseText} = case Reason of
        <<"NORMAL_CLEARING">> ->
            {<<"16">>, <<"Normal Clearing">>};
        <<"ORIGINATOR_CANCEL">> ->
            {<<"31">>, <<"Normal, unspecified">>};
        <<"CALL_REJECTED">> ->
            {<<"21">>, <<"Call Rejected">>};
        _ ->
            {<<"0">>, <<"Not Defined">>}
    end,

    lager:debug("channel ~p's last call id ~p destroyed", [EndpointName, CallId]),

    Payload = [[
        {<<"Event">>, <<"Hangup">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"Uniqueid">>, CallId},
        {<<"CallerIDNum">>, SourceCID},
        {<<"CallerIDName">>, SourceCID},
        {<<"ConnectedLineNum">>, OtherCID},
        {<<"ConnectedLineName">>, OtherCID},
        {<<"Cause">>, Cause},
        {<<"Cause-txt">>, CauseText}
    ]] ++ maybe_leave_conference(CallId),

    maybe_change_agent_status(Call),

    amimulator_event_listener:publish_amqp_event({publish, Payload}),

    ami_sm:delete_call(CallId).

maybe_leave_conference(CallId) ->
    case ami_sm:conf_cache(CallId) of
        undefined ->
            [];
        Cache ->
            CallerId = props:get_value(<<"CallerIDnum">>, Cache),
            Timestamp = props:get_value(<<"Timestamp">>, Cache),
            {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
            Duration = (MegaSecs * 1000000 + Secs) - Timestamp,
            [[
                {<<"Event">>, <<"MeetmeLeave">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Channel">>, props:get_value(<<"Channel">>, Cache)},
                {<<"Uniqueid">>, props:get_value(<<"Uniqueid">>, Cache)},
                {<<"Meetme">>, props:get_value(<<"Meetme">>, Cache)},
                {<<"Usernum">>, props:get_value(<<"Usernum">>, Cache)},
                {<<"CallerIDNum">>, CallerId},
                {<<"CallerIDName">>, CallerId},
                {<<"ConnectedLineNum">>, <<"<unknown>">>},
                {<<"ConnectedLineName">>, <<"<unknown>">>},
                {<<"Duration">>, Duration}
            ]]
    end.

maybe_change_agent_status(Call) ->
    case amimulator_call:user(Call) of
        'undefined' -> 'ok';
        UserDoc ->
            case wh_json:get_value(<<"queues">>, UserDoc) of
                'undefined' -> 'ok';
                [] -> 'ok';
                Queues ->
                    {'ok', StatusValue} = acdc_agent_util:most_recent_status(amimulator_call:account_id(Call)
                                                                             ,wh_json:get_value(<<"_id">>, UserDoc)),
                    Status = case StatusValue of
                        <<"ready">> -> 1;
                        <<"logged_in">> -> 1;
                        <<"connected">> -> 2;
                        <<"outbound">> -> 2;
                        <<"logged_out">> -> 5;
                        <<"paused">> -> 5;
                        <<"wrapup">> -> 1;
                        <<"connecting">> -> 6;
                        _ ->
                            lager:debug("unspecified status ~p", [StatusValue]),
                            5
                    end,
                    change_agent_status(amimulator_call:account_db(Call), UserDoc, Queues, Status)
            end
    end.

change_agent_status(AccountDb, UserDoc, Queues, Status) ->
    Username = wh_json:get_value(<<"username">>, UserDoc),
    FirstName = wh_json:get_value(<<"first_name">>, UserDoc),
    LastName = wh_json:get_value(<<"last_name">>, UserDoc),

    Payload = lists:foldl(fun(QueueId, Acc) ->
        case couch_mgr:get_results(AccountDb, <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
            {error, _E} ->
                Acc;
            {ok, []} ->
                Acc;
            {ok, Results} ->
                Value = wh_json:get_value(<<"value">>, hd(Results)),
                Number = hd(Value),
                %% TODO add the stats in here
                [[
                    {<<"Event">>, <<"QueueMemberStatus">>},
                    {<<"Queue">>, Number},
                    {<<"Location">>, <<"Local/", Username/binary, "@from-queue/n">>},
                    {<<"MemberName">>, <<FirstName/binary, " ", LastName/binary>>},
                    {<<"Membership">>, <<"dynamic">>},
                    {<<"Penalty">>, 0},
                    %{<<"LastCall">>, LastCall},
                    {<<"Status">>, Status},
                    {<<"Paused">>, 0}
                ] | Acc]
        end end, [], Queues),

    amimulator_event_listener:publish_amqp_event({publish, Payload}).
