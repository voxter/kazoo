-module(amimulator_call_fsm).

-behaviour(gen_fsm).

-include("../amimulator.hrl").

-export([start_link/2, start_link/3]).
-export([new_call/2
         ,add_initial/2
         ,answer/2
         ,bridge/2
         ,destroy/3
         ,monitoring/2
         ,accepts/2
        ]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([pre_create/2
         ,created/2
         ,answered/2
        ]).

-define(STATE_UP, 6).

-record(state, {supervisor :: api_pid()
                ,monitored_channel :: api_binary()
                ,call_ids = [] :: list()
                ,answered :: api_binary()
                ,conference_call_id :: api_binary()
                ,early_bridge_payload :: kz_json:object() | 'undefined'
               }).
-type state() :: #state{}.

%%
%% Public functions
%%

-spec start_link(pid(), amimulator_call:call()) -> startlink_ret().
start_link(Super, Call) ->
    gen_fsm:start_link(?MODULE, [Super, Call], []).

-spec start_link(pid(), amimulator_call:call(), 'initial') -> startlink_ret().
start_link(Super, Call, 'initial') ->
    gen_fsm:start_link(?MODULE, [Super, Call, 'initial'], []).

-spec new_call(pid(), amimulator_call:call()) -> 'ok'.
new_call(FSM, Call) ->
    gen_fsm:send_event(FSM, {'new_call', Call}).

-spec add_initial(pid(), amimulator_call:call()) -> 'ok'.
add_initial(FSM, Call) ->
    gen_fsm:send_event(FSM, {'add_initial', Call}).

-spec answer(pid(), ne_binary()) -> 'ok'.
answer(FSM, CallId) ->
    gen_fsm:send_event(FSM, {'answer', CallId}).

-spec bridge(pid(), kz_json:object()) -> 'ok'.
bridge(FSM, EventJObj) ->
    gen_fsm:send_event(FSM, {'bridge', EventJObj}).

-spec destroy(pid(), api_binary(), ne_binary()) -> 'ok'.
destroy(FSM, Reason, CallId) ->
    gen_fsm:send_event(FSM, {'destroy', Reason, CallId}).

-spec monitoring(pid(), amimulator_call:call()) -> boolean().
monitoring(FSM, Call) ->
    gen_fsm:sync_send_all_state_event(FSM, {'monitoring', amimulator_call:channel(Call)}).

-spec accepts(pid(), ne_binary()) -> term().
accepts(FSM, CallId) ->
    gen_fsm:sync_send_all_state_event(FSM, {'accepts', CallId}).

%%
%% gen_fsm callbacks
%%

-spec init(list()) -> {'ok', 'pre_create' | 'created' | 'answered', state()}.
init([Super, Call]) ->
    {'ok', 'pre_create', #state{supervisor=Super
                                ,monitored_channel = amimulator_call:channel(Call)
                               }};
init([Super, Call, 'initial']) ->
    lager:debug("catching up to correct start for call ~p", [amimulator_call:call_id(Call)]),
    initialize(Super, Call).

-spec handle_event(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_event(Event, StateName, State) ->
    lager:debug("unhandled event in state ~s: ~p", [StateName, Event]),
    {'next_state', StateName, State}.

-spec handle_sync_event(any(), {pid(),any()}, atom(), state()) -> handle_sync_event_ret(state()).
handle_sync_event({'monitoring', Channel}, _From, StateName, #state{monitored_channel=Channel}=State) ->
    {'reply', 'true', StateName, State};
handle_sync_event({'monitoring', _}, _From, StateName, State) ->
    {'reply', 'false', StateName, State};
handle_sync_event({'accepts', CallId}, _From, StateName, #state{call_ids=CallIds}=State) ->
    {'reply', lists:member(CallId, CallIds), StateName, State};
handle_sync_event(Event, _From, StateName, State) ->
    lager:debug("unhandled sync event in state ~s: ~p", [StateName, Event]),
    {'reply', 'ok', StateName, State}.

-spec handle_info(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_info({'$gen_cast', _}, StateName, State) ->
    {'next_state', StateName, State};
handle_info(Info, StateName, State) ->
    lager:debug("unhandled info in state ~s: ~p", [StateName, Info]),
    {'next_state', StateName, State}.

-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(Reason, StateName, _) ->
    lager:debug("terminating in state ~s (~s)", [StateName, Reason]).

-spec code_change(any(), atom(), state(), any()) -> {'ok', atom(), state()}.
code_change(_, StateName, State, _) ->
    {'ok', StateName, State}.

%%
%% gen_fsm states
%%

-spec pre_create({'new_call' | 'answer'
                 ,amimulator_call:call() | ne_binary()}
                ,state()
                ) -> handle_fsm_ret(state()).
pre_create({'new_call', Call}, #state{call_ids=CallIds}=State) ->
    CallId = amimulator_call:call_id(Call),

    new_channel_event(amimulator_call:direction(Call), Call),
    maybe_newexten(amimulator_call:direction(Call), Call),
    maybe_dial_event(Call),
    extension_status(Call),
    new_state(amimulator_call:direction(Call), Call),

    %% The second leg of a call might update CID of first
    maybe_update_other_call_dest(CallId, amimulator_call:other_leg_call_id(Call), Call),
    {'next_state', 'created', State#state{call_ids = add_call_id(CallId, CallIds)}};

pre_create({'answer', CallId}, State) ->
    lager:debug("early answer for call with id ~p", [CallId]),
    ami_sm:flag_early_answer(CallId),
    {'next_state', 'pre_create', State#state{answered=CallId}}.

-spec created({'new_call' | 'add_initial' | 'answer' | 'bridge' | 'destroy'}
             ,state()) -> handle_fsm_ret(state()).
created({'new_call', Call}, #state{call_ids=CallIds}=State) ->
    CallId = amimulator_call:call_id(Call),
    {'next_state', 'created', State#state{call_ids = add_call_id(CallId, CallIds)}};

created({'add_initial', Call}, #state{call_ids=CallIds}=State) ->
    CallId = amimulator_call:call_id(Call),
    case amimulator_call:answered(Call) of
        'true' -> {'next_state', 'answered', State#state{call_ids = add_call_id(CallId, CallIds)
                                                         ,answered=CallId
                                                        }};
        _ -> {'next_state', 'created', State#state{call_ids = add_call_id(CallId, CallIds)}}
    end;

created({'answer', CallId}, #state{early_bridge_payload=EventJObj}=State) ->
    Call = ami_sm:call(CallId),
    Call2 = amimulator_call:set_answered('true', Call),

    ami_sm:answer(amimulator_call:channel(Call2), CallId),
    ami_sm:update_call(Call2),
    busy_state(Call2, CallId),
    maybe_dial_event(Call),

    lager:debug("moving to answered for call ~p", [CallId]),

    case EventJObj of
        'undefined' -> 'ok';
        _ ->
            lager:debug("using early bridge payload right now"),
            bridge(self(), EventJObj)
    end,

    {'next_state', 'answered', State#state{answered=CallId}};

created({'bridge', EventJObj}, State) ->
    lager:debug("early bridge; probably offnet call"),
    {'next_state', 'created', State#state{early_bridge_payload=EventJObj}};

created({'destroy', Reason, CallId}, #state{monitored_channel=Channel
                                            ,call_ids=CallIds
                                           }=State) ->
    Call = ami_sm:call(CallId),
    ami_sm:delete_call(CallId),
    case delete_call_id(CallId, CallIds) of
        [] ->
            destroy_channel(Reason, Call),
            lager:debug("channel ~p's last call id ~p destroyed", [Channel, CallId]),
            {'stop', 'normal', State};
        CallIds2 ->
            lager:debug("channel ~p has call ids remaining: ~p", [Channel, CallIds2]),
            {'next_state', 'created', State#state{call_ids=CallIds2}}
    end.

-spec answered({'new_call' | 'add_initial' | 'answer' | 'bridge' | 'destroy'}
              ,state()) -> handle_fsm_ret(state()).
answered({'new_call', Call}, #state{call_ids=CallIds}=State) ->
    CallId = amimulator_call:call_id(Call),
    {'next_state', 'answered', State#state{call_ids = add_call_id(CallId, CallIds)}};

answered({'add_initial', Call}, #state{call_ids=CallIds}=State) ->
    CallId = amimulator_call:call_id(Call),
    {'next_state', 'answered', State#state{call_ids = add_call_id(CallId, CallIds)}};

answered({'answer', CallId}, #state{monitored_channel=Channel}=State) ->
    lager:debug("received a second answer for channel ~p on call with id ~p", [Channel, CallId]),
    {'next_state', 'answered', State};

answered({'bridge', EventJObj}, State) ->
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    OtherCallId = kz_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),
    OtherCall = ami_sm:call(OtherCallId),

    %% Sometimes the conferences will appear on the other media server, causing crashes
    %% Duplicate this one so the channel is in ami_sm
    case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Is-Conference">>], EventJObj) of
        <<"true">> ->
            Dupe = amimulator_call:set_call_id(OtherCallId, Call),
            ami_sm:new_call(Dupe),
            answered_bridge_to_conf(Call, Dupe, State);
        _ -> answered_bridge(Call, OtherCall, State)
    end;
answered({'destroy', Reason, CallId}, #state{monitored_channel=Channel
                                             ,call_ids=CallIds
                                             ,answered=Answered
                                             ,conference_call_id=ConferenceCallId
                                            }=State) ->
    if CallId =:= Answered ->
        Call = ami_sm:call(CallId),
        destroy_channel(Reason, Call),
        ami_sm:delete_call(CallId),
        lager:debug("channel ~p's answered call (~p) destroyed", [Channel, CallId]),

        case delete_call_id(CallId, CallIds) of
            [] -> 'ok';
            ExtraCallIds ->
                lager:debug("prevent leak - purge extra call ids (such as during transfer) ~p", [ExtraCallIds]),
                lists:foreach(fun(CallId2) ->
                    ami_sm:delete_call(CallId2)
                end, ExtraCallIds)
        end,

        case ConferenceCallId of
            'undefined' -> 'ok';
            _ ->
                lager:debug("deleting conference bridge ~s", [ConferenceCallId]),
                ami_sm:delete_call(ConferenceCallId)
        end,

        {'stop', 'normal', State};
    'true' ->
        ami_sm:delete_call(CallId),
        {'next_state', 'answered', State#state{call_ids = delete_call_id(CallId, CallIds)}}
    end.

%%
%% private functions
%%

% -spec initialize(
initialize(Super, Call) ->
    State = #state{supervisor=Super
                   ,monitored_channel=amimulator_call:channel(Call)
                   ,call_ids = [amimulator_call:call_id(Call)]
                  },
    case amimulator_call:answered(Call) of
        'true' -> {'ok', 'answered', State#state{answered=amimulator_call:call_id(Call)}};
        _ -> {'ok', 'created', State}
    end.

-spec maybe_update_other_call_dest(api_binary(), api_binary(), amimulator_call:call()) -> 'ok'.
maybe_update_other_call_dest(_, 'undefined', _) ->
    'ok';
maybe_update_other_call_dest(CallId, CallId, _) ->
    'ok';
maybe_update_other_call_dest(_, OtherCallId, Call) ->
    case ami_sm:call(OtherCallId) of
        'undefined' -> 'ok';
        OtherCall -> ami_sm:update_call(amimulator_call:update_from_other(Call, OtherCall))
    end.

add_call_id(CallId, CallIds) ->
    NoDupes = delete_call_id(CallId, CallIds),
    [CallId | NoDupes].

delete_call_id(CallId, CallIds) ->
    delete_call_id(CallId, CallIds, []).

delete_call_id(_, [], CallIds) ->
    CallIds;
delete_call_id(CallId, [CallId|B], CallIds) ->
    delete_call_id(CallId, B, CallIds);
delete_call_id(CallId, [A|B], CallIds) ->
    delete_call_id(CallId, B, [A | CallIds]).

answered_bridge(Call, 'undefined', State) ->
    CallId = amimulator_call:call_id(Call),
    lager:debug("the other call may be from an unhooked kazoo account"),
    lager:debug("updating answered id to ~p anyway", [CallId]),
    {'next_state', 'answered', State#state{answered=CallId}};
answered_bridge(Call, OtherCall, State) ->
    CallId = amimulator_call:call_id(Call),
    OtherCallId = amimulator_call:call_id(OtherCall),
    Channel = amimulator_call:channel(Call),
    OtherChannel = amimulator_call:channel(OtherCall),

    case Channel of
        OtherChannel -> {'next_state', 'answered', State};
        _ ->
            lager:debug("bridge, updating answered id to ~p", [CallId]),
            case ami_sm:call(<<CallId/binary, "-queue;2">>) of
                'undefined' ->
                    FlipDir = amimulator_call:ccv(<<"Flip-Direction-On-Bridge">>, Call) =:= <<"true">> orelse
                        amimulator_call:ccv(<<"Device-QuickCall">>, Call) =:= <<"true">>,
                    RevDirCall = case FlipDir of
                        'true' ->
                            Direction = case amimulator_call:direction(Call) of
                                <<"inbound">> -> <<"outbound">>;
                                <<"outbound">> -> <<"inbound">>
                            end,
                            amimulator_call:delete_ccv(<<"Device-QuickCall">>,
                                amimulator_call:delete_ccv(<<"Flip-Direction-On-Bridge">>, amimulator_call:set_direction(Direction, Call)));
                        'false' -> Call
                    end,
                    Call2 = amimulator_call:update_from_other(OtherCall, RevDirCall),
                    OtherCall2 = amimulator_call:update_from_other(RevDirCall, OtherCall),
                    ami_sm:update_call(Call2),
                    ami_sm:update_call(OtherCall2),

                    maybe_bridge(Call2, OtherCall2),
                    {'next_state', 'answered', State#state{answered=CallId}};
                LocalCall2 ->
                    MemberCall = ami_sm:call(OtherCallId),
                    LocalCall1 = ami_sm:call(<<CallId/binary, "-queue;1">>),

                    MemberCall2 = amimulator_call:set_other_leg_call_id(amimulator_call:call_id(LocalCall1), MemberCall),
                    MemberCall3 = amimulator_call:set_other_channel(amimulator_call:channel(LocalCall1), MemberCall2),

                    Call2 = amimulator_call:set_other_leg_call_id(amimulator_call:call_id(LocalCall2), Call),
                    Call3 = amimulator_call:set_other_channel(amimulator_call:channel(LocalCall2), Call2),

                    ami_sm:update_call(MemberCall3),
                    ami_sm:update_call(Call3),

                    maybe_bridge(Call3, LocalCall2),
                    {'next_state', 'answered', State#state{answered=CallId}}
            end
    end.

answered_bridge_to_conf(Call, OtherCall, State) ->
    {Action, StateName, State2} = answered_bridge(Call, OtherCall, State),
    {Action, StateName, State2#state{conference_call_id=amimulator_call:call_id(OtherCall)}}.

new_channel_event(<<"inbound">>, Call) ->
    CallId = amimulator_call:call_id(Call),
    SourceExten = amimulator_call:id_number(Call),
    DestExten = amimulator_call:other_id_number(Call),
    EndpointName = amimulator_call:channel(Call),

    SourceCID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
        <<"Voicemail">>;
    'true' ->
        amimulator_call:id_name(Call)
    end,

    Payload = new_channel_payload(EndpointName, amimulator_call:id_number(Call), SourceCID, DestExten, CallId),
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call));
new_channel_event(<<"outbound">>, Call) ->
    CallId = amimulator_call:call_id(Call),
    SourceCID = amimulator_call:id_name(Call),
    EndpointName = amimulator_call:channel(Call),

    % case EndpointName of
    %     undefined ->
    %       lager:debug("Error: Endpoint name undefined"),
    %         lager:debug("Call ~p", [Call]);
    %     _ ->
    %         ok
    % end,

    Payload = new_channel_payload(EndpointName, amimulator_call:id_number(Call), SourceCID, CallId),
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call)).

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
        {<<"ConnectedLineNum">>, <<>>},
        {<<"ConnectedLineName">>, <<>>},
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
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call)).

maybe_newexten(<<"inbound">>, Call) ->
    Payload = [{<<"Event">>, <<"Newexten">>}
               ,{<<"Privilege">>, <<"dialplan,all">>}
               ,{<<"Channel">>, amimulator_call:channel(Call)}
               ,{<<"Context">>, <<"from-internal">>}
               ,{<<"Extension">>, amimulator_call:other_id_number(Call)}
               ,{<<"Priority">>, 1}
               ,{<<"Application">>, <<"Macro">>}
               ,{<<"AppData">>, <<"user-callerid,LIMIT,">>}
               ,{<<"Uniqueid">>, amimulator_call:call_id(Call)}
              ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call));
maybe_newexten(_, _) ->
    'ok'.

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
            Payload = dial(EndpointName, 'undefined', SourceExten, SourceCID, DestExten, DestCID, CallId, 'undefined', DestCID),
            amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call));
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

    case OtherCall of
        'undefined' -> lager:debug("couldn't find other call, probably hung up quickly");
        _ ->
            case amimulator_call:other_id_number(OtherCall) of
                'undefined' -> 'ok';
                _OtherDialed ->
                    case EndpointName of
                        OtherEndpointName -> 'ok';
                        _ ->
                            Payload = case amimulator_call:direction(Call) of
                                <<"inbound">> -> dial(EndpointName, OtherEndpointName, SourceExten, CID, DestExten, OtherCID, CallId, OtherCallId, OtherCID);
                                <<"outbound">> -> dial(OtherEndpointName, EndpointName, DestExten, OtherCID, SourceExten, CID, OtherCallId, CallId, CID)
                            end,
                            amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call))
                    end
            end
    end.

maybe_bridge(Call, OtherCall) ->
    CallId = amimulator_call:call_id(Call),
    OtherCallId = amimulator_call:call_id(OtherCall),
    Channel1 = amimulator_call:channel(Call),
    Channel2 = amimulator_call:other_channel(Call),

    %% Don't publish bridge and dial if the channels are bridging to themselves, just causes problems
    if Channel1 =:= Channel2 ->
        'ok';
    'true' ->
        CIDNum = amimulator_call:id_number(Call),
        OtherCIDNum = amimulator_call:other_id_number(Call),

        Payload = case amimulator_call:direction(Call) of
            <<"inbound">> ->
                bridge(Channel1, Channel2, CallId, OtherCallId, CIDNum, OtherCIDNum);
            <<"outbound">> ->
                bridge(Channel2, Channel1, OtherCallId, CallId, OtherCIDNum, CIDNum)
        end,
        amimulator_event_listener:publish_amqp_event({publish, Payload}, amimulator_call:account_id(Call))
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

bridge(SourceChannel, DestChannel, SourceCallId, DestCallId, SourceCID, DestCID) ->
    [
        {<<"Event">>, <<"Bridge">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Bridgestate">>, <<"Link">>},
        {<<"Bridgetype">>, <<"core">>},
        {<<"Channel1">>, SourceChannel},
        {<<"Channel2">>, DestChannel},
        {<<"Uniqueid1">>, SourceCallId},
        {<<"Uniqueid2">>, DestCallId},
        {<<"CallerID1">>, SourceCID},
        {<<"CallerID2">>, DestCID}
    ].

new_state(<<"inbound">>, Call) ->
    CallId = amimulator_call:call_id(Call),
    SourceExten = amimulator_call:id_number(Call),
    DestExten = amimulator_call:other_id_number(Call),
    EndpointName = amimulator_call:channel(Call),

    SourceCID = if (DestExten =:= SourceExten) or (DestExten =:= <<"*97">>) ->
        <<"Voicemail">>;
    'true' ->
        amimulator_call:id_name(Call)
    end,

    % OtherCID = case DestExten of
    %     SourceExten ->
    %         <<"Voicemail">>;
    %     _ ->
    %         maybe_internal_cid(WhappsCall,
    %             hd(binary:split(kz_json:get_value(<<"To">>, EventJObj), <<"@">>)))
    % end,

    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, 4},
        {<<"ChannelStateDesc">>, <<"Ring">>},
        {<<"CallerIDNum">>, SourceExten},
        {<<"CallerIDName">>, SourceCID},
        {<<"ConnectedLineNum">>, <<>>},
        {<<"ConnectedLineName">>, <<>>},
        {<<"Uniqueid">>, CallId}
    ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call));
new_state(<<"outbound">>, Call) ->
    CallId = amimulator_call:call_id(Call),
    CIDNum = amimulator_call:id_number(Call),
    CID = amimulator_call:id_name(Call),
    OtherCIDNum = amimulator_call:other_id_number(Call),
    OtherCID = amimulator_call:other_id_name(Call),
    EndpointName = amimulator_call:channel(Call),

    % OtherCallId = kapps_call:other_leg_call_id(WhappsCall),
    % OtherCall = ami_sm:call(OtherCallId),

    % OtherCID = case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
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
    %             kapps_call:account_db(WhappsCall)
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
        {<<"CallerIDNum">>, CIDNum},
        {<<"CallerIDName">>, CID},
        {<<"ConnectedLineNum">>, OtherCIDNum},
        {<<"ConnectedLineName">>, OtherCID},
        {<<"Uniqueid">>, CallId}
    ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call)).

busy_state(Call, CallId) ->
    EndpointName = amimulator_call:channel(Call),
    CIDNum = amimulator_call:id_number(Call),
    CID = amimulator_call:id_name(Call),
    OtherCIDNum = amimulator_call:other_id_number(Call),
    OtherCID = amimulator_call:other_id_name(Call),

    % OtherCallId = kapps_call:other_leg_call_id(WhappsCall),
    % OtherCall = ami_sm:call(OtherCallId),

    % OtherCID = case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
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
    %             kapps_call:account_db(WhappsCall)
    %         ),
    %         <<"Queue ", Number/binary, " Call">>
    % end,

    Payload = [
        {<<"Event">>, <<"Newstate">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"ChannelState">>, ?STATE_UP},
        {<<"ChannelStateDesc">>, <<"Up">>},
        {<<"CallerIDNum">>, CIDNum},
        {<<"CallerIDName">>, CID},
        {<<"ConnectedLineNum">>, OtherCIDNum},
        {<<"ConnectedLineName">>, OtherCID},
        {<<"Uniqueid">>, CallId}
    ],
    amimulator_event_listener:publish_amqp_event({publish, Payload}, amimulator_call:account_id(Call)).

destroy_channel(Reason, Call) ->
    CallId = amimulator_call:call_id(Call),
    CIDNum = amimulator_call:id_number(Call),
    CID = amimulator_call:id_name(Call),
    OtherCIDNum = amimulator_call:other_id_number(Call),
    OtherCID = amimulator_call:other_id_name(Call),
    EndpointName = amimulator_call:channel(Call),

    % DestCID = case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], EventJObj) of
    %     undefined ->
    %         props:get_value(<<"bleg_cid">>, Call);
    %     QueueId ->
    %         AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], EventJObj),
    %         case amimulator_util:find_id_number(
    %             QueueId,
    %             kz_util:format_account_id(AccountId, encoded)
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

    Payload = [[
        {<<"Event">>, <<"Hangup">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"Uniqueid">>, CallId},
        {<<"CallerIDNum">>, CIDNum},
        {<<"CallerIDName">>, CID},
        {<<"ConnectedLineNum">>, OtherCIDNum},
        {<<"ConnectedLineName">>, OtherCID},
        {<<"Cause">>, Cause},
        {<<"Cause-txt">>, CauseText}
    ]],

    amimulator_event_listener:publish_amqp_event({publish, Payload}, amimulator_call:account_id(Call)).
