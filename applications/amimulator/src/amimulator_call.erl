-module(amimulator_call).

-include("amimulator.hrl").
-include("amimulator_call.hrl").

-export([from_json/1]).
-export([to_kapps_call/1]).
-export([update_from_other/2]).
-export([call_id/1, set_call_id/2]).
-export([other_leg_call_id/1, set_other_leg_call_id/2]).
-export([channel/1, short_channel/1, set_channel/1, set_channel/2]).
-export([other_channel/1, set_other_channel/2]).
-export([account_id/1, account_db/1]).
-export([authorizing_id/1, authorizing_type/1]).
-export([ccv/2, delete_ccv/2]).
-export([control_queue/1, set_control_queue/2]).
-export([acdc_queue_id/1, set_acdc_queue_id/2]).
-export([agent_id/1, set_agent_id/2]).
-export([username/1]).
-export([to_user/1]).
-export([from_user/1]).
-export([id_name/1, id_number/1]).
-export([other_id_name/1, other_id_number/1]).
-export([direction/1, set_direction/2]).
-export([answered/1, set_answered/2]).
-export([elapsed_s/1]).
-export([caller_id_name/1, caller_id_number/1, set_caller_id_name/2, set_caller_id_number/2]).
-export([callee_id_name/1, callee_id_number/1, set_callee_id_name/2, set_callee_id_number/2]).
-export([endpoint/1]).
-export([user/1]).

-type call() :: #call{}.
-export_type([call/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% API
%% @end
%%--------------------------------------------------------------------

-spec from_json(kz_json:objects() | kz_json:object()) -> call().
from_json(JObjs) when is_list(JObjs) ->
    lists:foldl(
      fun(JObj, Calls) ->
        [from_json(JObj) | Calls]
      end
      ,[]
      ,JObjs);
from_json(JObj) ->
    Call = #call{call_id = kz_json:get_first_defined([<<"uuid">>, <<"Call-ID">>], JObj)
                 ,other_leg_call_id = kz_json:get_first_defined([<<"other_leg">>, <<"Other-Leg-Call-ID">>], JObj)
                 ,account_id = kz_json:get_first_defined([<<"account_id">>, [<<"Custom-Channel-Vars">>, <<"Account-ID">>]], JObj)
                 ,authorizing_id = kz_json:get_first_defined([<<"authorizing_id">>, [<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>]], JObj)
                 ,authorizing_type = kz_json:get_first_defined([<<"authorizing_type">>, <<"Authorizing-Type">>], JObj)
                 ,custom_channel_vars = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new())
                 ,control_q = kz_json:get_value(<<"Control-Queue">>, JObj)
                 ,acdc_queue_id = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], JObj)
                 ,agent_id = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Agent-ID">>], JObj)
                 %,conference_id
                 ,username = kz_json:get_first_defined([<<"username">>, <<"Username">>], JObj)
                 ,to = case binary:split(kz_json:get_first_defined([<<"destination">>, <<"To">>], JObj, <<"@">>), <<"@">>) of
                           [ToUser, ToRealm] -> <<ToUser/binary, "@", ToRealm/binary>>;
                           [ToUser] -> <<ToUser/binary, "@">>
                       end
                 ,from = case kz_json:get_value(<<"From">>, JObj) of
                             'undefined' ->
                                 case kz_json:get_first_defined([<<"direction">>, <<"Call-Direction">>], JObj) of
                                     <<"inbound">> ->
                                        case kz_json:get_first_defined([<<"Caller-ID-Number">>, <<"caller_id">>], JObj) of
                                            'undefined' ->
                                                lager:debug("JObj ~p", [JObj]),
                                                <<"@">>;
                                            CallerIdNumber -> <<CallerIdNumber/binary, "@">>
                                        end;
                                     <<"outbound">> -> <<"@">>
                                 end;
                             From -> From
                         end
                 ,direction = kz_json:get_first_defined([<<"direction">>, <<"Call-Direction">>], JObj)
                 ,answered = kz_json:is_true(<<"answered">>, JObj, 'undefined')
                 ,timestamp = case kz_json:get_integer_value(<<"timestamp">>, JObj) of
                                  'undefined' ->
                                      Sec = kz_json:get_first_defined([<<"elapsed_s">>, <<"Duration-Seconds">>], JObj, 0),
                                      kz_util:current_tstamp() - Sec;
                                  Timestamp -> Timestamp
                              end
                 ,caller_id_name = kz_json:get_first_defined([<<"Caller-ID-Name">>, <<"caller_id">>], JObj, kz_util:anonymous_caller_id_name())
                 ,caller_id_number = kz_json:get_first_defined([<<"Caller-ID-Number">>, <<"caller_id">>], JObj, kz_util:anonymous_caller_id_number())

                 ,callee_id_name = case kz_json:get_value(<<"Callee-ID-Name">>, JObj) of
                    'undefined' ->
                       case binary:split(kz_json:get_first_defined([<<"destination">>, <<"To">>], JObj, <<"@">>), <<"@">>) of
                           [ToUser, _] -> ToUser;
                           [ToUser] -> ToUser
                       end;
                    CalleeIdName -> CalleeIdName
                  end
                 ,callee_id_number = case kz_json:get_value(<<"Callee-ID-Number">>, JObj) of
                    'undefined' ->
                       case binary:split(kz_json:get_first_defined([<<"destination">>, <<"To">>], JObj, <<"@">>), <<"@">>) of
                           [ToUser, _] -> ToUser;
                           [ToUser] -> ToUser
                       end;
                    CalleeIdName -> CalleeIdName
                  end
                },
    update_post_create(Call).

-spec to_kapps_call(call()) -> kapps_call:call().
to_kapps_call(#call{call_id=CallId
                     ,other_leg_call_id=OtherLegCallId
                     ,account_id=AccountId
                     ,authorizing_id=AuthorizingId
                     ,authorizing_type=AuthorizingType
                     ,custom_channel_vars=CCVs
                     ,control_q=ControlQueue
                     ,to=To
                     ,from=From
                     ,caller_id_name=CIDName
                     ,caller_id_number=CIDNumber
                     ,callee_id_name=CalleeIdName
                     ,callee_id_number=CalleeIdNumber
                    }) ->
    Setters = [fun(Call) -> kapps_call:set_call_id(CallId, Call) end
               ,fun(Call) -> kapps_call:set_other_leg_call_id(OtherLegCallId, Call) end
               ,fun(Call) -> kapps_call:set_account_id(AccountId, Call) end
               ,fun(Call) ->
                    case AuthorizingId of
                        'undefined' -> Call;
                        _ -> kapps_call:set_authorizing_id(AuthorizingId, Call) end
                    end
               ,fun(Call) ->
                    case AuthorizingType of
                        'undefined' -> Call;
                        _ -> kapps_call:set_authorizing_type(AuthorizingType, Call)
                    end
                end
               ,fun(Call) -> kapps_call:set_custom_channel_vars(kz_json:to_proplist(CCVs), Call) end
               ,fun(Call) ->
                    case ControlQueue of
                        'undefined' -> Call;
                        _ -> kapps_call:set_control_queue(ControlQueue, Call)
                    end
                end
               ,fun(Call) -> kapps_call:set_to(To, Call) end
               ,fun(Call) ->
                    case binary:split(From, <<"@">>) of
                        [_ToUser, _ToRealm] ->
                            kapps_call:set_from(From, Call);
                        [_ToUser] ->
                            kapps_call:set_from(<<From/binary, "@">>, Call)
                    end
                end
               ,fun(Call) ->
                    case CIDName of
                        'undefined' -> Call;
                        _ -> kapps_call:set_caller_id_name(CIDName, Call)
                    end
                end
               ,fun(Call) -> kapps_call:set_caller_id_number(CIDNumber, Call) end
               ,fun(Call) ->
                    case CalleeIdName of
                        'undefined' -> Call;
                        _ -> kapps_call:set_callee_id_name(CalleeIdName, Call)
                    end
                end
               ,fun(Call) ->
                    case CalleeIdNumber of
                        'undefined' -> Call;
                        _ -> kapps_call:set_callee_id_number(CalleeIdNumber, Call)
                    end
                end
              ],
    lists:foldl(fun(Setter, Call) -> Setter(Call) end, kapps_call:new(), Setters).

-spec update_from_other(call(), call()) -> call().
update_from_other('undefined', Call) ->
    Call;
update_from_other(OtherCall, #call{direction=Direction}=Call) ->
    Updaters = [fun(Call2) -> set_other_leg_call_id(call_id(OtherCall), Call2) end
                ,fun(Call2) -> set_other_channel(channel(OtherCall), Call2) end
               ],
    case {ccv(<<"Flip-Direction-On-Bridge">>, OtherCall), ccv(<<"Device-QuickCall">>, OtherCall)} of
        {<<"true">>, _} ->
            set_other_channel(channel(OtherCall), Call);
        {_, <<"true">>} ->
            Updaters2 = [fun(Call2) -> set_caller_id_name(callee_id_name(OtherCall), Call2) end
                         ,fun(Call2) -> set_caller_id_number(callee_id_number(OtherCall), Call2) end
                         ,fun(Call2) -> set_other_channel(channel(OtherCall), Call2) end
                        ],
            lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters2);
        _ ->
            directional_update_from_other(Direction
                                          ,OtherCall
                                          ,lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters))
    end.

-spec call_id(call()) -> api_binary().
call_id(#call{call_id=CallId}) ->
    CallId.

-spec set_call_id(api_binary(), call()) -> call().
set_call_id(CallId, Call) ->
    Call#call{call_id=CallId}.

-spec other_leg_call_id(call()) -> api_binary().
other_leg_call_id(#call{other_leg_call_id=OtherLegCallId}) ->
    OtherLegCallId.

-spec set_other_leg_call_id(api_binary(), call()) -> call().
set_other_leg_call_id(CallId, Call) ->
    Call#call{other_leg_call_id=CallId}.

-spec channel(call()) -> api_binary().
channel(#call{channel=Channel}) ->
    Channel.

-spec short_channel(call()) -> api_binary().
short_channel(#call{channel='undefined'}) ->
    'undefined';
short_channel(#call{channel=Channel}) ->
    hd(binary:split(Channel, <<"-">>)).

-spec set_channel(call()) -> call().
-spec set_channel(api_binary(), call()) -> call().
set_channel(Call) ->
    set_channel(amimulator_util:channel_string(Call), Call).

set_channel(Channel, Call) ->
    Call#call{channel=Channel}.

-spec other_channel(call()) -> api_binary().
other_channel(#call{other_channel=Channel}) ->
    Channel.

-spec set_other_channel(api_binary(), call()) -> call().
set_other_channel(Channel, Call) ->
    Call#call{other_channel=Channel}.

-spec account_id(call()) -> api_binary().
account_id(#call{account_id=AccountId}) ->
    AccountId.

-spec account_db(call()) -> api_binary().
account_db(#call{account_id=AccountId}) ->
    kz_util:format_account_id(AccountId, 'encoded').

-spec authorizing_id(call()) -> api_binary().
authorizing_id(#call{authorizing_id=AuthorizingId}) ->
    AuthorizingId.

-spec authorizing_type(call()) -> api_binary().
authorizing_type(#call{authorizing_type=AuthorizingType}) ->
    AuthorizingType.

-spec ccv(kz_json:key(), call()) -> kz_json:json_term() | 'undefined'.
ccv(Key, #call{custom_channel_vars=CCVs}) ->
    kz_json:get_value(Key, CCVs).

-spec set_ccv(kz_json:key(), kz_json:json_term(), call()) -> call().
set_ccv(Key, Value, #call{custom_channel_vars=CCVs}=Call) ->
    Call#call{custom_channel_vars=kz_json:set_value(Key, Value, CCVs)}.

-spec delete_ccv(kz_json:key(), call()) -> call().
delete_ccv(Key, #call{custom_channel_vars=CCVs}=Call) ->
    Call#call{custom_channel_vars=kz_json:delete_key(Key, CCVs)}.

-spec control_queue(call()) -> api_binary().
control_queue(#call{call_id=CallId
                    ,control_q='undefined'
                   }) ->
    Req = props:filter_undefined([{<<"Call-ID">>, CallId}
                                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    ReqResp = kapps_util:amqp_pool_request(Req
                                            ,fun kapi_amimulator:publish_control_queue_req/1
                                            ,fun kapi_amimulator:control_queue_resp_v/1
                                           ),
    case ReqResp of
        {'error', 'timeout'} ->
            'undefined';
            % case kapps_call:other_leg_call_id(WhappsCall) of
            %     'undefined' ->
            %         {'error', 'not_found'};
            %     OtherCallId ->
            %         ReqResp2 = kapps_util:amqp_pool_request(props:set_value(<<"Call-ID">>, OtherCallId, Req)
            %                                                  ,fun kapi_amimulator:publish_control_queue_req/1
            %                                                  ,fun kapi_amimulator:control_queue_resp_v/1
            %                                                 ),
            %         case ReqResp2 of
            %             {'error', _E}=Error -> Error;
            %             {'ok', Resp} ->
            %                 {'ok', Resp}
            %         end
            % end;
        {'error', E} ->
            lager:debug("could not fetch control queue for ~p (~p)", [CallId, E]),
            'undefined';
        {'ok', Resp} ->
            kz_json:get_value(<<"Control-Queue">>, Resp)
    end;
control_queue(#call{control_q=ControlQueue}) ->
    ControlQueue.

-spec set_control_queue(api_binary(), call()) -> call().
set_control_queue(ControlQueue, Call) ->
    Call#call{control_q=ControlQueue}.

-spec acdc_queue_id(call()) -> api_binary().
acdc_queue_id(#call{acdc_queue_id=QueueId}) ->
    QueueId.

-spec set_acdc_queue_id(api_binary(), call()) -> call().
set_acdc_queue_id(QueueId, Call) ->
    Call#call{acdc_queue_id=QueueId}.

-spec agent_id(call()) -> api_binary().
agent_id(#call{agent_id=AgentId}) ->
    AgentId.

-spec set_agent_id(api_binary(), call()) -> call().
set_agent_id(AgentId, Call) ->
    Call#call{agent_id=AgentId}.

-spec username(call()) -> api_binary().
username(#call{username=Username}) ->
    Username.

-spec to_user(call()) -> ne_binary().
to_user(#call{to=To}) ->
    parse_user_at_realm('user', To).

-spec from_user(call()) -> ne_binary().
from_user(#call{from=From}) ->
    parse_user_at_realm('user', From).

-spec id_name(call()) -> api_binary().
id_name(#call{direction = <<"inbound">>
              ,caller_id_name=CIDName
             }) ->
    CIDName;
id_name(#call{direction = <<"outbound">>
              ,callee_id_name=CIDName
             }) ->
    CIDName.

-spec id_number(call()) -> api_binary().
id_number(#call{direction = <<"inbound">>
                ,caller_id_number=CIDNumber
               }) ->
    CIDNumber;
id_number(#call{direction = <<"outbound">>
                ,callee_id_number=CIDNumber
               }) ->
    CIDNumber.

-spec other_id_name(call()) -> api_binary().
other_id_name(#call{direction = <<"inbound">>
              ,callee_id_name=CIDName
             }) ->
    CIDName;
other_id_name(#call{direction = <<"outbound">>
              ,caller_id_name=CIDName
             }) ->
    CIDName.

-spec other_id_number(call()) -> api_binary().
other_id_number(#call{direction = <<"inbound">>
                ,callee_id_number=CIDNumber
               }) ->
    CIDNumber;
other_id_number(#call{direction = <<"outbound">>
                ,caller_id_number=CIDNumber
               }) ->
    CIDNumber.

-spec direction(call()) -> api_binary().
direction(#call{direction=Direction}) ->
    Direction.

-spec set_direction(api_binary(), call()) -> call().
set_direction(Direction, Call) ->
    Call#call{direction=Direction}.

-spec answered(call()) -> api_boolean().
answered(#call{answered=Answered}) ->
    Answered.

-spec set_answered(boolean(), call()) -> call().
set_answered(Answered, Call) ->
    Call#call{answered=Answered}.

-spec elapsed_s(call()) -> api_integer().
elapsed_s(#call{timestamp=Timestamp}) ->
    kz_util:current_tstamp() - Timestamp.

-spec caller_id_name(call()) -> api_binary().
caller_id_name(#call{caller_id_name=CIDName}) ->
    CIDName.

-spec caller_id_number(call()) -> api_binary().
caller_id_number(#call{caller_id_number=CIDNumber}) ->
    CIDNumber.

-spec set_caller_id_name(api_binary(), call()) -> call().
set_caller_id_name(CIDName, Call) ->
    Call#call{caller_id_name=CIDName}.

-spec set_caller_id_number(api_binary(), call()) -> call().
set_caller_id_number(CIDNumber, Call) ->
    Call#call{caller_id_number=CIDNumber}.

-spec callee_id_name(call()) -> api_binary().
callee_id_name(#call{callee_id_name=CIDName}) ->
    CIDName.

-spec callee_id_number(call()) -> api_binary().
callee_id_number(#call{callee_id_number=CIDNumber}) ->
    CIDNumber.

-spec set_callee_id_name(api_binary(), call()) -> call().
set_callee_id_name(CIDName, Call) ->
    Call#call{callee_id_name=CIDName}.

-spec set_callee_id_number(api_binary(), call()) -> call().
set_callee_id_number(CIDNumber, Call) ->
    Call#call{callee_id_number=CIDNumber}.

-spec endpoint(call()) -> api_object().
endpoint(Call) ->
    case cf_endpoint:get(to_kapps_call(Call)) of
        {'ok', Endpoint} -> Endpoint;
        {'error', _} -> maybe_cellphone_endpoint(Call)
    end.

-spec user(call()) -> api_object().
user(Call) ->
    maybe_endpoint_owner(endpoint(Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------

-spec update_post_create(call()) -> call().
update_post_create(Call) ->
    Updaters = [fun unset_other_leg_self/1
                ,fun set_quickcall_ccv/1
                ,fun unset_offnet_authorizing_id/1
                ,fun set_id/1
                ,fun set_other_id/1
                ,fun set_channel/1
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters).

-spec directional_update_from_other(api_binary(), call(), call()) -> call().
directional_update_from_other(<<"inbound">>, OtherCall, Call) ->
    Updaters = [fun(Call2) -> set_callee_id_name(callee_id_name(OtherCall), Call2) end
                ,fun(Call2) -> set_callee_id_number(callee_id_number(OtherCall), Call2) end
                ,fun(Call2) -> set_other_channel(channel(OtherCall), Call2) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters);
directional_update_from_other(<<"outbound">>, OtherCall, Call) ->
    Updaters = [fun(Call2) -> set_caller_id_name(caller_id_name(OtherCall), Call2) end
                ,fun(Call2) -> set_caller_id_number(caller_id_number(OtherCall), Call2) end
                ,fun(Call2) -> set_other_channel(channel(OtherCall), Call2) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters).

-spec unset_other_leg_self(call()) -> call().
unset_other_leg_self(#call{call_id=CallId
                           ,other_leg_call_id=CallId
                          }=Call) ->
    Call#call{other_leg_call_id='undefined'};
unset_other_leg_self(Call) ->
    Call.

-spec set_quickcall_ccv(call()) -> call().
set_quickcall_ccv(Call) ->
    case caller_id_name(Call) of
        <<"Device QuickCall">> -> set_ccv(<<"Device-QuickCall">>, <<"true">>, Call);
        _ -> Call
    end.

-spec unset_offnet_authorizing_id(call()) -> call().
unset_offnet_authorizing_id(#call{custom_channel_vars=CCVs}=Call) ->
    %% Use E164-Destination as indicator that this is an offnet leg
    %% and therefore the authorizing id is not valid for the destination
    case kz_json:get_value(<<"E164-Destination">>, CCVs) of
        'undefined' -> Call;
        _ -> Call#call{authorizing_id='undefined'
                       ,custom_channel_vars=kz_json:delete_key(<<"Authorizing-ID">>, CCVs)
                      }
    end.

-spec set_id(call()) -> call().
-spec set_id(api_binary(), {api_binary(), api_binary()} | 'undefined', call()) -> call().
set_id(#call{direction=Direction}=Call) ->
    CallerId = case endpoint(Call) of
        'undefined' -> 'undefined';
        Endpoint -> endpoint_caller_id(Endpoint, Call)
    end,
    set_id(Direction, CallerId, Call).

set_id(_, 'undefined', Call) ->
    Call;
set_id(<<"inbound">>, {Name, Number}, Call) ->
    Updaters = [fun(Call2) -> set_caller_id_name(Name, Call2) end
                ,fun(Call2) -> set_caller_id_number(Number, Call2) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters);
set_id(<<"outbound">>, {Name, Number}, Call) ->
    Updaters = [fun(Call2) -> set_callee_id_name(Name, Call2) end
                ,fun(Call2) -> set_callee_id_number(Number, Call2) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters).

-spec set_other_id(call()) -> call().
-spec set_other_id(api_binary(), call()) -> call().
set_other_id(#call{direction=Direction}=Call) ->
    set_other_id(Direction, Call).

set_other_id(<<"inbound">>, #call{callee_id_name=CIDName
                                  ,callee_id_number=CIDNumber
                                 }=Call) when (CIDName =:= 'undefined') or (CIDNumber =:= 'undefined') ->
    Updaters = [fun(Call2) -> set_callee_id_name(amimulator_call:to_user(Call2), Call2) end
                ,fun(Call2) -> set_callee_id_number(amimulator_call:to_user(Call2), Call2) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters);
set_other_id(<<"inbound">>, Call) ->
    Call;
set_other_id(<<"outbound">>, #call{caller_id_name=CIDName
                                  ,caller_id_number=CIDNumber
                                 }=Call) when (CIDName =:= 'undefined') or (CIDNumber =:= 'undefined') ->
    Updaters = [fun(Call2) -> set_caller_id_name(amimulator_call:from_user(Call2), Call2) end
                ,fun(Call2) -> set_caller_id_number(amimulator_call:from_user(Call2), Call2) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters);
set_other_id(<<"outbound">>, Call) ->
    Call.

-spec endpoint_caller_id(kz_json:object(), call()) -> {api_binary(), api_binary()} | 'undefined'.
endpoint_caller_id(Endpoint, Call) ->
    case kz_json:get_value(<<"owner_id">>, Endpoint) of
        'undefined' -> 'undefined';%{kz_json:get_value(<<"name">>, Endpoint), }
        OwnerId ->
            {'ok', Owner} = kz_datamgr:open_doc(account_db(Call), OwnerId),
            {<<(kz_json:get_value(<<"username">>, Owner))/binary, " ",
               (kz_json:get_value(<<"first_name">>, Owner))/binary, " ",
               (kz_json:get_value(<<"last_name">>, Owner))/binary>>
             ,<<(kz_json:get_value(<<"username">>, Owner))/binary>>}
    end.



-spec parse_user_at_realm(atom(), ne_binary()) -> ne_binary().
parse_user_at_realm('user', Data) ->
    case binary:split(Data, <<"@">>) of
        [ToUser, _ToRealm] ->
            ToUser;
        [ToUser] ->
            ToUser
    end.

-spec maybe_cellphone_endpoint(call()) -> api_object().
maybe_cellphone_endpoint(Call) ->
    AccountDb = account_db(Call),
    E164 = case direction(Call) of
        <<"inbound">> -> wnm_util:to_e164(from_user(Call));
        <<"outbound">> -> wnm_util:to_e164(to_user(Call))
    end,

    Results1 = case kz_datamgr:get_results(AccountDb, <<"devices/call_forwards">>) of
        {'ok', Results} -> Results;
        {'error', _} -> []
    end,
    find_cellphone_endpoint_fold(AccountDb, E164, Results1).

-spec find_cellphone_endpoint_fold(api_binary(), ne_binary(), kz_json:objects()) -> api_object().
find_cellphone_endpoint_fold(_, _, []) ->
    'undefined';
find_cellphone_endpoint_fold(AccountDb, E164, [Result|Results]) ->
    find_cellphone_endpoint_fold(AccountDb, E164, kz_datamgr:open_doc(AccountDb, kz_json:get_value(<<"id">>, Result)), Results).

-spec find_cellphone_endpoint_fold(api_binary(), ne_binary(), {'ok', kz_json:object()} | kz_datamgr:couchbeam_error() | {'error', 'not_found'}, kz_json:objects()) -> api_object().
find_cellphone_endpoint_fold(AccountDb, E164, {'error', 'req_timedout'}, Results) ->
    find_cellphone_endpoint_fold(AccountDb, E164, Results);
find_cellphone_endpoint_fold(AccountDb, E164, {'ok', Device}, Results) ->
    case {wnm_util:to_e164(kz_json:get_value([<<"call_forward">>, <<"number">>], Device)), kz_json:get_value(<<"owner_id">>, Device)} of
        {_, 'undefined'} -> find_cellphone_endpoint_fold(AccountDb, E164, Results);
        {E164, _} -> Device;
        _ -> find_cellphone_endpoint_fold(AccountDb, E164, Results)
    end.

-spec maybe_endpoint_owner(api_object()) -> api_object().
maybe_endpoint_owner('undefined') ->
    'undefined';
maybe_endpoint_owner(Endpoint) ->
    case kz_json:get_value(<<"owner_id">>, Endpoint) of
        'undefined' -> 'undefined';
        OwnerId ->
            case kz_datamgr:open_doc(kz_json:get_value(<<"account_db">>, Endpoint), OwnerId) of
                {'ok', UserDoc} -> UserDoc;
                _ -> 'undefined'
            end
    end.
