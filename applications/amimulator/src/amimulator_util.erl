%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(amimulator_util).

-include("amimulator.hrl").

-export([parse_payload/1
        ,format_prop/1
        ,format_binary/1
        ,format_json_events/1

        ,parse_variable/1

        ,index_of/2
        ,create_call/1
        ,clear_call/1

        ,initial_calls/1
                                                % ,initial_calls2/1
        ,update_initial_call/1
        ,fork_agent_call_leg1/2
        ,fork_agent_call_leg2/2
        ,channel_string/1
        ,endpoint_exten/1
        ,queue_number/2

        ,update_originate_props/1
        ,kapps_call_from_ami_originate_props/1

        ,find_id_number/2, queue_for_number/2]).

%% AMI commands broken up by newlines
-spec parse_payload(binary()) -> list().
parse_payload(Payload) ->
    Lines = filter_empty(binary:split(Payload, <<"\r\n">>, ['global'])),
    lists:foldl(fun(Parameter, Acc) ->
                        KV = binary:split(Parameter, <<":">>),
                        {K, V} = {lists:nth(1, KV), lists:nth(2, KV)},
                        Prop = {K, binary:replace(V, <<" ">>, <<>>)},
                        [Prop] ++ Acc
                end, [], Lines).

%% Eliminates trailing lines from client payload
-spec filter_empty(list()) -> list().
filter_empty(Parameters) ->
    lists:foldl(fun(Param, Acc) ->
                        case Param of
                            <<>> ->
                                Acc;
                            _ ->
                                [Param] ++ Acc
                        end end, [], Parameters).

%% Recursive proplist formatting for writes to socket
-spec format_prop(tuple()) -> binary().
format_prop({V}) ->
    <<(kz_term:to_binary(V))/binary, "\r\n">>;
format_prop({K, V}) ->
    <<(kz_term:to_binary(K))/binary, ": ", (kz_term:to_binary(V))/binary, "\r\n">>.

-spec format_binary(list()) -> binary().
format_binary([KV|Rest]) ->
    Head = format_prop(KV),
    Tail = format_binary(Rest),
    <<Head/binary, Tail/binary>>;
format_binary([]) ->
    <<"\r\n">>.

%% Format a set of events for publishing to AMQP

-spec format_json_events(list()) -> list().
format_json_events(Events) ->
    format_json_events(Events, []).

-spec format_json_events(list(), list()) -> list().
format_json_events([], Acc) ->
    Acc;
format_json_events([{_K, _V}|_Other]=KVs, _Acc) ->
    [{KVs}];
format_json_events([Event|Events], Acc) ->
    format_json_events(Events, Acc ++ [{Event}]).

%%------------------------------------------------------------------------------
%% @doc Parse AMI variable assignments of the format Key=Value into a
%% pair.
%% @end
%%------------------------------------------------------------------------------
-spec parse_variable(kz_term:ne_binary()) -> {binary(), binary()}.
parse_variable(VarString) ->
    case binary:split(VarString, <<"=">>) of
        [K, V] -> {K, V};
        [K] -> {K, <<>>}
    end.

%% Find the index of an element in a list

-spec index_of(any(), list()) -> pos_integer() | 'not_found'.
index_of(Element, List) ->
    index_of(Element, List, 1).

-spec index_of(any(), list(), pos_integer()) -> pos_integer() | 'not_found'.
index_of(_, [], _) ->
    'not_found';
index_of(Element, [Element|_], Index) ->
    Index;
index_of(Element, [_|T], Index) ->
    index_of(Element, T, Index+1).

-spec create_call(kz_json:object()) -> amimulator_call:call().
create_call(EventJObj) ->
    Call = amimulator_call:from_json(EventJObj),
    UpdatedCall = amimulator_call:update_from_other(ami_sm:call(amimulator_call:other_leg_call_id(Call)), Call),
    lager:debug("call ~p", [UpdatedCall]),
    UpdatedCall.

-spec clear_call(kz_term:ne_binary()) -> boolean().
clear_call(CallId) ->
    case ami_sm:call(CallId) of
        'undefined' -> 'not_found';
        Call ->
            Payload = [[
                        {<<"Event">>, <<"Hangup">>},
                        {<<"Privilege">>, <<"call,all">>},
                        {<<"Channel">>, amimulator_call:channel(Call)},
                        {<<"Uniqueid">>, CallId},
                        {<<"CallerIDNum">>, <<>>},
                        {<<"CallerIDName">>, <<>>},
                        {<<"ConnectedLineNum">>, <<>>},
                        {<<"ConnectedLineName">>, <<>>},
                        {<<"Cause">>, 16},
                        {<<"Cause-txt">>, <<"Normal Clearing">>}
                       ]],

            amimulator_event_listener:publish_amqp_event({'publish', Payload}, amimulator_call:account_id(Call)),

            ami_sm:delete_call(CallId)
    end.

%% Fetches all active calls for an account
-spec initial_calls(kz_term:ne_binary()) -> [amimulator_call:call(),...] | [].
initial_calls(AccountId) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Active-Only">>, 'true'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_call:publish_query_account_channels_req/1
                                    ,{'ecallmgr', fun kapi_call:query_account_channels_resp_v/1}
                                    ) of
        {'ok', RespJObjs} ->
            %% Now we can produce all the channels and update the state master
            BasicCalls = lists:foldl(fun(RespJObj, Calls) ->
                                             Channels = kz_json:get_value(<<"Channels">>, RespJObj),
                                             case Channels of
                                                 'undefined' -> Calls;
                                                 _ ->
                                                     lists:foldl(fun(Call, Calls2) ->
                                                                         [update_initial_call(Call) | Calls2]
                                                                 end, Calls, amimulator_call:from_json(Channels))
                                             end
                                     end, [], RespJObjs),

            PrioritySortedBasicCalls = lists:sort(fun(A, _) ->
                                                          case amimulator_call:acdc_queue_id(A) of
                                                              'undefined' -> 'false';
                                                              _ -> 'true'
                                                          end
                                                  end, BasicCalls),

            process_basic_calls(PrioritySortedBasicCalls, PrioritySortedBasicCalls, []);
        E ->
            lager:error("could not get active channels: ~p", [E]),
            []
    end.

                                                % -spec
process_basic_calls([], _, Calls) ->
    Calls;
process_basic_calls([BasicCall|BasicCalls], AllBasicCalls, Calls) ->
    case amimulator_call:acdc_queue_id(BasicCall) of
        'undefined' -> process_regular_basic_call(BasicCall, BasicCalls, AllBasicCalls, Calls);
        _QueueId -> process_queue_basic_call(BasicCall, BasicCalls, AllBasicCalls, Calls)
    end.

                                                % -spec
process_regular_basic_call(BasicCall, BasicCalls, AllBasicCalls, Calls) ->
    case lists:keyfind(amimulator_call:other_leg_call_id(BasicCall), 2, AllBasicCalls) of
        'false' -> process_basic_calls(BasicCalls, AllBasicCalls, [BasicCall | Calls]);
        OtherCall -> process_basic_calls(BasicCalls, AllBasicCalls, [amimulator_call:update_from_other(OtherCall, BasicCall) | Calls])
    end.

                                                % -spec
process_queue_basic_call(BasicCall, BasicCalls, AllBasicCalls, Calls) ->
    AgentCallIds = find_agent_call_ids(BasicCall),
    {NewCalls, RemainingBasicCalls2} = lists:foldl(fun(AgentCallId, {LocalAgentCalls, RemainingBasicCalls}) ->
                                                           SipAgentCall = lists:keyfind(AgentCallId, 2, BasicCalls),

                                                           case SipAgentCall of
                                                               'false' -> {LocalAgentCalls, RemainingBasicCalls};
                                                               _ ->
                                                                   SipAgentCall2 = amimulator_call:set_acdc_queue_id(amimulator_call:acdc_queue_id(BasicCall), SipAgentCall),
                                                                   LocalCall1 = fork_agent_call_leg1(SipAgentCall2, BasicCall),
                                                                   LocalCall2 = fork_agent_call_leg2(SipAgentCall2, BasicCall),

                                                                   case amimulator_call:answered(SipAgentCall2) of
                                                                       'true' ->
                                                                           MemberCall2 = amimulator_call:set_other_leg_call_id(amimulator_call:call_id(LocalCall1), BasicCall),
                                                                           MemberCall3 = amimulator_call:set_other_channel(amimulator_call:channel(LocalCall1), MemberCall2),

                                                                           Call2 = amimulator_call:set_other_leg_call_id(amimulator_call:call_id(LocalCall2), SipAgentCall2),
                                                                           Call3 = amimulator_call:set_other_channel(amimulator_call:channel(LocalCall2), Call2),

                                                                           {[MemberCall3, Call3, LocalCall1, LocalCall2 | LocalAgentCalls], lists:keydelete(AgentCallId, 2, BasicCalls)};
                                                                       _ ->
                                                                           MemberCall2 = amimulator_call:set_other_leg_call_id('undefined', BasicCall),
                                                                           MemberCall3 = amimulator_call:set_other_channel('undefined', MemberCall2),

                                                                           Call2 = amimulator_call:set_other_leg_call_id('undefined', SipAgentCall2),
                                                                           Call3 = amimulator_call:set_other_channel('undefined', Call2),

                                                                           {[MemberCall3, Call3, LocalCall1, LocalCall2 | LocalAgentCalls], lists:keydelete(AgentCallId, 2, BasicCalls)}
                                                                   end
                                                           end
                                                   end, {[], BasicCalls}, AgentCallIds),
    process_basic_calls(RemainingBasicCalls2, AllBasicCalls, NewCalls ++ Calls).

-spec update_initial_call(amimulator_call:call()) -> amimulator_call:call().
update_initial_call(Call) ->
    Updaters = [fun maybe_queue_call/1
               ,fun maybe_conf_call/1
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters).

-spec maybe_queue_call(amimulator_call:call()) -> amimulator_call:call().
maybe_queue_call(Call) ->
    %% TODO change to AMQP request
    maybe_queue_call(acdc_stats:find_call(amimulator_call:call_id(Call)), Call).

-spec maybe_queue_call(kz_term:api_object(), amimulator_call:call()) -> amimulator_call:call().
maybe_queue_call('undefined', Call) ->
    Call;
maybe_queue_call(Stat, Call) ->
    QueueId = kz_json:get_value(<<"Queue-ID">>, Stat),
    amimulator_call:set_acdc_queue_id(QueueId, Call).

%% TODO implement
maybe_conf_call(Call) ->
    Call.

                                                % -spec
find_agent_call_ids(Call) ->
    Req = [{<<"Call-ID">>, amimulator_call:call_id(Call)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_acdc_agent:publish_stats_req/1
                                    ,500
                                    ) of
        {'ok', RespJObjs} ->
            lists:foldl(fun(Resp, Acc) ->
                                case kz_json:get_value(<<"Agent-Call-IDs">>, Resp) of
                                    'undefined' -> Acc;
                                    AgentCallIds -> AgentCallIds ++ Acc
                                end
                        end, [], RespJObjs);
        {'error', E} ->
            lager:debug("could not find agent call ids (~p)", [E]),
            [];
        {'timeout', RespJObjs} ->
            lists:foldl(fun(Resp, Acc) ->
                                case kz_json:get_value(<<"Agent-Call-IDs">>, Resp) of
                                    'undefined' -> Acc;
                                    AgentCallIds -> AgentCallIds ++ Acc
                                end
                        end, [], RespJObjs)
    end.

-spec fork_agent_call_leg1(amimulator_call:call(), amimulator_call:call()) ->
          amimulator_call:call().
fork_agent_call_leg1(SipAgentCall, Call) ->
    fork_agent_call_leg1(amimulator_call:answered(SipAgentCall), SipAgentCall, Call).

fork_agent_call_leg1('false', SipAgentCall, Call) ->
    Updaters = [fun(LC) -> amimulator_call:set_call_id(<<(amimulator_call:call_id(SipAgentCall))/binary, "-queue;1">>, LC) end
               ,fun(LC) -> amimulator_call:set_other_leg_call_id('undefined', LC) end
               ,fun(LC) -> amimulator_call:set_channel(channel_string('local', SipAgentCall, 1), LC) end
               ,fun(LC) -> amimulator_call:set_other_channel('undefined', LC) end
               ,fun(LC) -> amimulator_call:set_answered('false', LC) end
                                                % ,fun(LC) -> amimulator_call:set_direction(<<"outbound">>, LC) end
               ,fun(LC) -> amimulator_call:set_caller_id_name(amimulator_call:id_name(Call), LC) end
               ,fun(LC) -> amimulator_call:set_caller_id_number(amimulator_call:id_number(Call), LC) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, SipAgentCall, Updaters);
fork_agent_call_leg1(_, SipAgentCall, Call) ->
    Updaters = [fun(LC) -> amimulator_call:set_call_id(<<(amimulator_call:call_id(SipAgentCall))/binary, "-queue;1">>, LC) end
               ,fun(LC) -> amimulator_call:set_other_leg_call_id(amimulator_call:call_id(Call), LC) end
               ,fun(LC) -> amimulator_call:set_channel(channel_string('local', SipAgentCall, 1), LC) end
               ,fun(LC) -> amimulator_call:set_other_channel(amimulator_call:channel(Call), LC) end
               ,fun(LC) -> amimulator_call:set_answered('true', LC) end
                                                % ,fun(LC) -> amimulator_call:set_direction(<<"outbound">>, LC) end
               ,fun(LC) -> amimulator_call:set_caller_id_name(amimulator_call:id_name(Call), LC) end
               ,fun(LC) -> amimulator_call:set_caller_id_number(amimulator_call:id_number(Call), LC) end
               ,fun(LC) -> amimulator_call:set_other_leg_call_id(amimulator_call:call_id(Call), LC) end
               ,fun(LC) -> amimulator_call:set_other_channel(amimulator_call:channel(Call), LC) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, SipAgentCall, Updaters).

-spec fork_agent_call_leg2(amimulator_call:call(), amimulator_call:call()) ->
          amimulator_call:call().
fork_agent_call_leg2(SipAgentCall, Call) ->
    fork_agent_call_leg2(amimulator_call:answered(SipAgentCall), SipAgentCall, Call).

fork_agent_call_leg2('false', SipAgentCall, Call) ->
    Updaters = [fun(LC) -> amimulator_call:set_call_id(<<(amimulator_call:call_id(SipAgentCall))/binary, "-queue;2">>, LC) end
               ,fun(LC) -> amimulator_call:set_other_leg_call_id('undefined', LC) end
               ,fun(LC) -> amimulator_call:set_channel(channel_string('local', SipAgentCall, 2), LC) end
               ,fun(LC) -> amimulator_call:set_other_channel('undefined', LC) end
               ,fun(LC) -> amimulator_call:set_answered('false', LC) end
                                                % ,fun(LC) -> amimulator_call:set_direction(<<"outbound">>, LC) end
               ,fun(LC) -> amimulator_call:set_callee_id_name(amimulator_call:id_name(SipAgentCall), LC) end
               ,fun(LC) -> amimulator_call:set_callee_id_number(amimulator_call:id_number(SipAgentCall), LC) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters);
fork_agent_call_leg2(_, SipAgentCall, Call) ->
    Updaters = [fun(LC) -> amimulator_call:set_call_id(<<(amimulator_call:call_id(SipAgentCall))/binary, "-queue;2">>, LC) end
               ,fun(LC) -> amimulator_call:set_other_leg_call_id(amimulator_call:call_id(SipAgentCall), LC) end
               ,fun(LC) -> amimulator_call:set_channel(channel_string('local', SipAgentCall, 2), LC) end
               ,fun(LC) -> amimulator_call:set_other_channel(amimulator_call:channel(SipAgentCall), LC) end
               ,fun(LC) -> amimulator_call:set_answered('true', LC) end
                                                % ,fun(LC) -> amimulator_call:set_direction(<<"outbound">>, LC) end
               ,fun(LC) -> amimulator_call:set_callee_id_name(amimulator_call:id_name(SipAgentCall), LC) end
               ,fun(LC) -> amimulator_call:set_callee_id_number(amimulator_call:id_number(SipAgentCall), LC) end
               ],
    lists:foldl(fun(Updater, Call2) -> Updater(Call2) end, Call, Updaters).

-spec channel_string(amimulator_call:call()) -> binary().
channel_string(Call) ->
    channel_string('sip', amimulator_call:id_number(Call), amimulator_call:call_id(Call), amimulator_call:direction(Call)).

-spec channel_string('local', amimulator_call:call(), pos_integer()) -> binary().
channel_string('local', Call, Index) ->
    channel_string('local', amimulator_call:id_number(Call), amimulator_call:call_id(Call), Index).

-spec channel_string('sip' | 'local', kz_term:api_binary(), kz_term:api_binary(), pos_integer()) -> binary().
channel_string('sip', Number, CallId, _) ->
    <<"SIP/", Number/binary, "-", (channel_tail(CallId))/binary>>;
channel_string('local', Number, CallId, Index) ->
    <<"Local/", Number/binary, "@from-queue-", (channel_tail(Index, CallId))/binary>>.

%% Returns an 8-digit tail for channels for AMI calls

-spec channel_tail(kz_term:api_binary()) -> binary().
channel_tail(CallId) ->
    Seed = case binary:split(CallId, <<"-">>, ['global']) of
               List when length(List) =:= 5 ->
                   %% When the call id looks like 4cad762c-f415-11e4-b890-cdee54d38ecb there may be many legs created
                   %% The 2nd and 3rd parts are unique to a call as a whole even though the 1st and 5th change per leg
                   <<(lists:nth(2, List))/binary, "-", (lists:nth(3, List))/binary>>;
               _ ->
                   CallId
           end,
    Digest = crypto:hash('md5', kz_term:to_binary(Seed)),
    MD5 = lists:flatten([io_lib:format("~2.16.0b", [Part]) || <<Part>> <= Digest]),
    list_to_binary(lists:sublist(MD5, length(MD5)-7, 8)).

-spec channel_tail(kz_term:api_binary(), kz_term:api_binary()) -> binary().
channel_tail(Index, CallId) ->
    Seed = case binary:split(CallId, <<"-">>, ['global']) of
               List when length(List) =:= 5 ->
                   %% When the call id looks like 4cad762c-f415-11e4-b890-cdee54d38ecb-queue there may be many legs created
                   %% The 2nd and 3rd parts are unique to a call as a whole even though the 1st and 5th change per leg
                   <<(lists:nth(2, List))/binary, "-", (lists:nth(3, List))/binary>>;
               _ ->
                   CallId
           end,
    Digest = crypto:hash('md5', kz_term:to_binary(Seed)),
    MD5 = lists:flatten([io_lib:format("~2.16.0b", [Part]) || <<Part>> <= Digest]),
    list_to_binary(lists:sublist(MD5, length(MD5)-3, 4) ++ ";" ++ kz_term:to_list(Index)).

-spec endpoint_exten(kz_json:object()) -> kz_term:api_binary().
endpoint_exten(Endpoint) ->
    endpoint_exten(kz_json:get_value(<<"owner_id">>, Endpoint), Endpoint).

-spec endpoint_exten(kz_term:api_binary(), kz_json:object()) -> kz_term:api_binary().
endpoint_exten('undefined', Endpoint) ->
    case kz_json:get_value(<<"name">>, Endpoint) of
        'undefined' ->
            Server = hd(kz_json:get_value(<<"servers">>, Endpoint)),
            kz_json:get_value([<<"auth">>, <<"auth_user">>], Server);
        Name -> Name
    end;
endpoint_exten(OwnerId, Endpoint) ->
    case kz_datamgr:open_doc(kz_json:get_value(<<"pvt_account_db">>, Endpoint), OwnerId) of
        {'ok', UserDoc} -> <<(kz_json:get_value(<<"username">>, UserDoc))/binary>>;
        _ ->
            lager:debug("could not find owner doc for device ~p (~p)", [kz_json:get_value(<<"_id">>, Endpoint)
                                                                       ,kz_json:get_value(<<"name">>, Endpoint)]),
            endpoint_exten('undefined', Endpoint)
    end.

-spec queue_number(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:json_term() | 'undefined'.
queue_number(AccountDb, QueueId) ->
    case kz_datamgr:get_results(AccountDb, <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
        {'error', E} ->
            lager:debug("could not load queue callflows (~p)", [E]),
            'undefined';
        {'ok', []} ->
            lager:debug("the queue does not have a callflow specified"),
            'undefined';
        {ok, Results} ->
            Value = kz_json:get_value(<<"value">>, hd(Results)),
            hd(Value)
    end.

                                                % initial_calls2(AccountId) ->
                                                %       Req = [
                                                %         {<<"Account-ID">>, AccountId},
                                                %         {<<"Active-Only">>, 'true'}
                                                %         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                                %     ],
                                                %     case kz_amqp_worker:call_collect(
                                                %         Req,
                                                %         fun kapi_call:publish_query_account_channels_req/1,
                                                %         {'ecallmgr', fun kapi_call:query_account_channels_resp_v/1}
                                                %     ) of
                                                %         {'ok', RespJObjs} ->
                                                %               %% Now we can produce all the channels and update the state master
                                                %             LookupChannels = lists:foldl(fun(RespJObj, ChannelsAcc) ->
                                                %                 Channels = kz_json:get_value(<<"Channels">>, RespJObj),
                                                %                 case Channels of
                                                %                     undefined ->
                                                %                       ChannelsAcc;
                                                %                     _ ->
                                                %                       NewChannels = lists:foldl(fun(Channel, Acc) ->
                                                %                               CallId = kz_json:get_value(<<"uuid">>, Channel),
                                                %                               [{CallId, Channel} | Acc]
                                                %                       end, ChannelsAcc, Channels),
                                                %                       NewChannels
                                                %                 end
                                                %             end, [], RespJObjs),

                                                %             lists:foldl(fun({_CallId, Channel}, Acc) ->
                                                %               [call_from_channel(Channel, LookupChannels) | Acc]
                                                %             end, [], LookupChannels);
                                                %         E ->
                                                %             lager:debug("Could not get channel statuses: ~p", [E])
                                                %     end.

                                                % call_from_channel(JObj, Lookup) ->
                                                %       Routines = [
                                                %               fun({Call, WhappsCall}) ->
                                                %                       {Call, kapps_call:set_call_id(kz_json:get_value(<<"uuid">>, JObj), WhappsCall)} end,
                                                %               fun({Call, WhappsCall}) ->
                                                %                       {Call, kapps_call:set_other_leg_call_id(kz_json:get_value(<<"other_leg">>, JObj), WhappsCall)} end,
                                                %               fun({Call, WhappsCall}) ->
                                                %                       {Call, kapps_call:set_bridge_id(kz_json:get_value(<<"bridge_id">>, JObj), WhappsCall)} end,
                                                %               fun({Call, WhappsCall}) ->
                                                %                       {Call, kapps_call:set_account_id(kz_json:get_value(<<"account_id">>, JObj), WhappsCall)} end,
                                                %               fun({Call, WhappsCall}) ->
                                                %                       {Call, kapps_call:set_authorizing_id(kz_json:get_value(<<"authorizing_id">>, JObj, <<>>), WhappsCall)} end,
                                                %               fun({Call, WhappsCall}) ->
                                                %                       {Call, kapps_call:set_authorizing_type(kz_json:get_value(<<"authorizing_type">>, JObj, <<>>), WhappsCall)} end,
                                                %               fun({Call, WhappsCall}) ->
                                                %                       {Call, kapps_call:set_to_user(kz_json:get_value(<<"destination">>, JObj), WhappsCall)} end,

                                                %               fun({Call, WhappsCall}) ->
                                                %                       {props:set_value(<<"direction">>, kz_json:get_value(<<"direction">>, JObj), Call), WhappsCall} end,
                                                %         fun({Call, WhappsCall}) ->
                                                %               {props:set_value(<<"username">>, kz_json:get_value(<<"username">>, JObj), Call), WhappsCall} end,
                                                %         fun({Call, WhappsCall}) ->
                                                %               {props:set_value(<<"answered">>, kz_json:get_value(<<"answered">>, JObj), Call), WhappsCall} end,
                                                %         fun({Call, WhappsCall}) ->
                                                %               {props:set_value(<<"elapsed_s">>, kz_json:get_value(<<"elapsed_s">>, JObj), Call), WhappsCall} end,
                                                %               fun({Call, WhappsCall}) ->
                                                %                       CallId = kz_json:get_value(<<"uuid">>, JObj),
                                                %                       AccountDb = kapps_call:account_db(WhappsCall),
                                                %                       Props = case kapps_call:authorizing_id(WhappsCall) of
                                                %                               <<>> ->
                                                %                                       ALeg = case maybe_cellphone_endpoint2(
                                                %                                               kapps_call:to_user(WhappsCall), props:get_value(<<"direction">>, Call),
                                                %                                               CallId, kz_json:get_value(<<"presence_id">>, JObj), AccountDb) of
                                                %                                               {direction, D} ->
                                                %                                                       [
                                                %                                                               {<<"aleg_cid">>, props:get_value(<<"cid">>, D)},
                                                %                                                               {<<"aleg_exten">>, props:get_value(<<"cid">>, D)},
                                                %                                                               {<<"aleg_ami_channel">>, props:get_value(<<"channel">>, D)}
                                                %                                                       ];
                                                %                                               {endpoint, Endpoint} ->
                                                %                                                       [
                                                %                                                               {<<"aleg_cid">>, endpoint_cid(Endpoint, AccountDb)},
                                                %                                                               {<<"aleg_exten">>, endpoint_exten(Endpoint, AccountDb)},
                                                %                                                               {<<"aleg_ami_channel">>, endpoint_channel(Endpoint, AccountDb, CallId)}
                                                %                                                       ]
                                                %                                       end,
                                                %                                       BLeg = case kz_datamgr:open_doc(AccountDb,
                                                %                                               kz_json:get_value(<<"authorizing_id">>,
                                                %                                                       props:get_value(kapps_call:other_leg_call_id(WhappsCall), Lookup))) of
                                                %                                               {error, empty_doc_id} ->
                                                %                                                       [];
                                                %                                               {ok, Endpoint2} ->
                                                %                                                       [
                                                %                                                               {<<"bleg_cid">>, endpoint_cid(Endpoint2, AccountDb)},
                                                %                                                               {<<"bleg_exten">>, endpoint_exten(Endpoint2, AccountDb)},
                                                %                                                               {<<"bleg_ami_channel">>, endpoint_channel(Endpoint2, AccountDb, CallId)}
                                                %                                                       ]
                                                %                                       end,
                                                %                                       ALeg ++ BLeg;
                                                %                               _ ->
                                                %                                       Endpoint = case kz_endpoint:get(WhappsCall) of
                                                %                                               {error, E} ->
                                                %                                                       lager:debug("Error when getting endpoint: ~p", [WhappsCall]),
                                                %                                                       undefined;
                                                %                                               {ok, Endpoint2} ->
                                                %                                                       Endpoint2
                                                %                                       end,
                                                %                                       ALeg = [
                                                %                                               {<<"aleg_cid">>, endpoint_cid(Endpoint, AccountDb)},
                                                %                                               {<<"aleg_exten">>, endpoint_exten(Endpoint, AccountDb)},
                                                %                                               {<<"aleg_ami_channel">>, endpoint_channel(Endpoint, AccountDb, CallId)}
                                                %                                       ],
                                                %                                       OtherLegCallId = kapps_call:other_leg_call_id(WhappsCall),
                                                %                                       OtherChannel = props:get_value(OtherLegCallId, Lookup),
                                                %                                       BLeg = case maybe_cellphone_endpoint2(
                                                %                                               kz_json:get_value(<<"destination">>, OtherChannel), kz_json:get_value(<<"direction">>, OtherChannel),
                                                %                                               CallId, kz_json:get_value(<<"presence_id">>, OtherChannel), AccountDb) of
                                                %                                               {direction, D} ->
                                                %                                                       [
                                                %                                                               {<<"bleg_cid">>, props:get_value(<<"cid">>, D)},
                                                %                                                               {<<"bleg_exten">>, props:get_value(<<"cid">>, D)},
                                                %                                                               {<<"bleg_ami_channel">>, props:get_value(<<"channel">>, D)}
                                                %                                                       ];
                                                %                                               {endpoint, Endpoint} ->
                                                %                                                       [
                                                %                                                               {<<"bleg_cid">>, endpoint_cid(Endpoint, AccountDb)},
                                                %                                                               {<<"bleg_exten">>, endpoint_exten(Endpoint, AccountDb)},
                                                %                                                               {<<"bleg_ami_channel">>, endpoint_channel(Endpoint, AccountDb, OtherLegCallId)}
                                                %                                                       ]
                                                %                                       end,
                                                %                                       ALeg ++ BLeg
                                                %                       end,
                                                %                       {props:set_values(Props, Call), WhappsCall}
                                                %               end
                                                %       ],
                                                %     {Call, WhappsCall} = lists:foldl(fun(F, {Call, WhappsCall}) -> F({Call, WhappsCall}) end, {[], kapps_call:new()}, Routines),
                                                %     props:set_value(<<"call">>, WhappsCall, Call).

                                                % maybe_cellphone_endpoint2(To, Direction, CallId, PresenceId, AccountDb) ->
                                                %     {ok, Results} = kz_datamgr:get_results(AccountDb, <<"devices/call_forwards">>),

                                                %     Number = case Direction of
                                                %       <<"inbound">> ->
                                                %               hd(binary:split(PresenceId, <<"@">>));
                                                %       <<"outbound">> ->
                                                %               To
                                                %     end,

                                                %     E164 = wnm_util:to_e164(Number),
                                                %     case find_call_forward(E164, AccountDb, Results) of
                                                %       false ->
                                                %               {direction, call_direction_endpoint2(To, Direction, CallId, PresenceId)};
                                                %       Endpoint ->
                                                %               {endpoint, Endpoint}
                                                %     end.





                                                % find_call_forward(_, _, []) ->
                                                %       false;
                                                % find_call_forward(E164, AccountDb, [Result|Others]) ->
                                                %       case wnm_util:to_e164(kz_json:get_value(<<"key">>, Result)) of
                                                %               E164 ->
                                                %                       Value = kz_json:get_value(<<"value">>, Result),
                                                %                       {ok, Device} = kz_datamgr:open_doc(AccountDb, kz_json:get_value(<<"id">>, Value)),
                                                %                       Device;
                                                %               _ ->
                                                %                       find_call_forward(E164, AccountDb, Others)
                                                %       end.




                                                % call_direction_endpoint2(To, Direction, CallId, PresenceId) ->
                                                %     case Direction of
                                                %         <<"inbound">> ->
                                                %             [
                                                %               {<<"channel">>, channel_string(hd(binary:split(PresenceId, <<"@">>)), CallId)},
                                                %                   {<<"cid">>, hd(binary:split(PresenceId, <<"@">>))}
                                                %             ];
                                                %         <<"outbound">> ->
                                                %             [
                                                %               {<<"channel">>, channel_string(To, CallId)},
                                                %               {<<"cid">>, To}
                                                %             ]
                                                %     end.%,
                                                %    case props:get_value(<<"cid">>, Props) of
                                                %      <<"Unknown">> ->
                                                %              call_direction_cid(Call, WhappsCall, Props);
                                                %      <<"Device QuickCall">> ->
                                                %              call_direction_cid(Call, WhappsCall, Props);
                                                %       _ ->
                                                %               Props
                                                % end.






















                                                % better_call(JObj, BasicCalls) ->
                                                %     CallId = kz_json:get_first_defined([<<"uuid">>, <<"Call-ID">>], JObj),
                                                %     Call = props:get_value(CallId, BasicCalls),

                                                %     Routines = [
                                                %         % fun(Call2, JObj2, _BC) ->
                                                %         %     CallDirection = kz_json:get_first_defined([<<"direction">>, <<"Call-Direction">>], JObj2,
                                                %         %         <<"inbound">>),
                                                %         %     props:set_value(<<"direction">>, CallDirection, Call2) end,
                                                %         % fun aleg_cid/3,
                                                %         % fun aleg_exten/3,
                                                %         % fun aleg_ami_channel/3,
                                                %         fun bleg_cid/3,
                                                %         fun bleg_exten/3,
                                                %         fun bleg_ami_channel/3,
                                                %         % fun(Call2, JObj2, _BC) -> props:set_value(<<"username">>,
                                                %         %     kz_json:get_first_defined([<<"username">>, <<"Username">>], JObj2), Call2) end,
                                                %         % fun(Call2, JObj2, _BC) -> props:set_value(<<"answered">>,
                                                %         %     kz_json:get_first_defined([<<"answered">>], JObj2), Call2) end,
                                                %         % fun(Call2, JObj2, _BC) -> props:set_value(<<"elapsed_s">>,
                                                %         %     kz_json:get_first_defined([<<"elapsed_s">>], JObj2), Call2) end
                                                %     ],
                                                %     lists:foldl(
                                                %         fun(F, BCall) -> F(BCall, JObj, BasicCalls) end,
                                                %         [{<<"call">>, Call}],
                                                %         Routines
                                                %     ).

                                                % aleg_cid(Call, _ChannelJObj, _BC) ->
                                                %     WhappsCall = props:get_value(<<"call">>, Call),
                                                %     case WhappsCall of
                                                %         undefined ->
                                                %             try throw(42) catch 42 -> kz_util:log_stacktrace() end;
                                                %         _ ->
                                                %             ok
                                                %     end,
                                                %     case kz_endpoint:get(WhappsCall) of
                                                %         %% An external endpoint
                                                %         {error, _E} ->
                                                %               props:set_value(<<"aleg_cid">>, props:get_value(<<"cid">>, maybe_cellphone_endpoint(Call)), Call);
                                                %         %% Some internal extension
                                                %         {ok, Endpoint} ->
                                                %             props:set_value(<<"aleg_cid">>, endpoint_cid(Endpoint, kapps_call:account_db(WhappsCall)), Call)
                                                %     end.

                                                % aleg_exten(Call, _ChannelJObj, _BC) ->
                                                %     WhappsCall = props:get_value(<<"call">>, Call),
                                                %     case kz_endpoint:get(WhappsCall) of
                                                %         %% An external endpoint
                                                %         {error, _E} ->
                                                %             case props:get_value(<<"direction">>, Call) of
                                                %                 <<"inbound">> ->
                                                %                     props:set_value(<<"aleg_exten">>, kapps_call:from_user(WhappsCall), Call);
                                                %                 <<"outbound">> ->
                                                %                     props:set_value(<<"aleg_exten">>, kapps_call:to_user(WhappsCall), Call)
                                                %             end;
                                                %         %% Some internal extension
                                                %         {ok, Endpoint} ->
                                                %             props:set_value(<<"aleg_exten">>, endpoint_exten(Endpoint, kapps_call:account_db(WhappsCall)), Call)
                                                %     end.

                                                % aleg_ami_channel(Call, _ChannelJObj, _BC) ->
                                                %     WhappsCall = props:get_value(<<"call">>, Call),
                                                %     case kz_endpoint:get(WhappsCall) of
                                                %         %% An external endpoint
                                                %         {error, _E} ->
                                                %             props:set_value(<<"aleg_ami_channel">>, props:get_value(<<"channel">>, maybe_cellphone_endpoint(Call)), Call);
                                                %         %% Some internal extension
                                                %         {ok, Endpoint} ->
                                                %             props:set_value(<<"aleg_ami_channel">>, endpoint_channel(Endpoint, kapps_call:account_db(WhappsCall), kapps_call:call_id(WhappsCall)), Call)
                                                %     end.

                                                % maybe_cellphone_endpoint(Call) ->
                                                %     WhappsCall = props:get_value(<<"call">>, Call),
                                                %     AccountDb = kz_util:format_account_id(kapps_call:account_id(WhappsCall), encoded),
                                                %     {ok, Results} = kz_datamgr:get_results(AccountDb, <<"devices/call_forwards">>),
                                                %     E164 = wnm_util:to_e164(kapps_call:to_user(WhappsCall)),
                                                %     case lists:foldl(fun(Result, Found) ->
                                                %         case Found of
                                                %             false ->
                                                %                 {ok, Device} = kz_datamgr:open_doc(AccountDb, kz_json:get_value(<<"id">>, Result)),
                                                %                 case {wnm_util:to_e164(kz_json:get_value([<<"call_forward">>, <<"number">>], Device)),
                                                %                     kz_json:get_value(<<"owner_id">>, Device)} of
                                                %                     {_, undefined} ->
                                                %                         false;
                                                %                     {E164, _} ->
                                                %                         [{<<"channel">>, endpoint_channel(Device, AccountDb, kapps_call:call_id(WhappsCall))}
                                                %                          ,{<<"cid">>, endpoint_cid(Device, AccountDb)}
                                                %                         ];
                                                %                     _ ->
                                                %                         false
                                                %                 end;
                                                %             _ ->
                                                %                 Found
                                                %         end
                                                %     end, false, Results) of
                                                %         false ->
                                                %             %if length(Results) > 0 ->
                                                %             %    {ok, Device} = kz_datamgr:open_doc(AccountDb, kz_json:get_value(<<"id">>, hd(Results))),
                                                %             %    props:set_value(<<"aleg_ami_channel">>,
                                                %             %        endpoint_channel(Device, AccountDb, kapps_call:call_id(WhappsCall)), Call);
                                                %             %true ->
                                                %                 call_direction_endpoint(Call);
                                                %             %end;
                                                %         CellphoneEndpoint ->
                                                %             CellphoneEndpoint
                                                %     end.

                                                % call_direction_endpoint(Call) ->
                                                %     WhappsCall = props:get_value(<<"call">>, Call),
                                                %     Props = case props:get_value(<<"direction">>, Call) of
                                                %         <<"inbound">> ->
                                                %             [{<<"channel">>, channel_string(
                                                %                 kapps_call:from_user(WhappsCall),
                                                %                 kapps_call:call_id(WhappsCall)
                                                %              )}
                                                %              ,{<<"cid">>, kapps_call:caller_id_name(WhappsCall)}
                                                %             ];
                                                %         <<"outbound">> ->
                                                %             [{<<"channel">>, channel_string(
                                                %                 kapps_call:to_user(WhappsCall),
                                                %                 kapps_call:call_id(WhappsCall)
                                                %              )}
                                                %              ,{<<"cid">>, kapps_call:callee_id_name(WhappsCall)}
                                                %             ]
                                                %     end,
                                                %     case props:get_value(<<"cid">>, Props) of
                                                %       <<"Unknown">> ->
                                                %               call_direction_cid(Call, WhappsCall, Props);
                                                %       <<"Device QuickCall">> ->
                                                %               call_direction_cid(Call, WhappsCall, Props);
                                                %               _ ->
                                                %                       Props
                                                %       end.

                                                % call_direction_cid(Call, WhappsCall, Props) ->
                                                %       case props:get_value(<<"direction">>, Call) of
                                                %           <<"inbound">> ->
                                                %               props:set_value(<<"cid">>, kapps_call:from_user(WhappsCall), Props);
                                                %           <<"outbound">> ->
                                                %               props:set_value(<<"cid">>, kapps_call:to_user(WhappsCall), Props)
                                                %       end.

                                                % bleg_cid(Call, ChannelJObj, BC) ->
                                                %     case props:get_value(kapps_call:other_leg_call_id(props:get_value(<<"call">>, Call)),
                                                %         BC) of
                                                %         undefined ->
                                                %               % case props:get_value(<<"direction">>, Call) of
                                                %               %       <<"inbound">> ->
                                                %               %               WhappsCall = props:get_value(<<"call">>, Call),
                                                %               %               BLegCid = case kapps_call:callee_id_name(WhappsCall) of
                                                %               %                       <<>> ->
                                                %               %                               kapps_call:to_user(WhappsCall);
                                                %               %                       CalleeId ->
                                                %               %                               CalleeId
                                                %               %               end,
                                                %               %               props:set_value(<<"bleg_cid">>, BLegCid, Call);
                                                %               %       <<"outbound">> ->
                                                %               %               % WhappsCall = props:get_value(<<"call">>, Call),
                                                %               %               % BLegCid = case kapps_call:caller_id_name(WhappsCall) of
                                                %               %               %       <<>> ->
                                                %               %               %               kapps_call:from_user(WhappsCall);
                                                %               %               %       CallerId ->
                                                %               %               %               CallerId
                                                %               %               % end,
                                                %               %               % props:set_value(<<"bleg_cid">>, BLegCid)
                                                %               %               props:set_value(<<"bleg_cid">>, kapps_call:caller_id_name(props:get_value(<<"call">>, Call)), Call)
                                                %               % end;
                                                %             % props:set_value(<<"bleg_cid">>, kapps_call:to_user(props:get_value(<<"call">>, Call)), Call);
                                                %         OtherCall ->
                                                %             Direction = case props:get_value(<<"direction">>, Call) of
                                                %                 <<"inbound">> -> <<"outbound">>;
                                                %                 <<"outbound">> -> <<"inbound">>
                                                %             end,

                                                %             case is_tuple(OtherCall) of
                                                %                 true ->
                                                %                     props:set_value(<<"bleg_cid">>, props:get_value(<<"aleg_cid">>,
                                                %                         aleg_cid([{<<"call">>, OtherCall},
                                                %                         {<<"direction">>, Direction}], undefined, undefined)), Call);
                                                %                 _ ->
                                                %                     props:set_value(<<"bleg_cid">>, props:get_value(<<"aleg_cid">>, OtherCall), Call)
                                                %             end
                                                %     end.

                                                % bleg_exten(Call, _ChannelJObj, BC) ->
                                                %     case props:get_value(kapps_call:other_leg_call_id(props:get_value(<<"call">>, Call)),
                                                %         BC) of
                                                %         undefined ->
                                                %             % props:set_value(<<"bleg_exten">>, kapps_call:to_user(props:get_value(<<"call">>, Call)), Call);
                                                %         OtherCall ->
                                                %             Direction = case props:get_value(<<"direction">>, Call) of
                                                %                 <<"inbound">> -> <<"outbound">>;
                                                %                 <<"outbound">> -> <<"inbound">>
                                                %             end,

                                                %             case is_tuple(OtherCall) of
                                                %                 true ->
                                                %                     props:set_value(<<"bleg_exten">>, props:get_value(<<"aleg_exten">>,
                                                %                     aleg_exten([{<<"call">>, OtherCall},
                                                %                     {<<"direction">>, Direction}], undefined, undefined)), Call);
                                                %                 _ ->
                                                %                     props:set_value(<<"bleg_exten">>, props:get_value(<<"aleg_exten">>, OtherCall), Call)
                                                %             end
                                                %     end.

                                                % bleg_ami_channel(Call, _ChannelJObj, BC) ->
                                                %     case props:get_value(kapps_call:other_leg_call_id(props:get_value(<<"call">>, Call)),
                                                %         BC) of
                                                %         undefined ->
                                                %             %% TODO, find the call somehow
                                                %             % Call;
                                                %         OtherCall ->
                                                %             Direction = case props:get_value(<<"direction">>, Call) of
                                                %                 <<"inbound">> -> <<"outbound">>;
                                                %                 <<"outbound">> -> <<"inbound">>
                                                %             end,

                                                %             case is_tuple(OtherCall) of
                                                %                 true ->
                                                %                     props:set_value(<<"bleg_ami_channel">>, props:get_value(<<"aleg_ami_channel">>,
                                                %                         aleg_ami_channel([{<<"call">>, OtherCall},
                                                %                         {<<"direction">>, Direction}], undefined, undefined)), Call);
                                                %                 _ ->
                                                %                     props:set_value(<<"bleg_ami_channel">>, props:get_value(<<"aleg_ami_channel">>, OtherCall), Call)
                                                %             end
                                                %     end.




-spec find_id_number(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()} | {'error', 'not_found'}.
find_id_number(Id, AccountDb) ->
    {ok, Results} = kz_datamgr:get_results(AccountDb, <<"callflows/crossbar_listing">>),
    maybe_id_in_callflows(Id, Results, AccountDb).

maybe_id_in_callflows(_, [], _) ->
    {error, not_found};
maybe_id_in_callflows(Id, [Result|Results], AccountDb) ->
    CFId = kz_json:get_value(<<"id">>, Result),
    case maybe_id_in_callflow(Id, CFId, AccountDb) of
        false ->
            maybe_id_in_callflows(Id, Results, AccountDb);
        Number ->
            {ok, Number}
    end.

maybe_id_in_callflow(Id, CFId, AccountDb) ->
    {ok, CFDoc} = kz_datamgr:open_doc(AccountDb, CFId),
    case maybe_id_in_callflow(Id, kz_json:get_value(<<"flow">>, CFDoc)) of
        false ->
            false;
        true ->
            hd(kz_json:get_value(<<"numbers">>, CFDoc))
    end.

maybe_id_in_callflow(Id, Flow) ->
    Data = kz_json:get_value(<<"data">>, Flow),
    %% Skipping queue login and queue logout possibilities
    case {kz_json:get_value(<<"id">>, Data), kz_json:get_value(<<"module">>, Flow)} of
        {_, <<"acdc_queue">>} ->
            recurse_to_child_callflow(Id, Flow);
        {Id, _} ->
            true;
        _ ->
            recurse_to_child_callflow(Id, Flow)
    end.

recurse_to_child_callflow(Id, Flow) ->
    Children = kz_json:get_value(<<"children">>, Flow),
    case kz_json:get_value(<<"_">>, Children) of
        undefined ->
            false;
        SubFlow ->
            maybe_id_in_callflow(Id, SubFlow)
    end.





-spec queue_for_number(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} | {'error', atom()}.
queue_for_number(Number, AccountDb) ->
    case kz_datamgr:get_results(AccountDb, <<"callflow/listing_by_number">>, [{key, Number}]) of
        {ok, []} ->
            {error, number_not_found};
        {ok, [Result]} ->
            {ok, CFDoc} = kz_datamgr:open_doc(AccountDb, kz_json:get_value(<<"id">>, Result)),
            case maybe_queue_in_flow(kz_json:get_value(<<"flow">>, CFDoc)) of
                {error, E} ->
                    {error, E};
                {ok, QueueId} ->
                    kz_datamgr:open_doc(AccountDb, QueueId)
            end
    end.

maybe_queue_in_flow(Flow) ->
    case kz_json:get_value(<<"module">>, Flow) of
        <<"acdc_member">> ->
            {ok, kz_json:get_value(<<"id">>, kz_json:get_value(<<"data">>, Flow))};
        _ ->
            case kz_json:get_value(<<"_">>, kz_json:get_value(<<"flow">>, Flow)) of
                undefined ->
                    {error, invalid_queue_extension};
                SubFlow ->
                    maybe_queue_in_flow(SubFlow)
            end
    end.


                                                % %% Returns an 8-digit tail for channels for AMI calls
                                                % channel_tail(CallId) ->
                                                %       Seed = case binary:split(CallId, <<"-">>, [global]) of
                                                %               List when length(List) =:= 5 ->
                                                %                       %% When the call id looks like 4cad762c-f415-11e4-b890-cdee54d38ecb there may be many legs created
                                                %                       %% The 2nd and 3rd parts are unique to a call as a whole even though the 1st and 5th change per leg
                                                %                       <<(lists:nth(2, List))/binary, "-", (lists:nth(3, List))/binary>>;
                                                %               _ ->
                                                %                       CallId
                                                %       end,
                                                %     Digest = crypto:hash('md5', kz_term:to_binary(Seed)),
                                                %     MD5 = lists:flatten([io_lib:format("~2.16.0b", [Part]) || <<Part>> <= Digest]),
                                                %     list_to_binary(lists:sublist(MD5, length(MD5)-7, 8)).

%%------------------------------------------------------------------------------
%% @doc Add or update props used for origination commands
%% @end
%%------------------------------------------------------------------------------
-spec update_originate_props(kz_term:proplist()) -> kz_term:proplist().
update_originate_props(Props) ->
    AccountId = props:get_ne_binary_value(<<"AccountId">>, Props),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Channel = props:get_binary_value(<<"Channel">>, Props),
    SourceExten = channel_to_exten(Channel),

    props:set_values([{<<"AccountDb">>, AccountDb}
                     ,{<<"SourceExten">>, SourceExten}
                     ]
                    ,Props).

%%------------------------------------------------------------------------------
%% @doc Extract an extension (username) from a channel string
%% @end
%%------------------------------------------------------------------------------
-spec channel_to_exten(binary()) -> binary().
channel_to_exten(Channel) ->
    binary:replace(hd(binary:split(Channel, <<"@">>)), <<"SIP/">>, <<"">>).

%%------------------------------------------------------------------------------
%% @doc Create a kapps_call record with data pulled from props sent by
%% an AMI Action: Originate
%% @end
%%------------------------------------------------------------------------------
-spec kapps_call_from_ami_originate_props(kz_term:proplist()) -> kapps_call:call().
kapps_call_from_ami_originate_props(Props) ->
    Routines = [fun(C) -> kapps_call:set_account_id(props:get_ne_binary_value(<<"AccountId">>, Props), C) end
               ,fun(C) -> maybe_assign_aleg_props(Props, C) end
               ],
    kapps_call:exec(Routines, kapps_call:new()).

%%------------------------------------------------------------------------------
%% @doc Try to set props on the call if the authorizing ID can be
%% determined from the source extension
%% @end
%%------------------------------------------------------------------------------
-spec maybe_assign_aleg_props(kz_term:proplist(), kapps_call:call()) -> kapps_call:call().
maybe_assign_aleg_props(Props, Call) ->
    AccountDb = props:get_ne_binary_value(<<"AccountDb">>, Props),
    SourceExten = props:get_binary_value(<<"SourceExten">>, Props),
    case lookup_authorizing_id(AccountDb, SourceExten) of
        {'error', E} ->
            lager:debug("AMI: origination could not find aleg authorizing id (~p)", [E]),
            Call;
        {'ok', AuthorizingId} ->
            assign_aleg_props(AuthorizingId, Props, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Look up authorizing ID in account by username
%% @end
%%------------------------------------------------------------------------------
-spec assign_aleg_props(kz_term:ne_binary(), kz_term:proplist(), kapps_call:call()) -> kapps_call:call().
assign_aleg_props(AuthorizingId, Props, Call) ->
    To = props:get_binary_value(<<"SourceExten">>, Props),
    To1 = <<To/binary, "@blackholeami">>,
    Routines = [{fun kapps_call:set_authorizing_id/2, AuthorizingId}
               ,{fun kapps_call:set_authorizing_type/2, <<"user">>}
               ,{fun kapps_call:set_request/2, To1}
               ,{fun kapps_call:set_to/2, To1}
               ,{fun kapps_call:set_resource_type/2, <<"audio">>}
               ],
    kapps_call:exec(Routines, Call).

%%------------------------------------------------------------------------------
%% @doc Look up authorizing ID in account by username
%% @end
%%------------------------------------------------------------------------------
-spec lookup_authorizing_id(kz_term:ne_binary(), binary()) -> {'ok', kz_term:ne_binary()} | {'error', term()}.
lookup_authorizing_id(AccountDb, Username) ->
    ViewOptions = [{'key', Username}],
    case kz_datamgr:get_results(AccountDb, <<"users/list_by_username">>, ViewOptions) of
        {'ok', []} -> {'error', 'endpoint_not_found'};
        {'ok', [Result]} -> {'ok', kz_json:get_ne_binary_value(<<"id">>, Result)};
        E -> E
    end.
