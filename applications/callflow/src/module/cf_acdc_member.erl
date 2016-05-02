%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%%
%%% Data: {
%%%   "id":"queue id"
%%% }
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(cf_acdc_member).

-export([handle/2]).

-include("../callflow.hrl").

-type max_wait() :: pos_integer() | 'infinity'.

-define(MEMBER_TIMEOUT, <<"member_timeout">>).
-define(MEMBER_HANGUP, <<"member_hangup">>).

-record(member_call, {call              :: whapps_call:call()
                      ,queue_id         :: api_binary()
                      ,config_data = [] :: wh_proplist()
                      ,breakout_media   :: api_object()
                      ,max_wait = 60 * ?MILLISECONDS_IN_SECOND :: max_wait()
                      ,silence_noop     :: api_binary()
                     }).
-type member_call() :: #member_call{}.

-record(breakout_state, {active = 'false'             :: boolean()
                         ,retries = 3                 :: non_neg_integer()
                         ,callback_number             :: api_binary()
                         ,callback_entering = 'false' :: boolean()
                        }).
-type breakout_state() :: #breakout_state{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    QueueId = maybe_use_variable(Data, Call),
    lager:info("sending call to queue ~s", [QueueId]),

    Priority = lookup_priority(Data, Call),

    MemberCall = props:filter_undefined(
                   [{<<"Account-ID">>, whapps_call:account_id(Call)}
                    ,{<<"Queue-ID">>, QueueId}
                    ,{<<"Call">>, whapps_call:to_json(Call)}
                    ,{<<"Member-Priority">>, Priority}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ]),

    lager:info("loading ACDc queue: ~s", [QueueId]),
    {'ok', QueueJObj} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), QueueId),

    MaxWait = max_wait(wh_json:get_integer_value(<<"connection_timeout">>, QueueJObj, 3600)),
    MaxQueueSize = max_queue_size(wh_json:get_integer_value(<<"max_queue_size">>, QueueJObj, 0)),

    Call1 = whapps_call:kvs_store_proplist([{'caller_exit_key', wh_json:get_value(<<"caller_exit_key">>, QueueJObj)}
                                            ,{'breakout_key', wh_json:get_value([<<"breakout">>, <<"dtmf">>], QueueJObj)}
                                           ], Call),

    CurrQueueSize = wapi_acdc_queue:queue_size(whapps_call:account_id(Call1), QueueId),

    lager:info("max size: ~p curr size: ~p", [MaxQueueSize, CurrQueueSize]),

    maybe_enter_queue(#member_call{call=Call1
                                   ,config_data=MemberCall
                                   ,breakout_media=wh_json:get_value([<<"breakout">>, <<"media">>], QueueJObj, wh_json:new())
                                   ,queue_id=QueueId
                                   ,max_wait=MaxWait
                                  }
                      ,is_queue_full(MaxQueueSize, CurrQueueSize)
                     ).

-spec maybe_use_variable(wh_json:object(), whapps_call:call()) -> api_binary().
maybe_use_variable(Data, Call) ->
    case wh_json:get_value(<<"var">>, Data) of
        'undefined' ->
            wh_doc:id(Data);
        Variable ->
            Value = wh_json:get_value(<<"value">>, cf_kvs_set:get_kv(Variable, Call)),
            case couch_mgr:open_cache_doc(whapps_call:account_db(Call), Value) of
                {'ok', _} -> Value;
                _ -> wh_doc:id(Data)
            end
    end.

-spec lookup_priority(wh_json:object(), whapps_call:call()) -> api_binary().
lookup_priority(Data, Call) ->
    FromData = wh_json:get_integer_value(<<"priority">>, Data),
    FromCall = whapps_call:custom_channel_var(<<"Call-Priority">>, Call),
    case {FromData, FromCall} of
        {FromData, _} when is_integer(FromData) -> FromData;
        {_, FromCall} when is_binary(FromCall) -> wh_util:to_integer(FromCall);
        _ -> 'undefined'
    end.

-spec maybe_enter_queue(member_call(), boolean()) -> any().
maybe_enter_queue(#member_call{call=Call}, 'true') ->
    lager:info("queue has reached max size"),
    cf_exe:continue(Call);
maybe_enter_queue(#member_call{call=Call
                               ,queue_id=QueueId
                               ,max_wait=MaxWait
                              }=MC
                  ,'false') ->
    case whapps_call_command:b_channel_status(whapps_call:call_id(Call)) of
        {'ok', _} ->
            lager:info("asking for an agent, waiting up to ~p ms", [MaxWait]),

            NoopId = whapps_call_command:flush_dtmf(Call),
            wait_for_bridge(MC#member_call{call=whapps_call:kvs_store('queue_id', QueueId, Call)
                                           ,silence_noop=NoopId
                                          }
                            ,#breakout_state{}
                            ,MaxWait
                           );
        {'error', E} ->
            lager:info("not entering queue; call was destroyed already (~s)", [E]),
            cf_exe:stop(Call)
    end.

-spec wait_for_bridge(member_call(), breakout_state(), max_wait()) -> 'ok'.
-spec wait_for_bridge(member_call(), breakout_state(), max_wait(), wh_now()) -> 'ok'.
wait_for_bridge(MC, BreakoutState, Timeout) ->
    wait_for_bridge(MC, BreakoutState, Timeout, os:timestamp()).
wait_for_bridge(#member_call{call=Call}, _, Timeout, _Start) when Timeout < 0 ->
    lager:debug("timeout is less than 0: ~p", [Timeout]),
    end_member_call(Call);
wait_for_bridge(#member_call{call=Call}=MC, BreakoutState, Timeout, Start) ->
    Wait = os:timestamp(),
    receive
        {'amqp_msg', JObj} ->
            process_message(MC, BreakoutState, Timeout, Start, Wait, JObj, wh_util:get_event_type(JObj))
    after Timeout ->
            lager:info("failed to handle the call in time, proceeding"),
            end_member_call(Call)
    end.

end_member_call(Call) ->
    cancel_member_call(Call, ?MEMBER_TIMEOUT),
    stop_hold_music(Call),
    cf_exe:continue(Call).

-spec process_message(member_call(), breakout_state(), max_wait(), wh_now()
                      ,wh_now(), wh_json:object()
                      ,{ne_binary(), ne_binary()}
                     ) -> 'ok'.
process_message(#member_call{call=Call}, _, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}) ->
    lager:info("member was bridged to agent, yay! took ~b s", [wh_util:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(#member_call{call=Call}, _, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_DESTROY">>}) ->
    lager:info("member hungup while waiting in the queue (was there ~b s)", [wh_util:elapsed_s(Start)]),
    cancel_member_call(Call, ?MEMBER_HANGUP),
    cf_exe:stop(Call);
process_message(#member_call{call=Call
                             ,config_data=MemberCall
                             ,silence_noop=NoopId
                            }=MC, BreakoutState, Timeout, Start, Wait, JObj, {<<"call_event">>,<<"CHANNEL_EXECUTE_COMPLETE">>}) ->
    case wh_json:get_first_defined([<<"Application-Name">>
                                    ,[<<"Request">>, <<"Application-Name">>]
                                   ], JObj) =:= <<"noop">> andalso
           wh_json:get_value(<<"Application-Response">>, JObj) =:= NoopId of
        'true' -> cf_exe:send_amqp(Call, MemberCall, fun wapi_acdc_queue:publish_member_call/1);
        'false' -> 'ok'
    end,
    wait_for_bridge(MC, BreakoutState, wh_util:decr_timeout(Timeout, Wait), Start);
process_message(#member_call{call=Call
                             ,queue_id=QueueId
                            }=MC, BreakoutState, Timeout, Start, Wait, JObj, {<<"member">>, <<"call_fail">>}) ->
    case QueueId =:= wh_json:get_value(<<"Queue-ID">>, JObj) of
        'true' ->
            Failure = wh_json:get_value(<<"Failure-Reason">>, JObj),
            lager:info("call failed to be processed: ~s (took ~b s)"
                       ,[Failure, wh_util:elapsed_s(Start)]
                      ),
            cancel_member_call(Call, Failure),
            stop_hold_music(Call),
            cf_exe:continue(Call);
        'false' ->
            lager:info("failure json was for a different queue, ignoring"),
            wait_for_bridge(MC, BreakoutState, wh_util:decr_timeout(Timeout, Wait), Start)
    end;
process_message(#member_call{call=Call}, _, _, Start, _Wait, _JObj, {<<"member">>, <<"call_success">>}) ->
    lager:info("call was processed by queue (took ~b s)", [wh_util:elapsed_s(Start)]),
    whapps_call_command:flush(Call),
    cf_exe:control_usurped(Call);
process_message(MC, BreakoutState, Timeout, Start, Wait, JObj, {<<"call_event">>, <<"DTMF">>}) ->
    DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    process_dtmf(DTMF, MC, BreakoutState, Timeout, Start, Wait);
process_message(MC, BreakoutState, Timeout, Start, Wait, _JObj, _Type) ->
    wait_for_bridge(MC, BreakoutState, wh_util:decr_timeout(Timeout, Wait), Start).

-spec process_dtmf(binary(), member_call(), breakout_state(), max_wait(), wh_now(), wh_now()) -> 'ok'.
process_dtmf(DTMF, #member_call{call=Call
                                ,breakout_media=BreakoutMedia
                               }=MC
             ,#breakout_state{active='false'}=BreakoutState, Timeout, Start, Wait) ->
    CallerExitKey = whapps_call:kvs_fetch('caller_exit_key', Call),
    BreakoutKey = whapps_call:kvs_fetch('breakout_key', Call),
    case DTMF of
        CallerExitKey ->
            lager:info("caller pressed the exit key(~s), moving to next callflow action", [DTMF]),
            cancel_member_call(Call, <<"dtmf_exit">>),
            _ = whapps_call_command:flush_dtmf(Call),
            timer:sleep(?MILLISECONDS_IN_SECOND),
            cf_exe:continue(Call);
        BreakoutKey ->
            lager:info("caller pressed the breakout menu key(~s)", [DTMF]),
            whapps_call_command:flush(Call),
            whapps_call_command:hold(<<"silence_stream://0">>, Call),
            whapps_call_command:prompt(breakout_prompt(BreakoutMedia), whapps_call:language(Call), Call),
            wait_for_bridge(MC, BreakoutState#breakout_state{active='true'}, wh_util:decr_timeout(Timeout, Wait), Start);
        _ ->
            lager:info("caller pressed ~s, ignoring", [DTMF]),
            wait_for_bridge(MC, BreakoutState, wh_util:decr_timeout(Timeout, Wait), Start)
    end;
process_dtmf(DTMF, #member_call{call=Call}=MC, BreakoutState, Timeout, Start, Wait) ->
    case breakout_loop(DTMF, MC, BreakoutState) of
        #breakout_state{}=NextState ->
            wait_for_bridge(MC, NextState, wh_util:decr_timeout(Timeout, Wait), Start);
        'callback_registered' ->
            lager:debug("member callback registered, stopping callflow"),
            cf_exe:control_usurped(Call)
    end.

-spec breakout_loop(binary(), member_call(), breakout_state()) -> breakout_state().
breakout_loop(_, #member_call{call=Call}, #breakout_state{retries=0}) ->
    lager:info("maximum number of retries reached"),
    whapps_call_command:flush_dtmf(Call),
    whapps_call_command:hold(Call),
    #breakout_state{};
breakout_loop(DTMF, #member_call{call=Call}=MC, State) ->
    whapps_call_command:flush(Call),
    process_breakout_message(DTMF, MC, State).

-spec process_breakout_message(binary(), member_call(), breakout_state()) -> breakout_state() | 'callback_registered'.
process_breakout_message(DTMF, #member_call{call=Call
                                            ,breakout_media=BreakoutMedia
                                           }
                         ,#breakout_state{callback_number='undefined'}=State) ->
    case DTMF of
        <<"1">> ->
            From = whapps_call:from_user(Call),
            breakout_number_correct(Call, BreakoutMedia, State#breakout_state{callback_number=From});
        DTMF -> breakout_invalid_selection(Call, State, DTMF)
    end;
process_breakout_message(DTMF
                         ,#member_call{call=Call
                                       ,queue_id=QueueId
                                       ,breakout_media=BreakoutMedia
                                      }
                         ,#breakout_state{callback_number=Number
                                          ,callback_entering='false'
                                         }=State
                        ) ->
    case DTMF of
        <<"1">> ->
            lager:debug("accepted callback for number ~s", [Number]),
            Payload = [{<<"Account-ID">>, whapps_call:account_id(Call)}
                       ,{<<"Queue-ID">>, QueueId}
                       ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                       ,{<<"Number">>, Number}
                       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ],
            wapi_acdc_queue:publish_member_callback_reg(Payload),

            PromptVars = wh_json:from_list([{<<"var1">>, <<"breakout-callback_registered">>}]),
            whapps_call_command:prompt(callback_registered(BreakoutMedia), whapps_call:language(Call), PromptVars, Call),
            whapps_call_command:queued_hangup(Call),

            'callback_registered';
        <<"2">> ->
            whapps_call_command:prompt(enter_callback_number(BreakoutMedia), Call),
            State#breakout_state{callback_number= <<>>
                                 ,callback_entering='true'
                                };
        DTMF -> breakout_invalid_selection(Call, State, DTMF)
    end;
process_breakout_message(DTMF
                         ,#member_call{call=Call
                                       ,breakout_media=BreakoutMedia
                                      }
                         ,#breakout_state{callback_number=Number
                                          ,callback_entering='true'
                                         }=State
                        ) ->
    case DTMF of
        <<"#">> -> breakout_number_correct(Call, BreakoutMedia, State#breakout_state{callback_entering='false'});
        _ -> State#breakout_state{callback_number= <<Number/binary, DTMF/binary>>}
    end.

-spec breakout_number_correct(whapps_call:call(), ne_binary(), breakout_state()) -> breakout_state().
breakout_number_correct(Call, BreakoutMedia, #breakout_state{callback_number=Number}=State) ->
    Prompt = [{'prompt', call_back_at(BreakoutMedia)}
              ,{'say', Number}
              ,{'prompt', number_correct(BreakoutMedia)}
             ],
    whapps_call_command:audio_macro(Prompt, Call),
    State.

-spec breakout_invalid_selection(whapps_call:call(), breakout_state(), ne_binary()) -> breakout_state().
breakout_invalid_selection(Call, #breakout_state{retries=Retries}=State, DTMF) ->
    lager:debug("invalid selection ~s", [DTMF]),
    whapps_call_command:prompt(<<"menu-invalid_entry">>, Call),
    State#breakout_state{retries=Retries-1}.

-spec breakout_prompt(wh_json:object()) -> ne_binary().
breakout_prompt(JObj) ->
    wh_json:get_ne_value(<<"prompt">>, JObj, <<"breakout-prompt">>).

-spec callback_registered(wh_json:object()) -> ne_binary().
callback_registered(JObj) ->
    wh_json:get_ne_value(<<"callback_registered">>, JObj, <<"breakout-callback_registered">>).

-spec enter_callback_number(wh_json:object()) -> ne_binary().
enter_callback_number(JObj) ->
    wh_json:get_ne_value(<<"enter_callback_number">>, JObj, <<"breakout-enter_callback_number">>).

-spec call_back_at(wh_json:object()) -> ne_binary().
call_back_at(JObj) ->
    wh_json:get_ne_value(<<"call_back_at">>, JObj, <<"breakout-call_back_at">>).

-spec number_correct(wh_json:object()) -> ne_binary().
number_correct(JObj) ->
    wh_json:get_ne_value(<<"number_correct">>, JObj, <<"breakout-number_correct">>).

%% convert from seconds to milliseconds, or infinity
-spec max_wait(integer()) -> max_wait().
max_wait(N) when N < 1 -> 'infinity';
max_wait(N) -> N * ?MILLISECONDS_IN_SECOND.

max_queue_size(N) when is_integer(N), N > 0 -> N;
max_queue_size(_) -> 0.

-spec is_queue_full(non_neg_integer(), non_neg_integer()) -> boolean().
is_queue_full(0, _) -> 'false';
is_queue_full(MaxQueueSize, CurrQueueSize) -> CurrQueueSize >= MaxQueueSize.

-spec cancel_member_call(whapps_call:call(), ne_binary()) -> 'ok'.
cancel_member_call(Call, <<"timeout">>) ->
    lager:info("update reason from `timeout` to `member_timeout`"),
    cancel_member_call(Call, ?MEMBER_TIMEOUT);
cancel_member_call(_, <<"no agents">>) ->
    'ok';
cancel_member_call(Call, Reason) ->
    AcctId = whapps_call:account_id(Call),
    {'ok', QueueId} = whapps_call:kvs_find('queue_id', Call),
    CallId = whapps_call:call_id(Call),

    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Reason">>, Reason}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    wapi_acdc_queue:publish_member_call_cancel(Req).

stop_hold_music(Call) ->
    Cmd = [{<<"Application-Name">>, <<"play">>}
           ,{<<"Call-ID">>, whapps_call:call_id(Call)}
           ,{<<"Media-Name">>, <<"silence_stream://50">>}
           ,{<<"Insert-At">>, <<"now">>}
          ],
    whapps_call_command:send_command(Cmd, Call).
