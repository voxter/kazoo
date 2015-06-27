-module(amimulator_call_hook).

-export([init/1, bindings/1, responders/1, handle_event/1, handle_event/2]).

-include("../amimulator.hrl").

-define(STATE_UP, 6).

-define(CALLFLOW_CACHE, 'callflow_cache').
-define(CF_CONFIG_CAT, <<"callflow">>).
-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".park">>).

-define(DB_DOC_NAME, whapps_config:get(?MOD_CONFIG_CAT, <<"db_doc_name">>, <<"parked_calls">>)).
-define(DEFAULT_RINGBACK_TM, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_ringback_time">>, 120000)).
-define(PARKED_CALLS_KEY(Db), {'cf_park', 'parked_calls', Db}).

%%
%% Public functions
%%

init(AccountId) ->
    wh_hooks:register(AccountId).

bindings(_Props) ->
    [].

responders(_Props) ->
    [].

handle_event(EventJObj) ->
    handle_event(EventJObj, []).
    
handle_event(EventJObj, _Props) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).
    
handle_specific_event(<<"CHANNEL_CREATE">>, EventJObj) ->
	lager:debug("new channel with id ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)]),

    %% First add the sip version of the call
	Call = amimulator_util:create_call(EventJObj),
    ami_sm:new_call(Call),

    %% Publish the new one plus any Local/ calls that are required for queue calls
    relay_new_calls(maybe_create_agent_calls(Call));

handle_specific_event(<<"CHANNEL_ANSWER">>, EventJObj) ->
    lager:debug("channel answer for channel with id ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),

    amimulator_call_sup:relay_answer(CallId),
    amimulator_call_sup:relay_answer(<<CallId/binary, "-queue;1">>),
    amimulator_call_sup:relay_answer(<<CallId/binary, "-queue;2">>);

handle_specific_event(<<"CHANNEL_BRIDGE">>, EventJObj) ->
    lager:debug("channel bridge for channel with id ~p to ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)
                                                                ,wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj)]),
    amimulator_call_sup:relay_bridge(EventJObj);

handle_specific_event(<<"CHANNEL_DESTROY">>, EventJObj) ->
    lager:debug("channel destroy for channel with id ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)]),
    % lager:debug("channel destroy ~p", [EventJObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    HangupCause = wh_json:get_value(<<"Hangup-Cause">>, EventJObj),

    amimulator_call_sup:relay_destroy(HangupCause, CallId),
    amimulator_call_sup:relay_destroy(HangupCause, <<CallId/binary, "-queue;1">>),
    amimulator_call_sup:relay_destroy(HangupCause, <<CallId/binary, "-queue;2">>);
    
handle_specific_event(<<"DTMF">>, EventJObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Digit = wh_json:get_value(<<"DTMF-Digit">>, EventJObj),

    Payload = [
        {<<"Event">>, <<"DTMF">>},
        {<<"Privilege">>, <<"dtmf,all">>},
        {<<"Channel">>, CallId},
        {<<"Uniqueid">>, CallId},
        {<<"Digit">>, Digit},
        {<<"Direction">>, <<"Received">>},
        {<<"Begin">>, <<"Yes">>},
        {<<"End">>, <<"No">>}
    ],
    % TODO: Also need to do this with begin/end reversed
    
    amimulator_event_listener:publish_amqp_event({publish, Payload});

handle_specific_event(EventName, _EventJObj) ->
    lager:debug("unhandled call event ~p", [EventName]).

%%
%% Private functions
%%

maybe_create_agent_calls(Call) ->
    maybe_create_agent_calls(amimulator_call:ccv(<<"Member-Call-ID">>, Call), Call).

maybe_create_agent_calls('undefined', Call) ->
    [Call];
maybe_create_agent_calls(MemberCallId, Call) ->
    MemberCall = ami_sm:call(MemberCallId),
    create_agent_calls(MemberCall, Call).

create_agent_calls('undefined', Call) ->
    [Call];
create_agent_calls(MemberCall, Call) ->
    LocalCall1 = amimulator_util:fork_agent_call_leg1(Call, MemberCall),
    LocalCall2 = amimulator_util:fork_agent_call_leg2(Call, MemberCall),

    MemberCall2 = amimulator_call:set_other_leg_call_id('undefined', MemberCall),
    MemberCall3 = amimulator_call:set_other_channel('undefined', MemberCall2),

    Call2 = amimulator_call:set_other_leg_call_id('undefined', Call),
    Call3 = amimulator_call:set_other_channel('undefined', Call2),

    ami_sm:new_call(LocalCall1),
    ami_sm:new_call(LocalCall2),
    ami_sm:update_call(MemberCall3),
    ami_sm:update_call(Call3),

    [Call3, LocalCall1, LocalCall2].

relay_new_calls(Calls) ->
    lists:foreach(fun(Call) ->
        amimulator_call_sup:relay_new_call(Call)
    end, Calls).
