-module(amimulator_call_hook).

-export([init/1, bindings/1, responders/1, handle_event/1, handle_event/2]).

-include("../amimulator.hrl").

-define(STATE_UP, 6).

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
	Call = amimulator_util:create_call(EventJObj),
    ami_sm:new_call(Call),
    amimulator_call_sup:relay_new_call(Call);

handle_specific_event(<<"CHANNEL_ANSWER">>, EventJObj) ->
    lager:debug("channel answer for channel with id ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)]),
    lager:debug("channel answer ~p", [EventJObj]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),
    amimulator_call_sup:relay_answer(Call);

handle_specific_event(<<"CHANNEL_BRIDGE">>, EventJObj) ->
    lager:debug("channel bridge for channel with id ~p to ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)
                                                                ,wh_json:get_value(<<"Call-ID">>, EventJObj)]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    OtherCallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),
    OtherCall = ami_sm:call(OtherCallId),

    amimulator_call_sup:relay_bridge(Call, OtherCall);

    % UpdaterParams = [{OtherCallId, OtherCall}, {CallId, Call}],
    % Updaters = [
    %     fun(Call2) -> props:set_value(<<"call">>,
    %         whapps_call:set_other_leg_call_id(OtherCallId, WhappsCall), Call2) end,
    %     fun(Call2) -> amimulator_util:bleg_ami_channel(Call2, undefined, UpdaterParams) end,
    %     fun(Call2) -> amimulator_util:bleg_cid(Call2, undefined, UpdaterParams) end,
    %     fun(Call2) ->
    %       FlipDirection = wh_util:to_atom(whapps_call:custom_channel_var(<<"Flip-Direction-On-Bridge">>, <<"false">>, WhappsCall)) or
    %                         (whapps_call:caller_id_name(WhappsCall) =:= <<"Device QuickCall">>),

    %           if FlipDirection ->
    %               case props:get_value(<<"direction">>, Call2) of
    %               <<"inbound">> -> props:set_value(<<"direction">>, <<"outbound">>, Call2);
    %               <<"outbound">> -> props:set_value(<<"direction">>, <<"inbound">>, Call2)
    %           end;
    %       true ->
    %           Call2
    %       end end
    % ],
    % Call2 = lists:foldl(fun(F, Call3) -> F(Call3) end, Call, Updaters),

handle_specific_event(<<"CHANNEL_DESTROY">>, EventJObj) ->
    lager:debug("channel destroy for channel with id ~p", [wh_json:get_value(<<"Call-ID">>, EventJObj)]),
    CallId = wh_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),
    amimulator_call_sup:relay_destroy(wh_json:get_value(<<"Hangup-Cause">>, EventJObj), Call);
    
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
    
    ami_ev:publish_amqp_event({publish, Payload});
    
handle_specific_event(EventName, _EventJObj) ->
    lager:debug("unhandled call event ~p", [EventName]).
