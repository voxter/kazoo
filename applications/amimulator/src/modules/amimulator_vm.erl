-module(amimulator_vm).

-export([init/1, bindings/1, responders/1, handle_event/2]).

-include("../amimulator.hrl").

%%
%% Public functions
%%

init(_AccountId) ->
    ok.

bindings(Props) ->
    [
        {notifications, [
            {restrict_to, ['new_voicemail']},
            {account_id, props:get_value("AccountId", Props)}
        ]}
    ].

responders(_Props) ->
    [
        {<<"notification">>, <<"voicemail_saved">>}
    ].

handle_event(EventJObj, Props) ->
    {_EventType, EventName} = kz_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).

%%
%% Event type handlers
%%

%% Start of a VM has the following:
% 127.0.0.1            <- Event: Newexten
% 127.0.0.1            <- Privilege: dialplan,all
% 127.0.0.1            <- Channel: SIP/102-00000004
% 127.0.0.1            <- Context: macro-vm
% 127.0.0.1            <- Extension: s-NOANSWER
% 127.0.0.1            <- Priority: 2
% 127.0.0.1            <- Application: VoiceMail
% 127.0.0.1            <- AppData: 100@default,u
% 127.0.0.1            <- Uniqueid: 1433191552.4

handle_specific_event(<<"voicemail_saved">>, EventJObj) ->
    new_voicemail(EventJObj);
handle_specific_event(_, _EventJObj) ->
    lager:debug("unhandled event").

-spec new_voicemail(kz_json:object()) -> 'ok'.
new_voicemail(JObj) ->
    AccountDb = kz_json:get_value(<<"Account-DB">>, JObj),
    case couch_mgr:open_doc(AccountDb, kz_json:get_value(<<"Voicemail-Box">>, JObj)) of
        {'ok', VoicemailBox} ->
            Mailbox = kz_json:get_value(<<"mailbox">>, VoicemailBox),

            case couch_mgr:get_results(AccountDb, <<"vmboxes/crossbar_listing">>, [{'key', Mailbox}]) of
                {'ok', [Result]} ->
                    Value = kz_json:get_value(<<"value">>, Result),
                    Payload = [
                        {<<"Event">>, <<"MessageWaiting">>},
                        {<<"Privilege">>, <<"call,all">>},
                        {<<"Mailbox">>, <<Mailbox/binary, "@default">>},
                        {<<"Waiting">>, 1},
                        %% Assuming that this will always be behind by 1
                        {<<"New">>, kz_json:get_value(<<"new_messages">>, Value) + 1},
                        {<<"Old">>, kz_json:get_value(<<"old_messages">>, Value)}
                    ],
                    amimulator_event_listener:publish_amqp_event({'publish', Payload}, kz_json:get_value(<<"Account-ID">>, JObj));
                _ ->
                    lager:debug("Could not get voicemail count")
            end;
        _ ->
            lager:debug("Could not find voicemail box")
    end.
