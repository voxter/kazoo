-module(amimulator_vm).

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
        {notifications, [
            {restrict_to, [voicemail_new]},
            {realm, get_realm(AccountId)}
        ]}
    ].

responders(_Props) ->
    [
        {<<"directory">>, <<"reg_success">>},
        {<<"notification">>, <<"deregister">>}
    ].

handle_event(EventJObj) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).

%%
%% Event type handlers
%%

handle_specific_event(<<"reg_success">>, EventJObj) ->
    %lager:debug("reg success ~p", [EventJObj]);
    Realm = wh_json:get_value(<<"Realm">>, EventJObj),
    case couch_mgr:get_results(<<"accounts">>, <<"accounts/listing_by_realm">>, [{key, Realm}]) of
        {ok, [Result]} ->
            AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], Result),
            case wh_json:get_integer_value(<<"Expires">>, EventJObj, 0) of
                %% TODO
                0 ->
                    %% Manually requested deregister
                    handle_unregister(AccountId, EventJObj);
                _ ->
                    handle_register(AccountId, EventJObj)
            end;
        _ ->
            lager:debug("Couldn't find the realm that the registration was for"),
            ok
    end;
handle_specific_event(<<"deregister">>, EventJObj) ->
    %lager:debug("deregister ~p", [EventJObj]);
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    handle_unregister(AccountId, EventJObj);
handle_specific_event(_, _EventJObj) ->
    lager:debug("unhandled event").

%%
%% Private functions
%%

get_realm(AccountId) ->
    {ok, AccountDoc} = couch_mgr:open_doc(<<"accounts">>, AccountId),
    wh_json:get_value(<<"realm">>, AccountDoc).

handle_register(AccountId, EventJObj) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    Reg = cb_registrations:normalize_registration(EventJObj),
    case couch_mgr:get_results(AccountDb, <<"devices/sip_credentials">>, [{key, wh_json:get_value(<<"Username">>, EventJObj)}]) of
        {ok, [Result]} ->
            {ok, EndpointDoc} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, Result)),
            Exten = amimulator_util:endpoint_exten(EndpointDoc, AccountDb),
            Peer = <<"SIP/", Exten/binary>>,
            Payload = [[
                {<<"Event">>, <<"PeerStatus">>},
                {<<"Privilege">>, "system,all"},
                {<<"ChannelType">>, <<"SIP">>},
                {<<"Peer">>, Peer},
                {<<"PeerStatus">>, <<"Registered">>},
                {<<"Address">>, <<(wh_json:get_value(<<"contact_ip">>, Reg))/binary, ":",
                    (wh_json:get_value(<<"contact_port">>, Reg))/binary>>}
            ],[
                {<<"Event">>, <<"ExtensionStatus">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Exten">>, Exten},
                {<<"Context">>, <<"from-internal">>},
                {<<"Hint">>, <<Peer/binary, ",CustomPresence:", Exten/binary>>},
                {<<"Status">>, 0}
            ],[
                {<<"Event">>, <<"PeerStatus">>},
                {<<"Privilege">>, "system,all"},
                {<<"ChannelType">>, <<"SIP">>},
                {<<"Peer">>, Peer},
                {<<"PeerStatus">>, <<"Reachable">>},
                {<<"Time">>, <<"2">>}
            ]],
            ami_ev:publish_amqp_event({publish, Payload});
        _ ->
            ok
    end.

handle_unregister(AccountId, EventJObj) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:get_results(AccountDb, <<"devices/sip_credentials">>, [{key, wh_json:get_value(<<"Username">>, EventJObj)}]) of
        {ok, [Result]} ->
            {ok, EndpointDoc} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, Result)),
            Exten = amimulator_util:endpoint_exten(EndpointDoc, AccountDb),
            Peer = <<"SIP/", Exten/binary>>,
            Payload = [[
                {<<"Event">>, <<"PeerStatus">>},
                {<<"Privilege">>, "system,all"},
                {<<"ChannelType">>, <<"SIP">>},
                {<<"Peer">>, Peer},
                {<<"PeerStatus">>, <<"Unregistered">>},
                {<<"Cause">>, <<"Expired">>}
            ],[
                {<<"Event">>, <<"ExtensionStatus">>},
                {<<"Privilege">>, <<"call,all">>},
                {<<"Exten">>, Exten},
                {<<"Context">>, <<"from-internal">>},
                {<<"Hint">>, <<Peer/binary, ",CustomPresence:", Exten/binary>>},
                {<<"Status">>, 4}
            ]],
            ami_ev:publish_amqp_event({publish, Payload});
        _ ->
            ok
    end.
