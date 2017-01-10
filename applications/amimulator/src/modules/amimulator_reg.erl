-module(amimulator_reg).

-export([init/1
        ,bindings/1
        ,responders/1
        ,handle_event/2
        ]).

-include("../amimulator.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

%%
%% Public functions
%%

-spec init(ne_binary()) -> 'ok'.
init(_AccountId) ->
    ok.

-spec bindings(kz_proplist()) -> kz_proplist().
bindings(Props) ->
    AccountId = props:get_value("AccountId", Props),
    [{notifications, [{restrict_to, [deregister]}]}
    ,{registration, [{restrict_to, [reg_success]}
                    ,{realm, get_realm(AccountId)}
                    ]}].

-spec responders(kz_proplist()) -> kz_proplist().
responders(_Props) ->
    [{<<"directory">>, <<"reg_success">>}
    ,{<<"notification">>, <<"deregister">>}].

-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_event(EventJObj, _Props) ->
    {_EventType, EventName} = kz_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).

%%
%% Event type handlers
%%

handle_specific_event(<<"reg_success">>, EventJObj) ->
    Realm = kz_json:get_value(<<"Realm">>, EventJObj),
    case kz_datamgr:get_results(<<"accounts">>, <<"accounts/listing_by_realm">>, [{key, Realm}]) of
        {ok, [Result]} ->
            AccountId = kz_json:get_value([<<"value">>, <<"account_id">>], Result),
            case kz_json:get_integer_value(<<"Expires">>, EventJObj, 0) of
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
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    handle_unregister(AccountId, EventJObj);
handle_specific_event(_, _EventJObj) ->
    lager:debug("Unhandled event").

%%
%% Private functions
%%

get_realm(AccountId) ->
    {ok, AccountDoc} = kz_datamgr:open_doc(<<"accounts">>, AccountId),
    kz_json:get_value(<<"realm">>, AccountDoc).

handle_register(AccountId, EventJObj) ->
    AccountDb = kz_util:format_account_id(AccountId, encoded),

    case kz_datamgr:get_results(AccountDb, <<"devices/sip_credentials">>, [{key, kz_json:get_value(<<"Username">>, EventJObj)}]) of
        {ok, [Result]} ->
            EndpointId = kz_json:get_value(<<"id">>, Result),
            {'ok', EndpointDoc} = kz_datamgr:open_doc(AccountDb, EndpointId),
            Exten = amimulator_util:endpoint_exten(EndpointDoc),
            Contact = kz_json:get_value(<<"Contact">>, EventJObj),
            [#uri{domain=Domain,port=Port}] = kzsip_uri:uris(Contact),
            Port1 = kz_util:to_binary(Port),

            ami_sm:add_registration(AccountId, EndpointId, Domain, Port1),

            Peer = <<"SIP/", Exten/binary>>,
            Payload = [[
                        {<<"Event">>, <<"PeerStatus">>},
                        {<<"Privilege">>, "system,all"},
                        {<<"ChannelType">>, <<"SIP">>},
                        {<<"Peer">>, Peer},
                        {<<"PeerStatus">>, <<"Registered">>},
                        {<<"Address">>, <<Domain/binary, ":", Port1/binary>>}
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
            amimulator_event_listener:publish_amqp_event({publish, Payload}, AccountId);
        _ -> 'ok'
    end.

handle_unregister(AccountId, EventJObj) ->
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    case kz_datamgr:get_results(AccountDb, <<"devices/sip_credentials">>, [{key, kz_json:get_value(<<"Username">>, EventJObj)}]) of
        {ok, [Result]} ->
            {ok, EndpointDoc} = kz_datamgr:open_doc(AccountDb, kz_json:get_value(<<"id">>, Result)),

            ami_sm:delete_registration(kz_json:get_value(<<"id">>, Result)),

            Exten = amimulator_util:endpoint_exten(EndpointDoc),

            case Exten of
                'undefined' ->
                    lager:debug("exten not set, doc: ~p", [EndpointDoc]),
                    lager:debug("result ~p", [Result]);
                _ -> 'ok'
            end,

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
            amimulator_event_listener:publish_amqp_event({publish, Payload}, AccountId);
        _ ->
            ok
    end.
