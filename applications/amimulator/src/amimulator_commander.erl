-module(amimulator_commander).

-export([handle/3]).
-export([handle_event/2]).

-include("amimulator.hrl").

-define(AMI_DB, <<"ami">>).

%% Handle a payload sent as an AMI command
-spec handle(binary(), ne_binary(), pos_integer()) ->
                    {'ok', {kz_proplist(), 'n'}}
                        | {'logoff', 'ok'} | kz_proplist()
                        | {'error', 'no_action'}.
handle(Payload, AccountId, 'undefined') ->
    Props = amimulator_util:parse_payload(Payload),
    handle_event(update_props(Props, AccountId));
%% When authenticating via md5 challenge, pass the challenge along
handle(Payload, AccountId, Challenge) ->
    Props = amimulator_util:parse_payload(Payload),
    handle_event("login", props:set_value(<<"Challenge">>, Challenge, update_props(Props, AccountId))).

update_props(Props, AccountId) ->
    Routines = [
                fun(Props2) -> [{<<"AccountId">>, AccountId}] ++ Props2 end,
                fun(Props2) -> case AccountId of
                                   <<>> ->
                                       Props2;
                                   _ ->
                                       [{<<"AccountDb">>, kz_util:format_account_id(AccountId, encoded)}] ++ Props2
                               end end
               ],
    lists:foldl(fun(F, Props2) -> F(Props2) end, Props, Routines).

handle_event(Props) ->
    Action = string:to_lower(kz_term:to_list(proplists:get_value(<<"Action">>, Props))),
    handle_event(Action, Props).

                                                % TODO: add AMI username lookup db initialization
-spec handle_event(string(), kz_proplist()) ->
                          {'ok', {kz_proplist(), 'n'}}
                              | {'logoff', 'ok'} | kz_proplist()
                              | {'error', 'no_action'}.
handle_event("login", Props) ->
    Username = proplists:get_value(<<"Username">>, Props),
    Secret = proplists:get_value(<<"Secret">>, Props),
    Key = proplists:get_value(<<"Key">>, Props),
    ActionId = proplists:get_value(<<"ActionID">>, Props),

    case Key of
        'undefined' -> login_secret(Username, Secret, ActionId);
        _ -> login_md5(Username, Key, proplists:get_value(<<"Challenge">>, Props), ActionId)
    end;
handle_event("logoff", _Props) ->
    gen_server:cast(self(), {logout}),
    {logoff, ok};
handle_event("challenge", Props) ->
    Challenge = rand:uniform(899999999) + 100000000,
    ActionID = proplists:get_value(<<"ActionID">>, Props),
    Payload = [
               {<<"Asterisk Call Manager/1.1">>},
               {<<"Response">>, <<"Success">>},
               {<<"Challenge">>, Challenge},
               {<<"ActionID">>, ActionID}
              ],
    [{<<"Ret">>, {'ok', {Payload, 'n'}}}, {<<"Challenge">>, Challenge}];
handle_event("ping", Props) ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    Timestamp = Megasecs * 1000000 + Secs + Microsecs / 1000000,
    Payload = props:filter_undefined([
                                      {<<"Response">>, <<"Success">>},
                                      {<<"Ping">>, <<"Pong">>},
                                      {<<"Timestamp">>, Timestamp},
                                      {<<"ActionID">>, props:get_value(<<"ActionID">>, Props)}
                                     ]),
    {ok, {Payload, n}};
                                                % Handle AMI Status action
handle_event("status", Props) ->
    Calls = ami_sm:calls(props:get_value(<<"AccountId">>, Props)),
    Payload = initial_channel_status(Calls, Props, <<"Status">>),
    {ok, {Payload, n}};

handle_event("queuestatus", Props) ->
    Header = [[
               {<<"Response">>, <<"Success">>},
               {<<"Message">>, <<"Queue status will follow">>}
              ]],
    Footer = [[
               {<<"Event">>, <<"QueueStatusComplete">>}
              ]],
    {'ok', {Header ++ queues_status(Props) ++ Footer, 'n'}};

handle_event("sippeers", Props) ->
    ActionId = props:get_value(<<"ActionID">>, Props),
    SipPeers = sip_peers(Props),

    Payload = [[
                {<<"Response">>, <<"Success">>},
                {<<"ActionID">>, ActionId},
                {<<"EventList">>, <<"start">>},
                {<<"Message">>, <<"Peer status list will follow">>}
               ]] ++ SipPeers ++ [[
                                   {<<"Event">>, <<"PeerlistComplete">>},
                                   {<<"EventList">>, <<"Complete">>},
                                   {<<"ListItems">>, length(SipPeers)},
                                   {<<"ActionID">>, ActionId}
                                  ]],
    {ok, {Payload, n}};
handle_event("sipshowpeer", Props) ->
    Payload = props:filter_undefined([
                                      {<<"Response">>, <<"Success">>},
                                      {<<"ActionID">>, props:get_value(<<"ActionID">>, Props)},
                                      {<<"Channeltype">>, <<"SIP">>},
                                      {<<"ObjectName">>, <<"1011">>},
                                      {<<"ChanObjectType">>, <<"peer">>},
                                      {<<"SecretExist">>, <<"Y">>},
                                      {<<"RemoteSecretExist">>, <<"N">>},
                                      {<<"MD5SecretExist">>, <<"N">>},
                                      {<<"Context">>, <<"from-internal">>},
                                      {<<"Language">>, <<>>},
                                      {<<"AMAflags">>, <<"Unknown">>},
                                      {<<"CID-CallingPres">>, <<"Presentation Allowed, Not Screened">>},
                                      {<<"Callgroup">>, <<>>},
                                      {<<"Pickupgroup">>, <<>>},
                                      {<<"MOHSuggest">>, <<>>},
                                      {<<"VoiceMailbox">>, <<"1011@default">>},
                                      {<<"TransferMode">>, <<"open">>},
                                      {<<"Maxforwards">>, <<"0">>},
                                      {<<"LastMsgsSent">>, <<"-1">>},
                                      {<<"Maxforwards">>, <<"0">>},
                                      {<<"Call-limit">>, <<"2147483647">>},
                                      {<<"Busy-level">>, <<"0">>},
                                      {<<"MaxCallBR">>, <<"384 kbps">>},
                                      {<<"Dynamic">>, <<"Y">>},
                                      {<<"Callerid">>, <<"\"device\" <1011>">>},
                                      {<<"RegExpire">>, <<"71 seconds">>},
                                      {<<"SIP-AuthInsecure">>, <<"no">>},
                                      {<<"SIP-Forcerport">>, <<"Y">>},
                                      {<<"ACL">>, <<"Y">>},
                                      {<<"SIP-CanReinvite">>, <<"N">>},
                                      {<<"SIP-DirectMedia">>, <<"N">>},
                                      {<<"SIP-PromiscRedir">>, <<"N">>},
                                      {<<"SIP-UserPhone">>, <<"N">>},
                                      {<<"SIP-VideoSupport">>, <<"N">>},
                                      {<<"SIP-TextSupport">>, <<"N">>},
                                      {<<"SIP-T.38Support">>, <<"N">>},
                                      {<<"SIP-T.38EC">>, <<"Unknown">>},
                                      {<<"SIP-T.38MaxDtgrm">>, <<"-1">>},
                                      {<<"SIP-Sess-Timers">>, <<"Accept">>},
                                      {<<"SIP-Sess-Refresh">>, <<"uas">>},
                                      {<<"SIP-Sess-Expires">>, <<"1800">>},
                                      {<<"SIP-Sess-Min">>, <<"90">>},
                                      {<<"SIP-RTP-Engine">>, <<"asterisk">>},
                                      {<<"SIP-Encryption">>, <<"N">>},
                                      {<<"SIP-DTMFmode">>, <<"rfc2833">>},
                                      {<<"ToHost">>, <<>>},
                                      {<<"Address-IP">>, <<"206.191.105.50">>},
                                      {<<"Address-Port">>, <<"52511">>},
                                      {<<"Default-addr-IP">>, <<"(null)">>},
                                      {<<"Default-addr-port">>, <<"0">>},
                                      {<<"Default-Username">>, <<"448">>},
                                      {<<"Codecs">>, <<"0x104 (ulaw|g729)">>},
                                      {<<"CodecOrder">>, <<"ulaw,g729">>},
                                      {<<"Status">>, <<"OK (66 ms)">>},
                                      {<<"SIP-Useragent">>, <<"Aastra 57i/3.3.1.4305">>},
                                      {<<"Reg-Contact">>, <<"sip:1011@10.2.0.63:5060;transport=udp">>},
                                      {<<"QualifyFreq">>, <<"60000 ms">>},
                                      {<<"Parkinglot">>, <<>>},
                                      {<<"SIP-Use-Reason-Header ">>, <<"N">>}
                                     ]),
    {'ok', {Payload, 'n'}};
handle_event("mailboxcount", Props) ->
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),
    Mailbox = proplists:get_value(<<"Mailbox">>, Props),
    ActionId = proplists:get_value(<<"ActionID">>, Props),
    Exten = hd(binary:split(Mailbox, <<"@">>)),

    Payload = case kz_datamgr:get_results(AccountDb, <<"vmboxes/crossbar_listing">>, [{key, Exten}]) of
                  {ok, [Result]} ->
                      Value = kz_json:get_value(<<"value">>, Result),
                      [
                       {<<"Response">>, <<"Success">>},
                       {<<"ActionID">>, ActionId},
                       {<<"Message">>, <<"Mailbox Message Count">>},
                       {<<"Mailbox">>, Mailbox},
                       {<<"UrgMessages">>, 0},
                       {<<"NewMessages">>, kz_json:get_value(<<"new_messages">>, Value)},
                       {<<"OldMessages">>, kz_json:get_value(<<"old_messages">>, Value)}
                      ];
                  _ ->
                      mailbox_count_error(ActionId, Mailbox)
              end,
    {ok, {Payload, n}};
handle_event("queuepause", Props) ->
    queue_pause(Props);
handle_event("queueadd", Props) ->
    queue_add(Props);
handle_event("queueremove", Props) ->
    queue_remove(Props);
handle_event("originate", Props) ->
    case proplists:get_value(<<"Channel">>, Props) of
        undefined ->
            {error, channel_not_specified};
        _ ->
            case proplists:get_value(<<"Application">>, Props) of
                <<"ChanSpy">> ->
                    gen_listener:cast(amimulator_originator, {"eavesdrop", Props});
                <<"PickupChan">> ->
                                                % Props2 = props:set_value(<<"Channel">>, props:get_value(<<"Data">>,
                                                %       props:set_value(<<"SourceExten">>, hd(binary:split(props:get_value(<<"Channel">>, Props), <<"/">>)),
                                                %       Props)), Props),
                    gen_listener:cast(amimulator_originator, {"pickupchan", Props});
                _ ->
                    gen_listener:cast(amimulator_originator, {"originate", Props})
            end
    end;
handle_event("redirect", Props) ->
    EndpointName = props:get_value(<<"Channel">>, Props),

    Call = ami_sm:call_by_channel(EndpointName),
    case Call of
        undefined ->
            lager:debug("Missing call when trying to transfer"),
            ok;
        _ ->
            case props:get_value(<<"Context">>, Props) of
                <<"default">> ->
                    gen_listener:cast(amimulator_originator, {"vmxfer", props:set_value(<<"Call">>, Call, Props)});
                _ ->
                    gen_listener:cast(amimulator_originator, {"blindxfer", props:set_value(<<"Call">>, Call, Props)})
            end
    end;
handle_event("atxfer", Props) ->
    EndpointName = props:get_value(<<"Channel">>, Props),

    Call = ami_sm:call_by_channel(EndpointName),
    case Call of
        undefined ->
            lager:debug("Missing call when trying to transfer attended"),
            ok;
        _ ->
            gen_listener:cast(amimulator_originator, {"atxfer", props:set_value(<<"Call">>, Call, Props)})
    end;
handle_event("hangup", Props) ->
    EndpointName = props:get_value(<<"Channel">>, Props),
    Call = ami_sm:call_by_channel(EndpointName),
    lager:debug("Hanging up channel ~p (call id ~p)", [EndpointName, amimulator_call:call_id(Call)]),
    ControlQueue = amimulator_call:control_queue(Call),
    lager:debug("Control queue for call: ~p", [ControlQueue]),

    kapps_call_command:hangup(amimulator_call:to_kapps_call(amimulator_call:set_control_queue(ControlQueue, Call)));
                                                % CallId = ami_sm:call_by_channel(EndpointName),
                                                % Call = amimulator_util:kapps_call_from_cf_exe(CallId),
                                                % kapps_call_command:hangup(Call);
handle_event("getvar", Props) ->
    case getvar(proplists:get_value(<<"Variable">>, Props), Props) of
        undefined ->
            {error, undefined};
        {Payload, Mode} ->
            {ok, {Payload, Mode}};
        Payload ->
            {ok, {Payload, n}}
    end;
handle_event("command", Props) ->
    case command(proplists:get_value(<<"Command">>, Props), Props) of
        undefined ->
            {error, undefined};
        {Payload, Mode} ->
            {ok, {Payload, Mode}};
        Payload ->
            {ok, {Payload, n}}
    end;
handle_event("userevent", Props) ->
    case user_event(proplists:get_value(<<"UserEvent">>, Props), Props) of
        undefined ->
            {error, undefined};
        Payload ->
            {ok, {Payload, n}}
    end;
handle_event("events", Props) ->
    Events = case props:get_value(<<"EventMask">>, Props) of
                 <<"off">> ->
                     <<"Off">>;
                 EventMask ->
                     EventMask
             end,
    Payload = [
               {<<"Response">>, <<"Success">>},
               {<<"Events">>, Events}
              ],
    {ok, {Payload, n}};
handle_event("listcommands", Props) ->
    list_commands(Props);
handle_event(Event, Props) ->
    lager:debug("no handler defined for event ~p, props ~p", [Event, Props]),
    {error, no_action}.

login_secret(Username, Secret, ActionId) ->
    case get_ami_doc(Username) of
        'not_found' -> login_fail(ActionId);
        AMIDoc ->
            AMIDocSecret = kz_json:get_value(<<"secret">>, AMIDoc),
            AccountId = kz_json:get_value(<<"account_id">>, AMIDoc),
            check_login(Secret, AMIDocSecret, AccountId, ActionId)
    end.

login_md5(Username, Md5, Challenge, ActionId) ->
    case get_ami_doc(Username) of
        'not_found' -> login_fail(ActionId);
        AMIDoc ->
            AMIDocSecret = kz_json:get_value(<<"secret">>, AMIDoc),
            AccountId = kz_json:get_value(<<"account_id">>, AMIDoc),
            Digest = crypto:hash('md5', <<(kz_term:to_binary(Challenge))/binary, AMIDocSecret/binary>>),
            SuccessMd5 = kz_term:to_binary(lists:flatten([io_lib:format("~2.16.0b", [Part]) || <<Part>> <= Digest])),
            check_login(Md5, SuccessMd5, AccountId, ActionId)
    end.

-spec check_login(ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                         {'ok', {kz_proplist(), 'broken' | 'n'}}.
check_login(SuccessCredentials, SuccessCredentials, AccountId, ActionId) ->
    login_success(AccountId, ActionId);
check_login(_, _, _, ActionId) ->
    login_fail(ActionId).

login_success(AccountId, ActionId) ->
    lager:debug("successful login, starting event listener"),

    %% Record account id that is being used for logged in account
    amimulator_socket_listener:login(AccountId),

    Payload = props:filter_undefined([{<<"Response">>, <<"Success">>}
                                     ,{<<"ActionID">>, ActionId}
                                     ,{<<"Message">>, <<"Authentication accepted">>}
                                     ]),
    {'ok', {Payload, 'broken'}}.

login_fail(ActionId) ->
    Payload = props:filter_undefined([{<<"Response">>, <<"Error">>}
                                     ,{<<"ActionID">>, ActionId}
                                     ,{<<"Message">>, <<"Authentication failed">>}
                                     ]),
    {'ok', {Payload, 'n'}}.

-spec get_ami_doc(ne_binary()) -> kz_json:object() | 'not_found'.
get_ami_doc(Username) ->
    case kz_datamgr:open_doc(?AMI_DB, Username) of
        {'ok', Doc} -> Doc;
        {'error', _} -> 'not_found'
    end.

initial_channel_status(Calls, _Props, Format) ->
    FormattedCalls = lists:foldl(fun(Call, List) ->
                                         [ami_channel_status(Call, Format)] ++ List
                                 end, [], Calls),
    case Format of
        <<"Status">> ->
            [[
              {<<"Response">>, <<"Success">>},
              {<<"Message">>, <<"Channel status will follow">>}
             ]] ++ FormattedCalls ++ [[
                                       {<<"Event">>, <<"StatusComplete">>},
                                       {<<"Items">>, length(FormattedCalls)}
                                      ]];
        <<"concise">> ->
            lager:debug("~p", [FormattedCalls]),
            {[
              <<"Response: Follows\r\nPrivilege: Command\r\n">>
             ] ++ FormattedCalls ++ [
                                     <<"--END COMMAND--\r\n\r\n">>
                                    ], 'raw'}
    end.

ami_channel_status(Call, Format) ->
    CallId = amimulator_call:call_id(Call),
    BridgedCallId = amimulator_call:other_leg_call_id(Call),
    Channel = amimulator_call:channel(Call),
    BridgedChannel = other_channel(amimulator_call:other_channel(Call), Format),
    CID = amimulator_call:id_name(Call),
    OtherCID = other_cid(Call),
    {ChannelState, ChannelStateDesc} = channel_state(Call),
    {Application, Context} = application_and_context(Call),
    DestExten = dest_exten(Call),
    ElapsedSeconds = amimulator_call:elapsed_s(Call),

    PL = status_payload(Format, Channel, BridgedChannel, CID, CID, OtherCID, OtherCID
                       ,ChannelState, ChannelStateDesc, Application, Context, DestExten, ElapsedSeconds
                       ,CallId, BridgedCallId),
                                                % lager:debug("direction ~p gave ~p", [amimulator_call:direction(Call), PL]),
    PL.

other_channel('undefined', <<"concise">>) ->
    <<"(None)">>;
other_channel(Channel, _) ->
    Channel.

other_cid(Call) ->
    other_cid(amimulator_call:answered(Call), amimulator_call:direction(Call), amimulator_call:acdc_queue_id(Call)
             ,amimulator_call:account_id(Call), amimulator_call:other_channel(Call), Call).

other_cid(_, _, 'undefined', _, _, Call) ->
    amimulator_call:other_id_name(Call);
other_cid('true', <<"outbound">>, _, _, _, _) ->
    <<"(Outgoing Line)">>;
other_cid(_, _, _, _, _, Call) ->
    lager:debug("undeterminable other call CID"),
    amimulator_call:other_id_name(Call).

dest_exten(Call) ->
    dest_exten(amimulator_call:answered(Call), amimulator_call:direction(Call), amimulator_call:acdc_queue_id(Call)
              ,amimulator_call:account_id(Call), amimulator_call:other_channel(Call), Call).

dest_exten(_, _, 'undefined', _, _, Call) ->
    amimulator_call:other_id_number(Call);
dest_exten('true', <<"inbound">>, QueueId, AccountId, <<"Local", _/binary>>, Call) ->
    case amimulator_util:queue_number(kz_util:format_account_id(AccountId, 'encoded'), QueueId) of
        'undefined' -> amimulator_call:other_id_number(Call);
        Number -> Number
    end;
dest_exten('true', <<"outbound">>, QueueId, AccountId, <<"SIP", _/binary>>, Call) ->
    case amimulator_util:queue_number(kz_util:format_account_id(AccountId, 'encoded'), QueueId) of
        'undefined' -> amimulator_call:other_id_number(Call);
        Number -> Number
    end;
dest_exten('true', _, _, _, _, _) ->
    <<"s">>;
dest_exten(_, _, _, _, _, Call) ->
    lager:debug("undeterminable destination extension"),
    amimulator_call:other_id_number(Call).

channel_state(Call) ->
    channel_state(amimulator_call:answered(Call), amimulator_call:direction(Call)).

channel_state('true', _) ->
    {6, <<"Up">>};
channel_state(_, <<"inbound">>) ->
    {4, <<"Ring">>};
channel_state(_, <<"outbound">>) ->
    {5, <<"Ringing">>}.

application_and_context(Call) ->
    application_and_context(amimulator_call:answered(Call), amimulator_call:direction(Call), amimulator_call:acdc_queue_id(Call)
                           ,amimulator_call:account_id(Call), amimulator_call:other_channel(Call), Call).

application_and_context(_, _, 'undefined', _, _, _) ->
    {<<"Dial">>, <<"from-internal">>};
application_and_context('true', <<"inbound">>, _, _, <<"Local", _/binary>>, _) ->
    {<<"Queue">>, <<"ext-queues">>};
application_and_context('true', <<"inbound">>, _, _, <<"SIP", _/binary>>, _) ->
    {<<"Dial">>, <<"macro-dial-one">>};
application_and_context('true', <<"outbound">>, _, _, <<"SIP", _/binary>>, _) ->
    {<<"AppQueue">>, <<"from-queue">>};
application_and_context('true', <<"outbound">>, _, _, <<"Local", _/binary>>, _) ->
    {<<"AppDial">>, <<"macro-dial-one">>};
application_and_context(_, _, _, _, _, _) ->
    lager:debug("undeterminable application and context"),
    {<<"Dial">>, <<"from-internal">>}.




                                                % application_and_context(_, _, 'undefined', _) ->
                                                %     {<<"Dial">>, <<"from_internal">>};
                                                % application_and_context('true', <<"inbound">>, _, <<"SIP", _/binary>>) ->
                                                %     {<<"Dial">>, <<"macro-dial-one">>};
                                                % application_and_context('true', <<"inbound">>, _, <<"Local", _/binary>>) ->
                                                %     {<<"Queue">>, <<"ext-queues">>};
                                                % application_and_context('true', <<"outbound">>, _, <<"SIP", _/binary>>) ->
                                                %     {<<"AppQueue">>, <<"from-queue">>};
                                                % application_and_context('true', <<"outbound">>, _, <<"Local", _/binary>>) ->
                                                %     {<<"Queue">>, <<"ext-queues">>};
                                                % application_and_context(_, <<"inbound">>, _, _) ->
                                                %     {<<"Dial">>, <<"macro-dial-one">>};
                                                % application_and_context(_, <<"outbound">>, _, _) ->
                                                %     {<<"AppQueue">>, <<"from-queue">>}.


status_payload(<<"Status">>, <<"Local", _/binary>>=Channel, BridgedChannel, CallerIDNum, CallerIDName, ConnectedLineNum, ConnectedLineName
              ,ChannelState, ChannelStateDesc, <<"Dial">>, <<"macro-dial-one">>, <<"s">>, Seconds, UniqueId, BridgedUniqueId) ->
    [{<<"Event">>, <<"Status">>}
    ,{<<"Privilege">>, <<"Call">>}
    ,{<<"Channel">>, Channel}
    ,{<<"CallerIDNum">>, CallerIDNum}
    ,{<<"CallerIDName">>, CallerIDName}
    ,{<<"ConnectedLineNum">>, ConnectedLineNum}
    ,{<<"ConnectedLineName">>, ConnectedLineName}
    ,{<<"Accountcode">>, <<>>}
    ,{<<"ChannelState">>, ChannelState}
    ,{<<"ChannelStateDesc">>, ChannelStateDesc}
    ,{<<"Context">>, <<"macro-dial-one">>}
    ,{<<"Extension">>, <<"s">>}
    ,{<<"Priority">>, 12}
    ,{<<"Seconds">>, Seconds}
    ,{<<"BridgedChannel">>, BridgedChannel}
    ,{<<"BridgedUniqueId">>, BridgedUniqueId}
    ,{<<"Uniqueid">>, UniqueId}
    ];
status_payload(<<"Status">>, <<"SIP", _/binary>>=Channel, BridgedChannel, CallerIDNum, CallerIDName, ConnectedLineNum, ConnectedLineName
              ,_, ChannelStateDesc, <<"AppDial">>, <<"macro-dial-one">>, <<"s">>, _, UniqueId, BridgedUniqueId) ->
    [{<<"Event">>, <<"Status">>}
    ,{<<"Privilege">>, <<"Call">>}
    ,{<<"Channel">>, Channel}
    ,{<<"CallerIDNum">>, CallerIDNum}
    ,{<<"CallerIDName">>, CallerIDName}
    ,{<<"ConnectedLineNum">>, ConnectedLineNum}
    ,{<<"ConnectedLineName">>, ConnectedLineName}
    ,{<<"Account">>, <<>>}
    ,{<<"State">>, ChannelStateDesc}
    ,{<<"BridgedChannel">>, BridgedChannel}
    ,{<<"BridgedUniqueId">>, BridgedUniqueId}
    ,{<<"Uniqueid">>, UniqueId}
    ];
status_payload(<<"Status">>, <<"Local", _/binary>>=Channel, BridgedChannel, CallerIDNum, CallerIDName, ConnectedLineNum, ConnectedLineName
              ,_, ChannelStateDesc, <<"AppQueue">>, <<"from-queue">>, _, _, UniqueId, BridgedUniqueId) ->
    [{<<"Event">>, <<"Status">>}
    ,{<<"Privilege">>, <<"Call">>}
    ,{<<"Channel">>, Channel}
    ,{<<"CallerIDNum">>, CallerIDNum}
    ,{<<"CallerIDName">>, CallerIDName}
    ,{<<"ConnectedLineNum">>, ConnectedLineNum}
    ,{<<"ConnectedLineName">>, ConnectedLineName}
    ,{<<"Account">>, <<>>}
    ,{<<"State">>, ChannelStateDesc}
    ,{<<"BridgedChannel">>, BridgedChannel}
    ,{<<"BridgedUniqueId">>, BridgedUniqueId}
    ,{<<"Uniqueid">>, UniqueId}
    ];
status_payload(<<"Status">>, <<"SIP", _/binary>>=Channel, BridgedChannel, CallerIDNum, CallerIDName, ConnectedLineNum, ConnectedLineName
              ,ChannelState, ChannelStateDesc, <<"Queue">>, <<"ext-queues">>, Extension, Seconds, UniqueId, BridgedUniqueId) ->
    [{<<"Event">>, <<"Status">>}
    ,{<<"Privilege">>, <<"Call">>}
    ,{<<"Channel">>, Channel}
    ,{<<"CallerIDNum">>, CallerIDNum}
    ,{<<"CallerIDName">>, CallerIDName}
    ,{<<"ConnectedLineNum">>, ConnectedLineNum}
    ,{<<"ConnectedLineName">>, ConnectedLineName}
    ,{<<"Accountcode">>, <<>>}
    ,{<<"ChannelState">>, ChannelState}
    ,{<<"ChannelStateDesc">>, ChannelStateDesc}
    ,{<<"Context">>, <<"ext-queues">>}
    ,{<<"Extension">>, Extension}
    ,{<<"Priority">>, 12}
    ,{<<"Seconds">>, Seconds}
    ,{<<"BridgedChannel">>, BridgedChannel}
    ,{<<"BridgedUniqueId">>, BridgedUniqueId}
    ,{<<"Uniqueid">>, UniqueId}
    ];
status_payload(<<"Status">>, Channel, BridgedChannel, CallerIDNum, CallerIDName, ConnectedLineNum, ConnectedLineName
              ,ChannelState, ChannelStateDesc, _, Context, Extension, Seconds, UniqueId, BridgedUniqueId) ->
    [{<<"Event">>, <<"Status">>}
    ,{<<"Privilege">>, <<"Call">>}
    ,{<<"Channel">>, Channel}
    ,{<<"CallerIDNum">>, CallerIDNum}
    ,{<<"CallerIDName">>, CallerIDName}
    ,{<<"ConnectedLineNum">>, ConnectedLineNum}
    ,{<<"ConnectedLineName">>, ConnectedLineName}
    ,{<<"Accountcode">>, <<>>}
    ,{<<"ChannelState">>, ChannelState}
    ,{<<"ChannelStateDesc">>, ChannelStateDesc}
    ,{<<"Context">>, Context}
    ,{<<"Extension">>, Extension}
    ,{<<"Priority">>, 12}
    ,{<<"Seconds">>, Seconds}
    ,{<<"BridgedChannel">>, BridgedChannel}
    ,{<<"BridgedUniqueId">>, BridgedUniqueId}
    ,{<<"Uniqueid">>, UniqueId}
    ];
status_payload(<<"concise">>, Channel, BridgedChannel, _, CallerIDName, _, ConnectedLineName
              ,_, ChannelStateDesc, Application, Context, Extension, Seconds, _, _) ->
    <<Channel/binary, "!", Context/binary, "!", Extension/binary, "!1!", ChannelStateDesc/binary, "!", Application/binary
      ,"!", ConnectedLineName/binary, "!", CallerIDName/binary, "!!!3!", (kz_term:to_binary(Seconds))/binary
      ,"!", BridgedChannel/binary, "\n">>;
status_payload(<<"verbose">>, Channel, BridgedChannel, _, CallerIDName, _, ConnectedLineName
              ,_, ChannelStateDesc, Application, Context, Extension, Seconds, _, _) ->
    {H, M, S} = {Seconds div 3600, Seconds rem 3600 div 60, Seconds rem 60},
    TimeString = kz_term:to_binary(if H > 99 ->
                                           io_lib:format("~B:~2..0B:~2..0B", [H, M, S]);
                                      true ->
                                           io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S])
                                   end),

    <<(fit_list(Channel, 20))/binary, " ", (fit_list(Context, 20))/binary, " ", (fit_list(Extension, 16))/binary, " "
      ,"   1 ", (fit_list(ChannelStateDesc, 7))/binary, " ", (fit_list(Application, 12))/binary, " ", (fit_list(CallerIDName, 25))/binary
      ," ", (fit_list(ConnectedLineName, 15))/binary, " ", (fit_list(TimeString, 8))/binary, "                      "
      ,(fit_list(BridgedChannel, 20))/binary, "\n">>.

fit_list(Binary, Size) when is_binary(Binary) ->
    kz_term:to_binary(fit_list(kz_term:to_list(Binary), Size));
fit_list(List, Size) ->
    fit_list(List, length(List), Size).

fit_list(List, ListSize, Size) when ListSize > Size ->
    lists:sublist(List, Size);
fit_list(List, ListSize, Size) when ListSize =:= Size ->
    List;
fit_list(List, ListSize, Size) when ListSize < Size ->
    append_space(List, Size-ListSize).

append_space(List, Count) ->
    lists:reverse(append_space(lists:reverse(List), Count, 0)).

append_space(RevList, Count, Count) ->
    RevList;
append_space(RevList, Count, Index) ->
    append_space(" " ++ RevList, Count, Index+1).

queues_status(Props) ->
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),
    {'ok', Results} = kz_datamgr:get_results(AccountDb, <<"queues/crossbar_listing">>),

    lists:foldl(fun(Result, Acc) ->
                        QueueId = kz_json:get_value(<<"id">>, Result),
                        case kz_datamgr:open_doc(AccountDb, QueueId) of
                            {'error', E} ->
                                lager:debug("Error opening queue doc: ~p", [E]),
                                [] ++ Acc;
                            {'ok', QueueDoc} ->
                                case kz_datamgr:get_results(AccountDb, <<"callflows/queue_callflows">>, [{'key', QueueId}]) of
                                    {'error', E} ->
                                        lager:debug("Could not find queue number for queue ~p (~p)", [QueueId, E]),
                                        [] ++ Acc;
                                    {'ok', Results2} when length(Results2) =:= 1 ->
                                        Value = kz_json:get_value(<<"value">>, hd(Results2)),
                                        Number = hd(Value),

                                        RawStats = queue_stats(QueueId, AccountId),
                                        {Calls, Holdtime, TalkTime, Completed, Abandoned, AgentStats} = case RawStats of
                                                                                                            {error, E} ->
                                                                                                                lager:error("error ~p when getting queue stats for queue ~p", [E, QueueId]),
                                                                                                                {0, 0, 0, 0, 0, []};
                                                                                                            {ok, Resp} ->
                                                                                                                count_stats(Resp)
                                                                                                        end,

                                        CompletedCalls = Completed - Abandoned,
                                        AverageHold = case CompletedCalls of
                                                          0 ->
                                                              0.0;
                                                          _ ->
                                                              Holdtime / CompletedCalls
                                                      end,
                                        WaitingCalls = Calls - Completed,

                                        [[
                                          {<<"Event">>, <<"QueueParams">>},
                                          {<<"Queue">>, Number},
                                          {<<"Max">>, kz_json:get_value(<<"max_queue_size">>, QueueDoc)},
                                          {<<"Strategy">>, kz_json:get_value(<<"strategy">>, QueueDoc)},
                                          %% Calls actually represents number of waiting calls
                                          {<<"Calls">>, WaitingCalls},
                                          {<<"Holdtime">>, round(AverageHold)},
                                          {<<"TalkTime">>, TalkTime},
                                          {<<"Completed">>, CompletedCalls},
                                          {<<"Abandoned">>, Abandoned},
                                                % TODO: add servicelevel
                                          {<<"ServiceLevel">>, 60},
                                          {<<"ServicelevelPerf">>, 69.0}
                                         ]]
                                            ++ queue_entries(QueueId, Number, kz_json:get_value(<<"Waiting">>, element(2, RawStats), []))
                                            ++ agent_statuses(QueueId, AccountId, Number, AgentStats)
                                            ++ Acc;
                                    {'ok', Results2} ->
                                        lager:debug("Too many results when trying to find queue number for queue ~p: ~p", [QueueId, Results2]),
                                        [] ++ Acc
                                end
                        end
                end, [], Results).

                                                % TODO: maybe we need acdc stats to be persisted in couch
queue_stats(QueueId, AcctId) ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
            ,{<<"Queue-ID">>, QueueId}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    kapps_util:amqp_pool_request(
      Req,
      fun kapi_acdc_stats:publish_current_calls_req/1,
      fun kapi_acdc_stats:current_calls_resp_v/1
     ).

                                                % TODO: may still need to add counting of agents fails
count_stats(Stats) ->
    AllStats = kz_json:get_value(<<"Handled">>, Stats, []) ++
        kz_json:get_value(<<"Abandoned">>, Stats, []) ++
        kz_json:get_value(<<"Waiting">>, Stats, []) ++
        kz_json:get_value(<<"Processed">>, Stats, []),
    count_stats(AllStats, {0, 0, 0, 0, 0, []}).

count_stats([], {Calls, Holdtime, TalkTime, Completed, Abandoned, AgentStats}) ->
    {Calls, Holdtime, TalkTime, Completed, Abandoned, AgentStats};
count_stats([Stat|Stats], {Calls, Holdtime, TalkTime, Completed, Abandoned, AgentStats}) ->
    Status = kz_json:get_value(<<"status">>, Stat),
    AgentStats2 = if (Status =:= <<"handled">>) or (Status =:= <<"processed">>) ->
                          AgentId = kz_json:get_value(<<"agent_id">>, Stat),
                          LastCall = props:get_value(<<"LastCall">>, props:get_value(AgentId, AgentStats, []), 0),
                          StatLastCall = kz_json:get_first_defined([<<"processed_timestamp">>, <<"handled_timestamp">>], Stat, 0) - 62167219200,
                          NewLastCall = if StatLastCall > LastCall ->
                                                StatLastCall;
                                           true ->
                                                LastCall
                                        end,
                          props:set_value(AgentId, [
                                                    {<<"CallsTaken">>, props:get_value(<<"CallsTaken">>, props:get_value(AgentId, AgentStats, []), 0) + 1},
                                                    {<<"LastCall">>, NewLastCall}
                                                   ], AgentStats);
                     true ->
                          AgentStats
                  end,

    case Status of
        <<"abandoned">> ->
            WaitTime = kz_json:get_value(<<"abandoned_timestamp">>, Stat) -
                kz_json:get_value(<<"entered_timestamp">>, Stat),
            count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime, Completed+1, Abandoned+1, AgentStats2});
        <<"waiting">> ->
                                                % TODO: updated the calculation for wait can call time
            WaitTime = kz_time:current_tstamp() - kz_json:get_value(<<"entered_timestamp">>, Stat),
            count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime, Completed, Abandoned, AgentStats2});
        <<"handled">> ->
            WaitTime = kz_json:get_value(<<"handled_timestamp">>, Stat) -
                kz_json:get_value(<<"entered_timestamp">>, Stat),
            CallTime = kz_time:current_tstamp() - kz_json:get_value(<<"handled_timestamp">>, Stat),
            count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime+CallTime, Completed+1, Abandoned, AgentStats2});
        <<"processed">> ->
            case kz_json:get_value(<<"abandoned_timestamp">>, Stat) of
                undefined ->
                    WaitTime = kz_json:get_value(<<"handled_timestamp">>, Stat) -
                        kz_json:get_value(<<"entered_timestamp">>, Stat),
                    CallTime = kz_json:get_value(<<"processed_timestamp">>, Stat) -
                        kz_json:get_value(<<"handled_timestamp">>, Stat),
                    count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime+CallTime, Completed+1, Abandoned, AgentStats2});
                _ ->
                    WaitTime = kz_json:get_value(<<"abandoned_timestamp">>, Stat) -
                        kz_json:get_value(<<"entered_timestamp">>, Stat),
                    count_stats(Stats, {Calls+1, Holdtime+WaitTime, TalkTime, Completed+1, Abandoned+1, AgentStats2})
            end
    end.

queue_entries(QueueId, Number, WaitingCalls) ->
    CallIds = ami_sm:queue_calls(QueueId),
    lists:foldl(fun(CallId, Acc) ->
                        Call = ami_sm:call(CallId),
                        case Call of
                            'undefined' -> Acc;
                            _ ->
                                [queue_entry(Call, Number, waiting_call_stat(CallId, WaitingCalls)) | Acc]
                        end
                end, [], CallIds).

queue_entry(Call, Number, WaitingCallStat) ->
    %% TODO fixed entered timestamp for other acdc nodes
    [{<<"Event">>, <<"QueueEntry">>}
    ,{<<"Queue">>, Number}
    ,{<<"Position">>, ami_sm:queue_pos(amimulator_call:acdc_queue_id(Call), amimulator_call:call_id(Call))}
    ,{<<"Channel">>, amimulator_call:channel(Call)}
    ,{<<"CallerID">>, amimulator_call:id_number(Call)}
    ,{<<"CallerIDName">>, amimulator_call:id_name(Call)}
    ,{<<"Wait">>, kz_time:current_tstamp() - kz_json:get_value(<<"entered_timestamp">>, WaitingCallStat, 0)}
    ].

waiting_call_stat(CallId, []) ->
    lager:debug("could not find waiting call for id ~p", [CallId]),
    'undefined';
waiting_call_stat(CallId, [JObj|JObjs]) ->
    case kz_json:get_value(<<"call_id">>, JObj) of
        CallId -> JObj;
        _ -> waiting_call_stat(CallId, JObjs)
    end.

agent_statuses(QueueId, AccountId, Number, AgentStats) ->
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    {ok, Results} = kz_datamgr:get_results(AccountDb, <<"queues/agents_listing">>, [{key, QueueId}]),
    lists:foldl(fun(Result, Acc) ->
                        AgentId = kz_json:get_value(<<"id">>, Result),
                        case agent_status(AgentId, AccountId, Number, AgentStats) of
                            'logged_out' -> Acc;
                            AgentStatus -> [AgentStatus | Acc]
                        end
                end, [], Results).

agent_status(AgentId, AccountId, Number, AgentStats) ->
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    {ok, UserDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    FirstName = kz_json:get_value(<<"first_name">>, UserDoc),
    LastName = kz_json:get_value(<<"last_name">>, UserDoc),
    Username = kz_json:get_value(<<"username">>, UserDoc),

    {'ok', Status} = acdc_agent_util:most_recent_status(AccountId, AgentId),

    case Status of
        <<"logged_out">> -> 'logged_out';
        _ ->
                                                % TODO: properly assigned paused based on status
            Paused = case Status of
                         <<"paused">> ->
                             1;
                         _ ->
                             0
                     end,

            [
             {<<"Event">>, <<"QueueMember">>},
             {<<"Queue">>, Number},
             {<<"Name">>, <<FirstName/binary, " ", LastName/binary>>},
             {<<"Location">>, <<"Local/", Username/binary, "@from-queue/n">>},
             %% Membership static is also possible
             {<<"Membership">>, <<"dynamic">>},
             {<<"Penalty">>, 0},
             {<<"CallsTaken">>, props:get_value(<<"CallsTaken">>, props:get_value(AgentId, AgentStats, []), 0)},
             {<<"LastCall">>, props:get_value(<<"LastCall">>, props:get_value(AgentId, AgentStats, []), 0)},
             {<<"Status">>, translate_status(Status)},
             {<<"Paused">>, Paused}
            ]
    end.

translate_status(Status) ->
                                                % TODO: properly translate statuses
    case Status of
        <<"ready">> ->
            1;
        <<"logged_in">> ->
            1;
        <<"connected">> ->
            2;
        <<"outbound">> ->
            2;
        <<"logged_out">> ->
            5;
        <<"paused">> ->
            5;
        <<"wrapup">> ->
            1;
        <<"connecting">> ->
            6;
        _ ->
            lager:debug("unspecified status ~p", [Status]),
            5
    end.

sip_peers(Props) ->
    AccountId = props:get_value(<<"AccountId">>, Props),
    AccountDb = props:get_value(<<"AccountDb">>, Props),
    ActionId = props:get_value(<<"ActionID">>, Props),
    {'ok', Results} = kz_datamgr:get_results(AccountDb, <<"devices/listing_by_owner">>),
    lists:foldl(fun(Result, Registrations) ->
                        Value = kz_json:get_value(<<"value">>, Result),
                        case kz_json:get_value(<<"key">>, Result) of
                            'null' ->
                                [reg_entry(AccountId, kz_json:get_value(<<"id">>, Value), kz_json:get_value(<<"name">>, Value), ActionId)] ++ Registrations;
                            OwnerId ->
                                case kz_datamgr:open_doc(AccountDb, OwnerId) of
                                    {'error', 'not_found'} ->
                                        lager:debug("Missing owner ~p for endpoint with username ~p", [OwnerId, kz_json:get_value(<<"name">>, Value)]),
                                        Registrations;
                                    {'ok', Endpoint} ->
                                        [reg_entry(AccountId, kz_json:get_value(<<"id">>, Value), kz_json:get_value(<<"username">>, Endpoint), ActionId)] ++ Registrations
                                end
                        end
                end, [], Results).

reg_entry(AccountId, EndpointId, EndpointName, ActionId) ->
    case ami_sm:registration(AccountId, EndpointId) of
        'not_registered' ->
            props:filter_undefined([
                                    {<<"Event">>, <<"PeerEntry">>},
                                    {<<"ActionID">>, ActionId},
                                    {<<"Channeltype">>, <<"SIP">>},
                                    {<<"ObjectName">>, EndpointName},
                                    {<<"ChanObjectType">>, <<"peer">>},
                                    {<<"IPaddress">>, <<"-none-">>},
                                    {<<"IPport">>, 0},
                                    {<<"Dynamic">>, <<"yes">>},
                                    {<<"Forcerport">>, <<"no">>},
                                    {<<"VideoSupport">>, <<"no">>},
                                    {<<"TextSupport">>, <<"no">>},
                                    {<<"ACL">>, <<"yes">>},
                                    {<<"Status">>, <<"UNKNOWN">>},
                                    {<<"RealtimeDevice">>, <<"no">>}
                                   ]);
        RegProps ->
            props:filter_undefined([
                                    {<<"Event">>, <<"PeerEntry">>},
                                    {<<"ActionID">>, ActionId},
                                    {<<"Channeltype">>, <<"SIP">>},
                                    {<<"ObjectName">>, EndpointName},
                                    {<<"ChanObjectType">>, <<"peer">>},
                                    {<<"IPaddress">>, props:get_value(<<"IP">>, RegProps)},
                                    {<<"IPport">>, props:get_value(<<"Port">>, RegProps)},
                                    {<<"Dynamic">>, <<"yes">>},
                                    {<<"Forcerport">>, <<"no">>},
                                    {<<"VideoSupport">>, <<"no">>},
                                    {<<"TextSupport">>, <<"no">>},
                                    {<<"ACL">>, <<"yes">>},
                                    {<<"Status">>, <<"OK (1 ms)">>},
                                    {<<"RealtimeDevice">>, <<"no">>}
                                   ])
    end.

mailbox_count_error(ActionId, Mailbox) ->
    [
     {<<"Response">>, <<"Success">>},
     {<<"ActionID">>, ActionId},
     {<<"Message">>, <<"Mailbox Message Count">>},
     {<<"Mailbox">>, Mailbox},
     {<<"UrgMessages">>, 0},
     {<<"NewMessages">>, 0},
     {<<"OldMessages">>, 0}
    ].

queue_add(Props) ->
    Interface = proplists:get_value(<<"Interface">>, Props),
    Exten = hd(binary:split(binary:replace(Interface, <<"Local/">>, <<"">>), <<"@">>)),
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),

    %% Load agent doc having the Exten given as name
    case kz_datamgr:get_results(
           AccountDb,
           <<"users/list_by_username">>,
           [{key, Exten}]
          ) of
        {ok, [Result]} ->
            queue_add(AccountId, AccountDb, Result, Props);
        _ ->
            %% Error, could not find user
            {error, not_a_user}
    end.

queue_add(AccountId, AccountDb, Result, Props) ->
    AgentId = kz_json:get_value(<<"id">>, Result),
    {ok, QueueDoc} = amimulator_util:queue_for_number(proplists:get_value(<<"Queue">>, Props), AccountDb),
    QueueId = kz_json:get_value(<<"_id">>, QueueDoc),

    {ok, AgentDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    kz_datamgr:save_doc(AccountDb, cb_queues:maybe_add_queue_to_agent(QueueId, AgentDoc)),

    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Queue-ID">>, QueueId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapi_acdc_agent:publish_login_queue(Prop),

    Payload = [
               {<<"Response">>, <<"Success">>},
               {<<"Message">>, <<"Added interface to queue">>}
              ],
    {ok, {Payload, n}}.

queue_remove(Props) ->
    Interface = proplists:get_value(<<"Interface">>, Props),
    Exten = hd(binary:split(binary:replace(Interface, <<"Local/">>, <<"">>), <<"@">>)),
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),

    %% Load agent doc having the Exten given as name
    {ok, [Result]} = kz_datamgr:get_results(AccountDb,
                                            <<"users/list_by_username">>,
                                            [{key, Exten}]
                                           ),
    AgentId = kz_json:get_value(<<"id">>, Result),
    {ok, QueueDoc} = amimulator_util:queue_for_number(proplists:get_value(<<"Queue">>, Props), AccountDb),
    QueueId = kz_json:get_value(<<"_id">>, QueueDoc),

    {ok, AgentDoc} = kz_datamgr:open_doc(AccountDb, AgentId),
    kz_datamgr:save_doc(AccountDb, cb_queues:maybe_rm_queue_from_agent(QueueId, AgentDoc)),

    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Queue-ID">>, QueueId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapi_acdc_agent:publish_logout_queue(Prop),

    Payload = [
               {<<"Response">>, <<"Success">>},
               {<<"Message">>, <<"Removed interface from queue">>}
              ],
    {ok, {Payload, n}}.

queue_pause(Props) ->
    Interface = proplists:get_value(<<"Interface">>, Props),
    Exten = hd(binary:split(binary:replace(Interface, <<"Local/">>, <<"">>), <<"@">>)),
    AccountId = proplists:get_value(<<"AccountId">>, Props),
    AccountDb = proplists:get_value(<<"AccountDb">>, Props),

    %% Load agent doc having the Exten given as name
    case kz_datamgr:get_results(
           AccountDb,
           <<"users/list_by_username">>,
           [{key, Exten}]
          ) of
        {ok, [Result]} ->
            pause_exten(AccountId, Result, Props);
        _ ->
            %% Error, could not find user
            {error, not_a_user}
    end.

pause_exten(AccountId, Result, Props) ->
    AgentId = kz_json:get_value(<<"id">>, Result),

    case props:get_value(<<"Paused">>, Props) of
        <<"0">> ->
            Prop = props:filter_undefined(
                     [{<<"Account-ID">>, AccountId}
                     ,{<<"Agent-ID">>, AgentId}
                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                     ]),
            kapi_acdc_agent:publish_resume(Prop),

            Payload = [
                       {<<"Response">>, <<"Success">>},
                       {<<"Message">>, <<"Interface unpaused successfully">>}
                      ],
            {ok, {Payload, n}};
        <<"1">> ->
            Prop = props:filter_undefined(
                     [{<<"Account-ID">>, AccountId}
                     ,{<<"Agent-ID">>, AgentId}
                     ,{<<"Time-Limit">>, 600000}
                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                     ]),
            kapi_acdc_agent:publish_pause(Prop),

            Payload = [
                       {<<"Response">>, <<"Success">>},
                       {<<"Message">>, <<"Interface paused successfully">>}
                      ],
            {ok, {Payload, n}}
    end.

getvar(<<"CDR(dst)">>, Props) ->
    Channel = props:get_value(<<"Channel">>, Props),
    CallIds = ami_sm:channel_call_ids(Channel),
    case CallIds of
        %% The call id is undefined if the channel is not a sip peer
        [] ->
            undefined;
        _ ->
            lager:debug("channel ~p", [props:get_value(<<"Channel">>, Props)]),
            CallId = hd(CallIds),
            lager:debug("callid ~p", [CallId]),
            Call = ami_sm:call(CallId),

            [
             {<<"Response">>, <<"Success">>},
             {<<"Variable">>, props:get_value(<<"Variable">>, Props)},
             {<<"Value">>, amimulator_call:other_id_name(Call)},
             {<<"ActionID">>, props:get_value(<<"ActionID">>, Props)}
            ]
    end;
getvar(<<"AGENTBYCALLERID_", _CallerId/binary>>=Variable, _Props) ->
    {[
      <<"Response: Success\r\n">>,
      <<"Variable: ", Variable/binary, "\r\nValue: \r\n\r\n">>
     ], raw};
getvar(<<"EPOCH">>=Variable, _Props) ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    Timestamp = kz_term:to_binary(MegaSecs * 1000000 + Secs),
    {[
      <<"Response: Success\r\n">>,
      <<"Variable: ", Variable/binary, "\r\nValue: ", Timestamp/binary, "\r\n\r\n">>
     ], raw};
getvar(Variable, Props) ->
    lager:debug("Unhandled getvar ~p with props ~p", [Variable, Props]),
    undefined.

command(<<"meetme list ", MeetMeSpec/binary>>, Props) ->
    [Number, _Mode] = binary:split(MeetMeSpec, <<" ">>),
    maybe_list_conf(Number, Props);
command(<<"meetme mute ", MeetMeSpec/binary>>, Props) ->
    [Number, ParticipantId] = binary:split(MeetMeSpec, <<" ">>),
    mute_conf(Number, ParticipantId, Props);
command(<<"meetme unmute ", MeetMeSpec/binary>>, Props) ->
    [Number, ParticipantId] = binary:split(MeetMeSpec, <<" ">>),
    unmute_conf(Number, ParticipantId, Props);
command(<<"meetme kick ", MeetMeSpec/binary>>, Props) ->
    [Number, ParticipantId] = binary:split(MeetMeSpec, <<" ">>),
    kick_conf(Number, ParticipantId, Props);
command(<<"core show channels ", Verbosity/binary>>, Props) ->
    initial_channel_status(ami_sm:calls(proplists:get_value(<<"AccountId">>, Props)), Props, Verbosity);
command(<<"database put ", Variable/binary>>, Props) ->
    [Family, Key, Value] = parse_command(kz_term:to_list(Variable)),
    ami_sm:db_put(props:get_value(<<"AccountId">>, Props), Family, Key, Value),

    {[
      <<"Response: Follows\r\nPrivilege: Command\r\n">>,
      <<"Updated database successfully\n">>,
      <<"--END COMMAND--\r\n\r\n">>
     ], raw};
command(<<"database del ", Variable/binary>>, Props) ->
    [Family, Key] = parse_command(kz_term:to_list(Variable)),
    ami_sm:db_del(props:get_value(<<"AccountId">>, Props), Family, Key),

    {[
      <<"Response: Follows\r\nPrivilege: Command\r\n">>,
      <<"Database entry removed.\n">>,
      <<"--END COMMAND--\r\n\r\n">>
     ], raw};
command(CommandName, Props) ->
    lager:debug("Unhandled command ~p with props ~p", [CommandName, Props]),
    {[
      <<"Response: Follows\r\nPrivilege: Command\r\n">>,
      <<"No such command '", CommandName/binary, "' (type 'core show help ",
        CommandName/binary, "' for other possible commands)\n">>,
      <<"--END COMMAND--\r\n\r\n">>
     ], raw}.

-spec parse_command(list()) -> list().
parse_command(Command) ->
    parse_command(Command, [], []).

parse_command([], Parts, _Acc) ->
    lists:reverse(Parts);
parse_command([Char|Chars], Parts, Acc) ->
    if Char =:= 34 ->
            parse_command(quoted, Chars, Parts, Acc);
       Char =:= 32 ->
            parse_command(Chars, [lists:reverse(Acc)] ++ Parts, []);
       true ->
            parse_command(Chars, Parts, [Char | Acc])
    end.

parse_command(quoted, [], Parts, _Acc) ->
    lists:reverse(Parts);
parse_command(quoted, [Char|Chars], Parts, Acc) ->
    if Char =:= 34 ->
            parse_command(Chars, [lists:reverse(Acc)] ++ Parts, []);
       true ->
            parse_command(quoted, Chars, Parts, [Char | Acc])
    end.

user_event(<<"MEETME-REFRESH">>, Props) ->
    [[
      {<<"Response">>, <<"Success">>},
      {<<"Message">>, <<"Event Sent">>}
     ] ++ [
           {<<"Event">>, <<"UserEvent">>},
           {<<"Privilege">>, <<"user,all">>},
           {<<"UserEvent">>, <<"MEETME-REFRESH">>},
           {<<"Action">>, <<"UserEvent">>},
           {<<"Meetme">>, props:get_value(<<"Meetme">>, Props)}
          ]];
user_event(<<"FOP2ASTDB">>, Props) ->
    [[
      {<<"Response">>, <<"Success">>},
      {<<"Message">>, <<"Event Sent">>}
     ] ++ [
           {<<"Event">>, <<"UserEvent">>},
           {<<"Privilege">>, <<"user,all">>},
           {<<"UserEvent">>, <<"FOP2ASTDB">>},
           {<<"Action">>, <<"UserEvent">>},
           {<<"Family">>, props:get_value(<<"Family">>, Props)},
           {<<"Key">>, props:get_value(<<"Key">>, Props)},
           {<<"Channel">>, props:get_value(<<"Channel">>, Props)},
           {<<"Value">>, props:get_value(<<"Value">>, Props)}
          ]];
user_event(<<"FOP2CHAT">>, Props) ->
    [[
      {<<"Response">>, <<"Success">>},
      {<<"Message">>, <<"Event Sent">>}
     ] ++ [
           {<<"Event">>, <<"UserEvent">>},
           {<<"Privilege">>, <<"user,all">>},
           {<<"UserEvent">>, <<"FOP2CHAT">>},
           {<<"Action">>, <<"UserEvent">>},
           {<<"From">>, props:get_value(<<"From">>, Props)},
           {<<"Channel">>, props:get_value(<<"Channel">>, Props)},
           {<<"Msg">>, props:get_value(<<"Msg">>, Props)}
          ]];
user_event(<<"FOP2NOTIFY">>, Props) ->
    [[
      {<<"Response">>, <<"Success">>},
      {<<"Message">>, <<"Event Sent">>}
     ] ++ [
           {<<"Event">>, <<"UserEvent">>},
           {<<"Privilege">>, <<"user,all">>},
           {<<"UserEvent">>, <<"FOP2NOTIFY">>},
           {<<"Action">>, <<"UserEvent">>},
           {<<"From">>, props:get_value(<<"From">>, Props)},
           {<<"Channel">>, props:get_value(<<"Channel">>, Props)},
           {<<"Msg">>, props:get_value(<<"Msg">>, Props)}
          ]];
user_event(EventName, Props) ->
    lager:debug("Unhandled event ~p with props ~p", [EventName, Props]),
    undefined.

maybe_list_conf(Number, Props) ->
    case conf_id_for_number(Number, props:get_value(<<"AccountId">>, Props)) of
        'undefined' -> 'undefined';
        ConfId ->
            ConfDetails = conf_details(ConfId),
            case kz_json:is_empty(ConfDetails) of
                'true' -> 'undefined';
                'false' ->
                    participant_payloads(kz_json:get_value(<<"Participants">>, ConfDetails)
                                        ,kz_json:get_value(<<"Run-Time">>, ConfDetails)
                                        ,props:get_value(<<"ActionID">>, Props)
                                        )
            end
    end.

mute_conf(Number, ParticipantId, Props) when is_binary(ParticipantId) ->
    mute_conf(Number, list_to_integer(binary_to_list(ParticipantId)), Props);
mute_conf(Number, ParticipantId, Props) ->
    case conf_id_for_number(Number, props:get_value(<<"AccountId">>, Props)) of
        'undefined' -> 'ok';
        ConfId ->
            Conference = kapps_conference:set_id(ConfId, kapps_conference:new()),
            kapps_conference_command:mute_participant(ParticipantId, Conference),
            ConfDetails = conf_details(ConfId),
            CallId = participant_call_id(ParticipantId, kz_json:get_value(<<"Participants">>, ConfDetails)),
            Call = ami_sm:call(CallId),
            [{<<"Event">>, <<"MeetmeMute">>}
            ,{<<"Privilege">>, <<"call,all">>}
            ,{<<"Channel">>, amimulator_call:channel(Call)}
            ,{<<"Uniqueid">>, CallId}
            ,{<<"Meetme">>, Number}
            ,{<<"Usernum">>, ParticipantId}
            ,{<<"Status">>, <<"on">>}]
    end.

unmute_conf(Number, ParticipantId, Props) when is_binary(ParticipantId) ->
    unmute_conf(Number, list_to_integer(binary_to_list(ParticipantId)), Props);
unmute_conf(Number, ParticipantId, Props) ->
    case conf_id_for_number(Number, props:get_value(<<"AccountId">>, Props)) of
        'undefined' -> 'ok';
        ConfId ->
            Conference = kapps_conference:set_id(ConfId, kapps_conference:new()),
            kapps_conference_command:unmute_participant(ParticipantId, Conference),
            ConfDetails = conf_details(ConfId),
            CallId = participant_call_id(ParticipantId, kz_json:get_value(<<"Participants">>, ConfDetails)),
            Call = ami_sm:call(CallId),
            [{<<"Event">>, <<"MeetmeMute">>}
            ,{<<"Privilege">>, <<"call,all">>}
            ,{<<"Channel">>, amimulator_call:channel(Call)}
            ,{<<"Uniqueid">>, CallId}
            ,{<<"Meetme">>, Number}
            ,{<<"Usernum">>, ParticipantId}
            ,{<<"Status">>, <<"off">>}]
    end.

kick_conf(Number, ParticipantId, Props) when is_binary(ParticipantId) ->
    kick_conf(Number, list_to_integer(binary_to_list(ParticipantId)), Props);
kick_conf(Number, ParticipantId, Props) ->
    case conf_id_for_number(Number, props:get_value(<<"AccountId">>, Props)) of
        'undefined' -> 'ok';
        ConfId ->
            ConfDetails = conf_details(ConfId),
            CallId = participant_call_id(ParticipantId, kz_json:get_value(<<"Participants">>, ConfDetails)),
            Payload = amimulator_util:maybe_leave_conference(CallId),
            Conference = kapps_conference:set_id(ConfId, kapps_conference:new()),
            kapps_conference_command:kick(ParticipantId, Conference),
            Payload
    end.

conf_id_for_number(Number, AccountId) ->
    case kz_datamgr:get_results(kz_util:format_account_id(AccountId, 'encoded'), <<"conferences/conference_map">>) of
        {'ok', Results} -> conf_id_for_number(Number, AccountId, Results);
        {'error', E} ->
            lager:debug("couldn't read couch view 'conferences/conference_map' (~p)", [E]),
            'undefined'
    end.

conf_id_for_number(_, _, []) ->
    'undefined';
conf_id_for_number(Number, AccountId, [Result|Results]) ->
    case kz_json:get_value([<<"value">>, <<"numbers">>], Result) of
        [_|_]=Numbers ->
            case lists:member(Number, Numbers) of
                'true' -> kz_json:get_value(<<"key">>, Result);
                'false' -> conf_id_for_number(Number, AccountId, Results)
            end;
        _ -> conf_id_for_number(Number, AccountId, Results)
    end.

conf_details(ConfId) ->
    Req = props:filter_undefined([
                                  {<<"Conference-ID">>, ConfId}
                                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    case kapps_util:amqp_pool_collect(
           Req,
           fun kapi_conference:publish_search_req/1,
           {'ecallmgr', 'true'}
          ) of
        {'error', E} ->
            lager:debug("conf_details error ~p", [E]),
            'undefined';
        {'ok', JObjs} -> merge_conference_resps(JObjs)
    end.

merge_conference_resps(JObjs) ->
    merge_conference_resps(JObjs, kz_json:new()).

merge_conference_resps([], Acc) ->
    Acc;
merge_conference_resps([JObj|JObjs], Acc) ->
    case kz_json:get_value(<<"Participants">>, JObj) of
        'undefined' -> merge_conference_resps(JObjs, Acc);
        Participants ->
            Participants2 = lists:foldl(fun(Participant, Acc2) ->
                                                [Participant | delete_participant(kz_json:get_value(<<"Call-ID">>, Participant), Acc2)]
                                        end, kz_json:get_value(<<"Participants">>, Acc, []), Participants),
            kz_json:set_values([{<<"Participants">>, Participants2}
                               ,{<<"Run-Time">>, kz_json:get_value(<<"Run-Time">>, JObj, kz_json:get_value(<<"Run-Time">>, Acc))}
                               ], Acc)
    end.

delete_participant(CallId, Participants) ->
    delete_participant(CallId, Participants, []).

delete_participant(_, [], Acc) ->
    Acc;
delete_participant(CallId, [Participant|Participants], Acc) ->
    case kz_json:get_value(<<"Call-ID">>, Participant) of
        CallId -> delete_participant(CallId, Participants, Acc);
        _ -> delete_participant(CallId, Participants, [Participant | Acc])
    end.

participant_call_id(_, []) ->
    'undefined';
participant_call_id(ParticipantId, [Participant|Participants]) ->
    case kz_json:get_value(<<"Participant-ID">>, Participant) of
        ParticipantId -> kz_json:get_value(<<"Call-ID">>, Participant);
        _ -> participant_call_id(ParticipantId, Participants)
    end.

participant_payloads(Participants, RunTime, ActionId) ->
    {H, M, S} = {RunTime div 3600, RunTime rem 3600 div 60, RunTime rem 60},
    TimeString = kz_term:to_binary(if H > 99 ->
                                           io_lib:format("~B:~2..0B:~2..0B", [H, M, S]);
                                      true ->
                                           io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S])
                                   end),

    {[
      <<"Response: Follows\r\nPrivilege: Command\r\n">>,
      <<"ActionID: ", ActionId/binary, "\r\n">>
     ] ++ lists:foldl(fun(Participant, Payloads) ->
                              CallId = kz_json:get_value(<<"Call-ID">>, Participant),
                              Call = ami_sm:call(CallId),

                              ParticipantId = kz_term:to_binary(kz_json:get_value(<<"Participant-ID">>, Participant)),
                                                %ParticipantId = <<"1">>,
                              CallerId = amimulator_call:id_name(Call),
                              EndpointName = amimulator_call:channel(Call),

                              [
                               <<ParticipantId/binary, "!!", CallerId/binary, "!",
                                 EndpointName/binary, "!!!!!-", ParticipantId/binary, "!", TimeString/binary, "\n">>
                              ] ++ Payloads
                      end, [], Participants) ++
         [
          <<"--END COMMAND--\r\n\r\n">>
         ], raw}.

list_commands(Props) ->
    Payload = [{<<"Response">>, <<"Success">>}
              ,{<<"ActionID">>, props:get_value(<<"ActionID">>, Props)}
                                                %               ,{<<"WaitEvent">>, <<"Wait for an event to occur.  (Priv: <none>)">>}
                                                %               ,{<<"QueueReset">>, <<"Reset queue statistics.  (Priv: <none>)">>}
                                                %               ,{<<"QueueReload">>, <<"Reload a queue, queues, or any sub-section of a queue or queues.  (Priv: <none>)">>}
                                                %               ,{<<"QueueRule">>, <<"Queue Rules.  (Priv: <none>)">>}
                                                %               ,{<<"QueuePenalty">>, <<"Set the penalty for a queue member.  (Priv: agent,all)">>}
                                                %               ,{<<"QueueLog">>, <<"Adds custom entry in queue_log.  (Priv: agent,all)">>}
                                                %               ,{<<"QueuePause">>, <<"Makes a queue member temporarily unavailable.  (Priv: agent,all)">>}
                                                %               ,{<<"QueueRemove">>, <<"Remove interface from queue.  (Priv: agent,all)">>}
                                                %               ,{<<"QueueAdd">>, <<"Add interface to queue.  (Priv: agent,all)">>}
                                                %               ,{<<"QueueSummary">>, <<"Show queue summary.  (Priv: <none>)">>}
                                                %               ,{<<"QueueStatus">>, <<"Show queue status.  (Priv: <none>)">>}
                                                %               ,{<<"Queues">>, <<"Queues.  (Priv: <none>)">>}
                                                %               ,{<<"VoicemailUsersList">>, <<"List All Voicemail User Information.  (Priv: call,reporting,all)">>}
              ,{<<"PlayDTMF">>, <<"Play DTMF signal on a specific channel.  (Priv: call,all)">>}
                                                %               ,{<<"MixMonitorMute">>, <<"Mute / unMute a Mixmonitor recording.  (Priv: <none>)">>}
                                                %               ,{<<"MuteAudio">>, <<"Mute an audio stream (Priv: system,all)">>}
                                                %               ,{<<"MeetmeList">>, <<"List participants in a conference.  (Priv: reporting,all)">>}
                                                %               ,{<<"MeetmeUnmute">>, <<"Unmute a Meetme user.  (Priv: call,all)">>}
                                                %               ,{<<"MeetmeMute">>, <<"Mute a Meetme user.  (Priv: call,all)">>}
              ,{<<"SIPnotify">>, <<"Send a SIP notify.  (Priv: system,all)">>}
                                                %               ,{<<"SIPshowregistry">>, <<"Show SIP registrations (text format).  (Priv: system,reporting,all)">>}
                                                %               ,{<<"SIPqualifypeer">>, <<"Qualify SIP peers.  (Priv: system,reporting,all)">>}
              ,{<<"SIPshowpeer">>, <<"show SIP peer (text format).  (Priv: system,reporting,all)">>}
                                                %               ,{<<"SIPpeers">>, <<"List SIP peers (text format).  (Priv: system,reporting,all)">>}
                                                %               ,{<<"DAHDIRestart">>, <<"Fully Restart DAHDI channels (terminates calls).  (Priv: <none>)">>}
                                                %               ,{<<"DAHDIShowChannels">>, <<"Show status of DAHDI channels.  (Priv: <none>)">>}
                                                %               ,{<<"DAHDIDNDoff">>, <<"Toggle DAHDI channel Do Not Disturb status OFF.  (Priv: <none>)">>}
                                                %               ,{<<"DAHDIDNDon">>, <<"Toggle DAHDI channel Do Not Disturb status ON.  (Priv: <none>)">>}
                                                %               ,{<<"DAHDIDialOffhook">>, <<"Dial over DAHDI channel while offhook.  (Priv: <none>)">>}
                                                %               ,{<<"DAHDIHangup">>, <<"Hangup DAHDI Channel.  (Priv: <none>)">>}
                                                %               ,{<<"DAHDITransfer">>, <<"Transfer DAHDI Channel.  (Priv: <none>)">>}
                                                %               ,{<<"IAXregistry">>, <<"Show IAX registrations.  (Priv: system,reporting,all)">>}
                                                %               ,{<<"IAXnetstats">>, <<"Show IAX Netstats.  (Priv: system,reporting,all)">>}
                                                %               ,{<<"IAXpeerlist">>, <<"List IAX Peers.  (Priv: system,reporting,all)">>}
                                                %               ,{<<"IAXpeers">>, <<"List IAX peers.  (Priv: system,reporting,all)">>}
                                                %               ,{<<"UnpauseMonitor">>, <<"Unpause monitoring of a channel.  (Priv: call,all)">>}
                                                %               ,{<<"PauseMonitor">>, <<"Pause monitoring of a channel.  (Priv: call,all)">>}
                                                %               ,{<<"ChangeMonitor">>, <<"Change monitoring filename of a channel.  (Priv: call,all)">>}
                                                %               ,{<<"StopMonitor">>, <<"Stop monitoring a channel.  (Priv: call,all)">>}
                                                %               ,{<<"Monitor">>, <<"Monitor a channel.  (Priv: call,all)">>}
                                                %               ,{<<"DBDelTree">>, <<"Delete DB Tree.  (Priv: system,all)">>}
                                                %               ,{<<"DBDel">>, <<"Delete DB entry.  (Priv: system,all)">>}
                                                %               ,{<<"DBPut">>, <<"Put DB entry.  (Priv: system,all)">>}
                                                %               ,{<<"DBGet">>, <<"Get DB Entry.  (Priv: system,reporting,all)">>}
                                                %               ,{<<"Bridge">>, <<"Bridge two channels already in the PBX.  (Priv: call,all)">>}
                                                %               ,{<<"Park">>, <<"Park a channel.  (Priv: call,all)">>}
                                                %               ,{<<"ParkedCalls">>, <<"List parked calls.  (Priv: <none>)">>}
                                                %               ,{<<"ShowDialPlan">>, <<"Show dialplan contexts and extensions  (Priv: config,reporting,all)">>}
                                                %               ,{<<"ModuleCheck">>, <<"Check if module is loaded.  (Priv: system,all)">>}
                                                %               ,{<<"ModuleLoad">>, <<"Module management.  (Priv: system,all)">>}
                                                %               ,{<<"CoreShowChannels">>, <<"List currently active channels.  (Priv: system,reporting,all)">>}
                                                %               ,{<<"Reload">>, <<"Send a reload event.  (Priv: system,config,all)">>}
                                                %               ,{<<"CoreStatus">>, <<"Show PBX core status variables.  (Priv: system,reporting,all)">>}
                                                %               ,{<<"CoreSettings">>, <<"Show PBX core settings (version etc).  (Priv: system,reporting,all)">>}
                                                %               ,{<<"UserEvent">>, <<"Send an arbitrary event.  (Priv: user,all)">>}
                                                %               ,{<<"UpdateConfig">>, <<"Update basic configuration.  (Priv: config,all)">>}
                                                %               ,{<<"SendText">>, <<"Send text message to channel.  (Priv: call,all)">>}
                                                %               ,{<<"ListCommands">>, <<"List available manager commands.  (Priv: <none>)">>}
                                                %               ,{<<"MailboxCount">>, <<"Check Mailbox Message Count.  (Priv: call,reporting,all)">>}
                                                %               ,{<<"MailboxStatus">>, <<"Check mailbox.  (Priv: call,reporting,all)">>}
                                                %               ,{<<"AbsoluteTimeout">>, <<"Set absolute timeout.  (Priv: system,call,all)">>}
                                                %               ,{<<"ExtensionState">>, <<"Check Extension Status.  (Priv: call,reporting,all)">>}
                                                %               ,{<<"Command">>, <<"Execute Asterisk CLI Command.  (Priv: command,all)">>}
              ,{<<"Originate">>, <<"Originate a call.  (Priv: originate,all)">>}
                                                %               ,{<<"Atxfer">>, <<"Attended transfer.  (Priv: call,all)">>}
              ,{<<"Redirect">>, <<"Redirect (transfer) a call.  (Priv: call,all)">>}
                                                %               ,{<<"ListCategories">>, <<"List categories in configuration file.  (Priv: config,all)">>}
                                                %               ,{<<"CreateConfig">>, <<"Creates an empty file in the configuration directory.  (Priv: config,all)">>}
              ,{<<"Status">>, <<"List channel status.  (Priv: system,call,reporting,all)">>}
                                                %               ,{<<"GetConfigJSON">>, <<"Retrieve configuration (JSON format).  (Priv: system,config,all)">>}
                                                %               ,{<<"GetConfig">>, <<"Retrieve configuration.  (Priv: system,config,all)">>}
                                                %               ,{<<"Getvar">>, <<"Gets a channel variable.  (Priv: call,reporting,all)">>}
                                                %               ,{<<"Setvar">>, <<"Set a channel variable.  (Priv: call,all)">>}
                                                %               ,{<<"Ping">>, <<"Keepalive command.  (Priv: <none>)">>}
              ,{<<"Hangup">>, <<"Hangup channel.  (Priv: system,call,all)">>}
                                                %               ,{<<"Challenge">>, <<"Generate Challenge for MD5 Auth.  (Priv: <none>)">>}
                                                %               ,{<<"Login">>, <<"Login Manager.  (Priv: <none>)">>}
                                                %               ,{<<"Logoff">>, <<"Logoff Manager.  (Priv: <none>)">>}
                                                %               ,{<<"Events">>, <<"Control Event Flow.  (Priv: <none>)">>}
                                                %               ,{<<"LocalOptimizeAway">>, <<"Optimize away a local channel when possible.  (Priv: system,call,all)">>}
                                                %               ,{<<"DataGet">>, <<"Retrieve the data api tree.  (Priv: <none>)">>}
              ],
    {'ok', {props:filter_undefined(Payload), 'n'}}.
