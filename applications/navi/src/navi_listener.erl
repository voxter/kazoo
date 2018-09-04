%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @author Ben Partridge
%%% @end
%%%-----------------------------------------------------------------------------
-module(navi_listener).
-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-export([handle_new_voicemail/2
        ,handle_push_request_device/2
        ,handle_pusher_request/2
        ]).

-include("navi.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

-define(BINDINGS, [{'notifications', [{'restrict_to', ['voicemail_new']}]}
                  ,{'navi', [{'restrict_to', ['push_device']}]}
                  ,{'pusher', []}
                  ,{'self', []}
                  ]).
%% TODO: One day refactor to allow adding additional events without modifying this file
-define(RESPONDERS, [{{?MODULE, 'handle_new_voicemail'}, [{<<"notification">>, <<"voicemail_new">>}]}
                    ,{{?MODULE, 'handle_push_request_device'}, [{<<"navi">>, <<"push_device">>}]}
                     %% Pusher's push request
                    ,{{?MODULE, 'handle_pusher_request'}, [{<<"notification">>, <<"push_req">>}]}
                    ]).
-define(QUEUE_NAME, <<"navi_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Determines who received the voicemail and which user that corresponds to, then sends notifications.
%% @end
%%------------------------------------------------------------------------------
-spec handle_new_voicemail(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_new_voicemail(JObj, _Props) ->
    %% Ensure object is actually of event we want
    'true' = kapi_notifications:voicemail_new_v(JObj),
    lager:debug("Handling New Voicemail push notification: ~s", [kz_json:encode(JObj)]),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    OwnerId = get_voicemail_owner(JObj),
    send_voicemail_notification(JObj, AccountId, OwnerId).

-spec get_voicemail_owner(kz_json:object()) -> kz_term:api_ne_binary().
get_voicemail_owner(JObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    VoicemailBoxId = kz_json:get_value(<<"Voicemail-Box">>, JObj),
    {'ok', Doc} = kz_datamgr:open_cache_doc(kz_util:format_account_db(AccountId), VoicemailBoxId),
    kzd_voicemail_box:owner_id(Doc).

-spec send_voicemail_notification(kz_json:object(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> any().
send_voicemail_notification(JObj, AccountId, OwnerId) ->
    Msg = kz_term:to_binary(io_lib:format("You received a voicemail message from ~s", [create_caller_id_string(JObj)])),
    ExtraParameters = [{<<"metadata">>, kz_json:from_list([{<<"event_name">>, <<"voicemail_new">>}])}
                      ,{<<"title">>, <<"Voicemail Received">>}
                      ],
    send_voicemail_notification(JObj, AccountId, OwnerId, Msg, ExtraParameters).
-spec send_voicemail_notification(kz_json:object(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> any().
send_voicemail_notification(_JObj, AccountId, OwnerId, Msg, ExtraParameters) when is_binary(OwnerId) ->
    push_notifications(AccountId, OwnerId, <<"new_voicemail">>, Msg, ExtraParameters);
send_voicemail_notification(_JObj, AccountId, 'undefined', Msg, ExtraParameters) ->
    %% When the voicemail box does not have an owner we will send the push notification to all users on that account.
    %% We're using a different subscription name than the regular "new_voicemail" subscription so that users can opt out
    %% of receiving messages for boxes which they don't own, without having to unsubscribe from all voicemail push
    %% notifications.
    push_notifications(AccountId, 'undefined', <<"new_unowned_voicemail">>, Msg, ExtraParameters).

%%------------------------------------------------------------------------------
%% @doc Handles external requests to send push notifications
%% @end
%%------------------------------------------------------------------------------
-spec handle_push_request_device(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_push_request_device(JObj, _Props) ->
    lager:debug("Navi received external push request"),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    DeviceId = kz_json:get_value(<<"Device-ID">>, JObj),
    Msg = kz_json:get_value(<<"Message">>, JObj),
    Event = kz_json:get_value(<<"Push-Topic">>, JObj),
    ExtraParameters = [{<<"metadata">>, kz_json:normalize(kz_json:get_value(<<"Metadata">>, JObj, kz_json:new()))}
                      ,{<<"title">>, kz_json:get_value(<<"Title">>, JObj)}
                      ],
    push_notifications_device(AccountId, DeviceId, Event, Msg, ExtraParameters).

-spec handle_pusher_request(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_pusher_request(JObj, _Props) ->
    lager:debug("Navi received pusher request ~s", [kz_json:encode(JObj)]),
    %% Token-App may need to also contain platform - will be in format "AppName-Platform" if this is the case
    {AppName, Platform} = decode_token_app(kz_json:get_value(<<"Token-App">>, JObj)),
    Registration = kz_json:from_list([{<<"notification_type">>, kz_json:get_value(<<"Token-Type">>, JObj)}
                                     ,{<<"notification_registration_id">>, kz_json:get_value(<<"Token-ID">>, JObj)}
                                     ,{<<"app_name">>, AppName}
                                     ,{<<"platform">>, Platform}
                                     ]),
    ExtraParameters = [{<<"metadata">>, kz_json:new()}
                      ,{<<"topic_extension">>, <<"voip">>}
                      ],
    send_notification_to_device(Registration, <<"incoming_call">>, ExtraParameters).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%% TODO: Move all of these functions into navi_util. Refactor to reduce code duplication.

%%% Gets the registrations for the user of the given type and pushes the notifications.
%%% If user is undefined then use any matching subscription on the account.
-spec push_notifications(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> any().
push_notifications(Account, User, Event, Message, ExtraParameters) when is_binary(User) ->
    case get_user_notification_registrations(Account, User, Event) of
        [_|_]=Registrations ->
            lager:debug("Found ~p notification registrations for event ~p for user: ~p on account: ~p", [length(Registrations), Event, User, Account]),
            [send_notification_to_device(Registration, Message, ExtraParameters) || Registration <- Registrations];
        [] -> lager:debug("No push notification registrations for event ~p for user: ~p on account: ~p", [Event, User, Account])
    end;
push_notifications(Account, 'undefined', Event, Message, ExtraParameters) ->
    case get_account_notification_registrations(Account, Event) of
        [_|_]=Registrations ->
            lager:debug("Found ~p notification registrations for event ~p for account: ~p", [length(Registrations), Event, Account]),
            [send_notification_to_device(Registration, Message, ExtraParameters) || Registration <- Registrations];
        [] -> lager:debug("No push notification registrations for event ~p for account: ~p", [Event, Account])
    end.

%%% Gets the registration(s) for the device of the given type and pushes the notification to that device
-spec push_notifications_device(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> any().
push_notifications_device(Account, Device, Event, Message, ExtraParameters) ->
    %% There may be multiple push notification registrations for the one device if the user uses many accounts on one device
    %% without unregistering, but we only need to return one of them as that will still deliver the notification to the device.
    case get_user_notification_registrations_device(Account, Device, Event) of
        [Reg|_]=Regs ->
            lager:debug("Found ~p notification registrations for device: ~p on account: ~p", [length(Regs), Device, Account]),
            send_notification_to_device(Reg, Message, ExtraParameters);
        [] -> lager:debug("No push notification registrations for device: ~p on account: ~p", [Device, Account])
    end.

%% Reads the view for the push registrations for a device
-spec get_user_notification_registrations_device(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:objects().
get_user_notification_registrations_device(Account, Device, Event) ->
    AccountDB = kz_util:format_account_db(Account),
    {'ok', Rows} = kz_datamgr:get_results(AccountDB, <<"push_notification_subscriptions/by_reg_by_subscription">>, [{'key', [Device, Event]}]),
    [kz_json:get_value(<<"value">>, Row) || Row <- Rows].

%%% Reads the view for all registered devices for a given user
-spec get_user_notification_registrations(kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:objects().
get_user_notification_registrations(Account, User, Event) ->
    AccountDB = kz_util:format_account_db(Account),
    {'ok', Rows} = kz_datamgr:get_results(AccountDB, <<"push_notification_subscriptions/by_user_by_subscription">>, [{'key', [User, Event]}]),
    [kz_json:get_value(<<"value">>, Row) || Row <- Rows].

%% Gets all registrations for an event on an account
-spec get_account_notification_registrations(kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_json:objects().
get_account_notification_registrations(Account, Event) ->
    AccountDB = kz_util:format_account_db(Account),
    {'ok', Rows} = kz_datamgr:get_results(AccountDB, <<"push_notification_subscriptions/by_subscription">>, [{'key', [Event]}]),
    [kz_json:get_value(<<"value">>, Row) || Row <- Rows].

%%% Develops a human readable string for the person's voicemail or missed call notification
-spec create_caller_id_string(kz_json:object()) -> kz_term:ne_binary().
create_caller_id_string(JObj) ->
    case {kz_json:get_value(<<"Caller-ID-Number">>, JObj), kz_json:get_value(<<"Caller-ID-Name">>, JObj)} of
        {'undefined', _} -> <<"someone">>;
        {Number, 'undefined'} -> Number;
        {Number, Name} -> kz_term:to_binary(io_lib:format("~s (~s)", [Name, Number]))
    end.

%%% Sets up the datastructure and calls the push notification service
-spec send_notification_to_device(kz_json:object(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok' | 'error'.
send_notification_to_device(Registration, Msg, ExtraParameters) ->
    lager:debug("Sending notification for registration: ~p", [Registration]),
    navi_module_sup:push(Registration, Msg, ExtraParameters).

-spec decode_token_app(kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_term:api_ne_binary()}.
decode_token_app(TokenApp) ->
    case binary:split(TokenApp, <<"-">>) of
        [App] -> {App, 'undefined'};
        [App, Platform] -> {App, Platform}
    end.
