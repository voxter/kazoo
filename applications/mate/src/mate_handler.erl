%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc Handler for push notification request from mattermost
%%%
%%% @author Ben Partridge
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(mate_handler).
-behaviour(cowboy_handler).

-export([init/2
        ,handle/2
        ,terminate/3
        ]).

-include("mate.hrl").

-record(state, {}).

-define(BY_MATTERMOST_TEAM, <<"mate/listing_by_mattermost_team_id">>).

-define(UNKNOWN_USER, <<"Unknown User">>).

%% Channel name regexes
-define(DIRECT_CHN_REGEX, <<"^direct-([0-9a-f]{28})-([0-9a-f]{28})$">>).
-define(CUSTOM_CHN_REGEX, <<"^custom-.+$">>).
-define(KUDOS_CHN_REGEX, <<"^kudos-([0-9a-f]{32})-.*$">>).

%% Message regexes
-define(USER_IN_CHN_MSG_PARTIAL, "^([0-9a-f]{32}) in ([^:]+): ").
-define(MSG_REGEX, <<?USER_IN_CHN_MSG_PARTIAL, "(.+)$">>).
-define(FILE_REGEX, <<"^([0-9a-f]{32}) Uploaded one or more files in (.+)$">>).

%% type, channel name regex, message regex
-define(MESSAGE_DECOMPOSERS, [{'direct_message', ?DIRECT_CHN_REGEX, ?MSG_REGEX}
                             ,{'direct_file', ?DIRECT_CHN_REGEX, ?FILE_REGEX}
                             ,{'custom_group_message', ?CUSTOM_CHN_REGEX, ?MSG_REGEX}
                             ,{'custom_group_file', ?CUSTOM_CHN_REGEX, ?FILE_REGEX}
                             ,{'kudos_transaction', ?KUDOS_CHN_REGEX, <<?USER_IN_CHN_MSG_PARTIAL, "([0-9]{1,3})-([0-9a-f]{32}(?:,[0-9a-f]{32})*)-(.+)$">>}
                             ,{'kudos_reply', ?KUDOS_CHN_REGEX, <<?USER_IN_CHN_MSG_PARTIAL, "([a-z0-9]{26})-(\d+)-(.+)$">>}
                             ,{'kudos_end_of_period', ?KUDOS_CHN_REGEX, <<?USER_IN_CHN_MSG_PARTIAL, "-end_of_period-$">>}
                             ]).

-type message_type() :: 'direct_message' | 'direct_file' | 'custom_group_message' | 'custom_group_file' | 'kudos_transaction' | 'kudos_reply' | 'kudos_end_of_period' | 'unknown'.
-type message_decomposer() :: {message_type(), kz_term:ne_binary(), kz_term:ne_binary()}.
-type message_decomposers() :: [message_decomposer()].
-type message_decomposition() :: {message_type(), kz_term:ne_binaries(), kz_term:ne_binaries()}.

-type state() :: #state{}.
-type callback_ret() :: {'ok', cowboy_req:req(), state() | 'no_state'}.

%% Called at beginning of request to do setup
-spec init(cowboy_req:req(), any()) -> callback_ret().
init(Req, []) ->
    handle(Req, #state{}).

%% Handles request
-spec handle(cowboy_req:req(), state()) -> callback_ret().
handle(Req, _State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> handle_post(Req);
        _ -> response(Req, 405, <<"method not allowed">>)
    end.

%% Does processing for post request
-spec handle_post(cowboy_req:req()) -> callback_ret().
handle_post(Req) ->
    lager:debug("push request received"),
    {JsonData, Req1} = read_body(Req),

    TeamId = kz_json:get_ne_value(<<"team_id">>, JsonData),
    DeviceId = kz_json:get_ne_value(<<"device_id">>, JsonData),
    Message = kz_json:get_ne_value(<<"message">>, JsonData),
    ChannelName = kz_json:get_ne_value(<<"channel_name">>, JsonData),

    case {account_from_team(TeamId), DeviceId, ChannelName, Message} of
        {'undefined', DeviceId, ChannelName, Message} ->
            lager:info("failed to find account for team_id ~s", [TeamId]),
            %% Request was probably valid, but this will ensure it shows up in the Mattermost logs!
            response(Req1, 400, <<"account_id">>);
        {_AccountId, 'undefined', ChannelName, Message} ->
            lager:info("no device_id in request json"),
            response(Req1, 400, <<"no device_id">>);
        {_AccountId, DeviceId, 'undefined', Message} ->
            lager:info("no channel_name in request json"),
            response(Req1, 400, <<"no device_id">>);
        {_AccountId, DeviceId, ChannelName, 'undefined'} ->
            lager:info("no message in request json"),
            response(Req1, 400, <<"no message">>);
        {AccountId, DeviceId, ChannelName, Message} ->
            lager:debug("seemingly valid push request, continuing"),
            %% Reformat the message into one that is meaningful for the user
            case format_message(AccountId, ChannelName, Message) of
                {'ok', Title, FormattedMessage} ->
                    lager:debug("formatted message successfully"),
                    %% Extract important metadata from push request
                    Metadata = get_notification_metadata(JsonData),
                    %% Send the notification
                    send_navi_req(AccountId, DeviceId, Title, FormattedMessage, Metadata),
                    lager:debug("forwarded push notification to navi"),
                    response(Req1, 200, <<"successfully forwarded push notification to navi">>);
                {'error', 'no_match'} ->
                    lager:debug("failed to format message"),
                    response(Req1, 400, <<"no formatter">>)
            end
    end.

-spec send_navi_req(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_navi_req(AccountId, DeviceId, Title, Message, Metadata) ->
    Payload = kz_json:from_list([{<<"Account-ID">>, AccountId}
                                ,{<<"Device-ID">>, DeviceId}
                                ,{<<"Message">>, Message}
                                ,{<<"Push-Topic">>, <<"chat">>}
                                ,{<<"Metadata">>, Metadata}
                                ,{<<"Title">>, Title}
                                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                ]),
    kapi_navi:publish_push_device(Payload),
    lager:debug("sent navi request").

%% Called at end of request for cleanup
-spec terminate(any(), cowboy_req:req(), state()) -> 'ok'.
terminate(_Reason, _Req, _State) -> 'ok'.

-spec response(cowboy_req:req(), pos_integer(), kz_term:ne_binary()) -> callback_ret().
response(Req0, Status, Body) ->
    Req = cowboy_req:reply(Status, #{<<"content-type">> => <<"text/plain">>}, Body, Req0),
    {'ok', Req, 'no_state'}.

%% Reads the entire body of the request and returns it in JSON
-spec read_body(cowboy_req:req()) -> {kz_json:object(), cowboy_req:req()}.
read_body(Req) ->
    read_body(cowboy_req:read_body(Req), <<>>).
read_body({'ok', Data, Req}, Acc) ->
    {kz_json:decode(<<Acc/binary, Data/binary>>), Req};
read_body({'more', Data, Req}, Acc) ->
    read_body(cowboy_req:read_body(Req), <<Acc/binary, Data/binary>>).

%% Gets the corresponding account id for the given mattermost team
-spec account_from_team(kz_term:api_binary()) -> kz_term:api_binary().
account_from_team(TeamId) ->
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?BY_MATTERMOST_TEAM, [{'key', TeamId}]) of
        {'ok', [TeamRow|[]]} ->
            Team = kz_json:get_value(<<"value">>, TeamRow),
            kz_json:get_value(<<"account_id">>, Team);
        _ ->
            'undefined'
    end.

-spec format_message(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_term:ne_binary(), kz_term:ne_binary()} | {'error', 'no_match'}.
format_message(AccountId, ChannelName, PushMessage) ->
    format_message(AccountId, decompose_message(ChannelName, PushMessage)).
%% We will receive the following args: (AccountId, {MessageType, ChannelNameRegexMatchGroups, PushMessageRegexMatchGroups})
%% We should return {'ok', NotificationTitle, NotificationBody}

-spec format_message(kz_term:ne_binary(), message_decomposition()) -> {'ok', kz_term:ne_binary(), kz_term:ne_binary()} | {'error', 'no_match'}.
format_message(AccountId, {'direct_message', _, [_PushMessage, SenderId, _GroupName, Message]}) ->
    {'ok', username(AccountId, SenderId), Message};
format_message(AccountId, {'direct_file', _, [_PushMessage, SenderId, _GroupName]}) ->
    {'ok', username(AccountId, SenderId), <<"Sent you a file.">>};
format_message(AccountId, {'custom_group_message', _, [_PushMessage, SenderId, GroupName, Message]}) ->
    {'ok', <<(username(AccountId, SenderId))/binary, " in ", GroupName/binary>>, Message};
format_message(AccountId, {'custom_group_file', _, [_PushMessage, SenderId, GroupName]}) ->
    {'ok', <<(username(AccountId, SenderId))/binary, " in ", GroupName/binary>>, <<"Sent you a file.">>};
format_message(AccountId, {'kudos_transaction', _, [_PushMessage, SenderId, _GroupName, Points, Recipients, Message]}) ->
    Sender = username(AccountId, SenderId),
    RecipientIds = binary:split(Recipients, <<",">>, ['global']),
    RecipientString = kz_binary:join([username(AccountId, Id) || Id <- RecipientIds], <<", ">>),
    {'ok', <<Sender/binary, " sent ", RecipientString/binary, " ", Points/binary, " Kudos">>, Message};
format_message(AccountId, {'kudos_end_of_period', [_ChannelName, OwnerId], [_PushMessage, OwnerId, _GroupName]}) ->
    {'ok', username(AccountId, OwnerId), <<"Has closed the current Kudos period, points have been reset.">>};
format_message(_AccountId, {'kudos_reply', _, _}) ->
    lager:info("not currently handling kudos reply"),
    {'error', 'no_match'};
format_message(_AccountId, {'unknown', [ChannelName], [PushMessage]}) ->
    lager:info("didn't know how to process push '~s' message '~s'", [ChannelName, PushMessage]),
    {'error', 'no_match'}.

-spec decompose_message(kz_term:ne_binary(), kz_term:ne_binary()) -> message_decomposition().
decompose_message(ChatName, Message) ->
    decompose_message(ChatName, Message, ?MESSAGE_DECOMPOSERS).

-spec decompose_message(kz_term:ne_binary(), kz_term:ne_binary(), message_decomposers()) -> message_decomposition().
decompose_message(ChatName, Message, [{MatchType, ChatNameRegex, MessageRegex} | RemainingRegexes]) ->
    case re:run(ChatName, ChatNameRegex, [{'capture', 'all', 'binary'}]) of
        {'match', ChatNameCapture} ->
            case re:run(Message, MessageRegex, [{'capture', 'all', 'binary'}]) of
                {'match', MessageCapture} -> {MatchType, ChatNameCapture, MessageCapture};
                'nomatch' -> decompose_message(ChatName, Message, RemainingRegexes)
            end;
        'nomatch' ->
            decompose_message(ChatName, Message, RemainingRegexes)
    end;
decompose_message(ChatName, Message, []) ->
    {'unknown', [ChatName], [Message]}.

%% Gets the human readable username to put in the message body
-spec username(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
username(AccountId, UserId) ->
    case kzd_user:fetch(AccountId, UserId) of
        {'ok', UserDoc} ->
            kzd_user:name(UserDoc);
        {'error', _Reason} ->
            lager:info("failed to find user ~s for account ~s with reason ~p", [AccountId, UserId, _Reason]),
            ?UNKNOWN_USER
    end.

%% Gets the useful mattermost metadata from the push request
-spec get_notification_metadata(kz_json:object()) -> kz_term:proplist().
get_notification_metadata(JsonData) ->
    kz_json:from_list([{<<"badge_count">>, kz_json:get_value(<<"badge">>, JsonData)}
                      ,{<<"team_id">>, kz_json:get_value(<<"team_id">>, JsonData)}
                      ,{<<"channel_id">>, kz_json:get_value(<<"channel_id">>, JsonData)}
                      ,{<<"post_id">>, kz_json:get_value(<<"post_id">>, JsonData)}
                      ,{<<"sender_id">>, kz_json:get_value(<<"sender_id">>, JsonData)}
                      ]).
