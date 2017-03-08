-ifndef(ACDC_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_api.hrl").

-define(CONFIG_CAT, <<"acdc">>).

-define(APP_NAME, <<"acdc">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(ACDC_CACHE, 'acdc_cache').

-define(ABANDON_TIMEOUT, 'member_timeout').
-define(ABANDON_EXIT, 'member_exit').
-define(ABANDON_HANGUP, 'member_hangup').
-define(ABANDON_EMPTY, 'member_exit_empty').

-define(PRESENCE_GREEN, <<"terminated">>).
-define(PRESENCE_RED_FLASH, <<"early">>).
-define(PRESENCE_RED_SOLID, <<"confirmed">>).

-define(ENDPOINT_UPDATE_REG(AcctId, EPId), {'p', 'l', {'endpoint_update', AcctId, EPId}}).
-define(ENDPOINT_CREATED(EP), {'endpoint_created', EP}).
-define(ENDPOINT_EDITED(EP), {'endpoint_edited', EP}).
-define(ENDPOINT_DELETED(EP), {'endpoint_deleted', EP}).

-define(OWNER_UPDATE_REG(AcctId, OwnerId), {'p', 'l', {'owner_update', AcctId, OwnerId}}).

-define(NEW_CHANNEL_REG(AcctId, User), {'p', 'l', {'new_channel', AcctId, User}}).
-define(NEW_CHANNEL_FROM(CallId), {'call_from', CallId}).
-define(NEW_CHANNEL_TO(CallId, MemberCallId), {'call_to', CallId, MemberCallId}).

-define(DESTROYED_CHANNEL_REG(AcctId, User), {'p', 'l', {'destroyed_channel', AcctId, User}}).
-define(DESTROYED_CHANNEL(CallId, HangupCause), {'call_down', CallId, HangupCause}).

-type abandon_reason() :: ?ABANDON_TIMEOUT | ?ABANDON_EXIT |
                          ?ABANDON_HANGUP.

-type deliveries() :: [gen_listener:basic_deliver()].

-type announce_pid_list() :: [{api_binary(), pid()},...] | [].

-type fsm_state_name() :: 'wait' | 'sync' | 'ready' | 'ringing' |
                          'ringing_callback' | 'awaiting_callback' |
                          'answered' | 'wrapup' | 'paused' | 'outbound'.

%% Check for cleanup every 5 minutes
-define(CLEANUP_PERIOD, whapps_config:get_integer(?CONFIG_CAT, <<"cleanup_period_ms">>, 360000)).

%% Remove data from ETS
-define(CLEANUP_WINDOW, whapps_config:get_integer(?CONFIG_CAT, <<"cleanup_window_s">>, ?SECONDS_IN_DAY)).

%% Archive every 60 seconds
-define(ARCHIVE_PERIOD, whapps_config:get_integer(?CONFIG_CAT, <<"archive_period_ms">>, 60000)).

%% Save data to the DB
-define(ARCHIVE_WINDOW, whapps_config:get_integer(?CONFIG_CAT, <<"archive_window_s">>, 60)).


-define(ACDC_HRL, 'true').
-endif.
