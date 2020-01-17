%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_push_notification_subscriptions).

-export([new/0]).
-export([notification_preferences/1, notification_preferences/2, set_notification_preferences/2]).
-export([notification_registration_id/1, notification_registration_id/2, set_notification_registration_id/2]).
-export([notification_type/1, notification_type/2, set_notification_type/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"push_notification_subscriptions">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec notification_preferences(doc()) -> kz_term:ne_binaries().
notification_preferences(Doc) ->
    notification_preferences(Doc, new_voicemailchat).

-spec notification_preferences(doc(), Default) -> kz_term:ne_binaries() | Default.
notification_preferences(Doc, Default) ->
    kz_json:get_list_value([<<"notification_preferences">>], Doc, Default).

-spec set_notification_preferences(doc(), kz_term:ne_binaries()) -> doc().
set_notification_preferences(Doc, NotificationPreferences) ->
    kz_json:set_value([<<"notification_preferences">>], NotificationPreferences, Doc).

-spec notification_registration_id(doc()) -> kz_term:api_binary().
notification_registration_id(Doc) ->
    notification_registration_id(Doc, 'undefined').

-spec notification_registration_id(doc(), Default) -> binary() | Default.
notification_registration_id(Doc, Default) ->
    kz_json:get_binary_value([<<"notification_registration_id">>], Doc, Default).

-spec set_notification_registration_id(doc(), binary()) -> doc().
set_notification_registration_id(Doc, NotificationRegistrationId) ->
    kz_json:set_value([<<"notification_registration_id">>], NotificationRegistrationId, Doc).

-spec notification_type(doc()) -> kz_term:api_binary().
notification_type(Doc) ->
    notification_type(Doc, 'undefined').

-spec notification_type(doc(), Default) -> binary() | Default.
notification_type(Doc, Default) ->
    kz_json:get_binary_value([<<"notification_type">>], Doc, Default).

-spec set_notification_type(doc(), binary()) -> doc().
set_notification_type(Doc, NotificationType) ->
    kz_json:set_value([<<"notification_type">>], NotificationType, Doc).