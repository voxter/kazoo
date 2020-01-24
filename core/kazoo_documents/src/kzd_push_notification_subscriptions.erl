%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_push_notification_subscriptions).

-export([new/0]).
-export([app_name/1, app_name/2, set_app_name/2]).
-export([mobile_device_id/1, mobile_device_id/2, set_mobile_device_id/2]).
-export([notification_registration_ids/1, notification_registration_ids/2, set_notification_registration_ids/2]).
-export([platform/1, platform/2, set_platform/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"push_notification_subscriptions">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec app_name(doc()) -> kz_term:api_binary().
app_name(Doc) ->
    app_name(Doc, 'undefined').

-spec app_name(doc(), Default) -> binary() | Default.
app_name(Doc, Default) ->
    kz_json:get_binary_value([<<"app_name">>], Doc, Default).

-spec set_app_name(doc(), binary()) -> doc().
set_app_name(Doc, AppName) ->
    kz_json:set_value([<<"app_name">>], AppName, Doc).

-spec mobile_device_id(doc()) -> kz_term:api_binary().
mobile_device_id(Doc) ->
    mobile_device_id(Doc, 'undefined').

-spec mobile_device_id(doc(), Default) -> binary() | Default.
mobile_device_id(Doc, Default) ->
    kz_json:get_binary_value([<<"mobile_device_id">>], Doc, Default).

-spec set_mobile_device_id(doc(), binary()) -> doc().
set_mobile_device_id(Doc, MobileDeviceId) ->
    kz_json:set_value([<<"mobile_device_id">>], MobileDeviceId, Doc).

-spec notification_registration_ids(doc()) -> kz_term:api_object().
notification_registration_ids(Doc) ->
    notification_registration_ids(Doc, 'undefined').

-spec notification_registration_ids(doc(), Default) -> kz_json:object() | Default.
notification_registration_ids(Doc, Default) ->
    kz_json:get_json_value([<<"notification_registration_ids">>], Doc, Default).

-spec set_notification_registration_ids(doc(), kz_json:object()) -> doc().
set_notification_registration_ids(Doc, NotificationRegistrationIds) ->
    kz_json:set_value([<<"notification_registration_ids">>], NotificationRegistrationIds, Doc).

-spec platform(doc()) -> kz_term:api_binary().
platform(Doc) ->
    platform(Doc, 'undefined').

-spec platform(doc(), Default) -> binary() | Default.
platform(Doc, Default) ->
    kz_json:get_binary_value([<<"platform">>], Doc, Default).

-spec set_platform(doc(), binary()) -> doc().
set_platform(Doc, Platform) ->
    kz_json:set_value([<<"platform">>], Platform, Doc).
