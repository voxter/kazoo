%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc API that registers mobile devices for push notifications
%%%
%%%
%%% @author Ben Partridge
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_push_notification_subscriptions).

-export([init/0
        ,allowed_methods/2
        ,resource_exists/2
        ,validate/3
        ,put/3
        ,post/3
        ,delete/3
        ]).

-include("crossbar.hrl").

-define(PUSH_KEY, <<"push">>).
-define(PUSH_NOTIFICATION_SUBSCRIPTIONS, <<"push_notification_subscriptions">>).

-define(BY_MOBILE_DEVICE, <<"push_notification_subscriptions/by_mobile_device">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.push_notification_subscriptions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.push_notification_subscriptions">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.push_notification_subscriptions">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.push_notification_subscriptions">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.push_notification_subscriptions">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.push_notification_subscriptions">>, ?MODULE, 'delete').


%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_App, _DeviceId) ->
    [?HTTP_PUT, ?HTTP_DELETE, ?HTTP_POST, ?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource
%%
%% For example:
%%
%% ```
%%    /push_notification_subscriptions => [].
%%    /push_notification_subscriptions/foo => [<<"foo">>]
%%    /push_notification_subscriptions/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_App, _DeviceId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /push_notification_subscriptions mights load a list of push_notification_subscription objects
%% /push_notification_subscriptions/123 might load the push_notification_subscription object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, App, DeviceId) ->
    validate_push_notification_subscriptions(Context, App, DeviceId, cb_context:req_verb(Context)).

-spec validate_push_notification_subscriptions(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_push_notification_subscriptions(Context, App, DeviceId, ?HTTP_PUT) ->
    lager:debug("validating push notification subscription for PUT"),
    create(Context, App, DeviceId);
validate_push_notification_subscriptions(Context, App, DeviceId, ?HTTP_POST) ->
    lager:debug("validating push notification subscription for POST"),
    update(Context, App, DeviceId);
validate_push_notification_subscriptions(Context, _App, DeviceId, ?HTTP_DELETE) ->
    lager:debug("validating push notification subscription for DELETE"),
    {_Status, Context1} = read_doc_for_subscriptions(Context, DeviceId),
    Context1;
validate_push_notification_subscriptions(Context, _App, DeviceId, ?HTTP_GET) ->
    lager:debug("validating push notification subscription for GET"),
    case read_doc_for_subscriptions(Context, DeviceId) of
        {'exists', DeviceContext} -> subs_response(DeviceContext);
        {_Status, BadContext} -> BadContext
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, App, DeviceId) ->
    post(Context, App, DeviceId).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _App, DeviceId) ->
    OnPurgeSuccess = fun() -> save_subscriptions(Context, fun subs_response/1) end,
    purge_existing_subscriptions_for_mobile_device(Context, DeviceId, OnPurgeSuccess).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _App, DeviceId) ->
    lager:debug("delete push notification subscriptions from device ~s", [DeviceId]),
    DeviceDoc = delete_subscriptions(cb_context:doc(Context)),
    save_subscriptions(cb_context:set_doc(Context, DeviceDoc), fun delete_response/1).

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context(), path_token(), path_token()) -> cb_context:context().
create(Context, App, DeviceId) ->
    case subscription_does_exist(DeviceId, Context) of
        {'exists', _DeviceContext} ->
            lager:error("database already contains push notification subscription for [~p, ~p]", [DeviceId, App]),
            crossbar_util:response_400(<<"subscription already exists for this device">>, <<"bad request">>, Context);
        {'not_exists', DeviceContext} -> validate_push_notification_subscriptions(Context, App, DeviceContext);
        {_Status, BadContext} -> BadContext
    end.

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(cb_context:context(), path_token(), path_token()) -> cb_context:context().
update(Context, App, DeviceId) ->
    case subscription_does_exist(DeviceId, Context) of
        {'exists', DeviceContext} -> validate_push_notification_subscriptions(Context, App, DeviceContext);
        {'not_exists', DeviceContext} -> response_device_not_found(DeviceId, DeviceContext);
        {_Status, BadContext} -> BadContext
    end.

%%------------------------------------------------------------------------------
%% @doc Read active push notification registrations for the given device
%% @end
%%------------------------------------------------------------------------------
-spec read_doc_for_subscriptions(cb_context:context(), path_token()) -> {'exists' | 'not_exists' | 'error', cb_context:context()}.
read_doc_for_subscriptions(Context, DeviceId) ->
    case subscription_does_exist(DeviceId, Context) of
        {'exists', _DeviceContext}=Result -> Result;
        {'not_exists', DeviceContext} -> {'not_exists', response_device_not_found(DeviceId, DeviceContext)};
        {_Status, _BadContext}=Result ->
            lager:debug("could not read document for push notifications"),
            Result
    end.

%%------------------------------------------------------------------------------
%% @doc Validate request data against the push notification subscriptions schema
%% @end
%%------------------------------------------------------------------------------
-spec validate_push_notification_subscriptions(cb_context:context(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_push_notification_subscriptions(Context, App, DeviceContext) ->
    OnSuccess = fun(C) -> on_successful_validation(App, DeviceContext, C) end,
    cb_context:validate_request_data(?PUSH_NOTIFICATION_SUBSCRIPTIONS, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Called when request has validated successfully, prepares document for saving in db
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:ne_binary(), cb_context:context(), cb_context:context()) -> cb_context:context().
on_successful_validation(App, DeviceContext, Context) ->
    %% Set values that came from path
    lager:debug("new push notification subscriptions validated successfully"),
    NewDoc = kz_json:set_value(<<"app_name">>, App, cb_context:doc(Context)),
    lager:debug("set push notification subscriptions app_name: ~p", [NewDoc]),
    DeviceDoc = kz_json:set_value(?PUSH_NOTIFICATION_SUBSCRIPTIONS, NewDoc, cb_context:doc(DeviceContext)),
    cb_context:set_doc(DeviceContext, update_pusher_props(DeviceDoc)).

%%------------------------------------------------------------------------------
%% @doc Shims the behaviour originally handled by
%% `pusher_listener:handle_reg_success/2' - that is, push token information is
%% set on the device so that it will be treated as a push endpoint by
%% `kz_endpoint'. This is in place because pusher _cannot_ be run alongside
%% navi.
%% @end
%%------------------------------------------------------------------------------
-spec update_pusher_props(kzd_devices:doc()) -> kzd_devices:doc().
update_pusher_props(DeviceDoc) ->
    case pusher_params_compat(subscriptions(DeviceDoc)) of
        [] -> kz_json:delete_key(?PUSH_KEY, DeviceDoc);
        Params ->
            Push = kz_json:set_values(Params, kz_json:new()),
            kz_json:set_value(?PUSH_KEY, Push, DeviceDoc)
    end.

%%------------------------------------------------------------------------------
%% @doc Uses a subscription for `incoming_call', if present, to produce push
%% params in a format that is compatible with `pusher_listener:build_push/4'.
%% @end
%%------------------------------------------------------------------------------
-spec pusher_params_compat(kz_json:object()) -> kz_term:proplist().
pusher_params_compat(Subscriptions) ->
    AppName = kz_json:get_ne_binary_value(<<"app_name">>, Subscriptions),
    NotifRegs = kz_json:get_json_value(<<"notification_registration_ids">>, Subscriptions),
    pusher_params_compat_fold(AppName, kz_json:get_values(NotifRegs)).

-spec pusher_params_compat_fold(kz_term:ne_binary(), {kz_json:json_terms(), kz_json:keys()}) ->
          kz_term:proplist().
pusher_params_compat_fold(_, {[], []}) -> [];
pusher_params_compat_fold(AppName, {[Subscription|Subscriptions], [RegId|RegIds]}) ->
    NotifPrefs = kz_json:get_list_value(<<"notification_preferences">>, Subscription),
    NotifType = kz_json:get_ne_binary_value(<<"notification_type">>, Subscription),
    case lists:member(<<"incoming_call">>, NotifPrefs) of
        'true' ->
            [{<<"Token-App">>, AppName}
            ,{<<"Token-ID">>, RegId}
            ,{<<"Token-Type">>, NotifType}
            ];
        'false' -> pusher_params_compat_fold(AppName, {Subscriptions, RegIds})
    end.

%%------------------------------------------------------------------------------
%% @doc Saves new push notification subscriptions into the device doc.
%% @end
%%------------------------------------------------------------------------------
-type on_save_success_callback() :: fun((cb_context:context()) -> cb_context:context()).

-spec save_subscriptions(cb_context:context(), on_save_success_callback()) -> cb_context:context().
save_subscriptions(DeviceContext, OnSaveSuccess) ->
    DeviceContext1 = crossbar_doc:save(DeviceContext),
    case cb_context:has_errors(DeviceContext1) of
        'true' -> DeviceContext1;
        'false' -> OnSaveSuccess(DeviceContext1)
    end.

%%------------------------------------------------------------------------------
%% @doc Purge existing subscriptions for the mobile device from Kazoo device
%% docs.
%% @end
%%------------------------------------------------------------------------------
-type on_purge_success_callback() :: fun(() -> cb_context:context()).

-spec purge_existing_subscriptions_for_mobile_device(cb_context:context(), kz_term:ne_binary(), on_purge_success_callback()) ->
          cb_context:context().
purge_existing_subscriptions_for_mobile_device(Context, DeviceId, OnPurgeSuccess) ->
    MobileDeviceId = kz_json:get_ne_binary_value([?PUSH_NOTIFICATION_SUBSCRIPTIONS, <<"mobile_device_id">>]
                                                ,cb_context:doc(Context)
                                                ),
    ViewContext = crossbar_doc:load_view(?BY_MOBILE_DEVICE, [{'key', MobileDeviceId}], Context, fun normalize_view_results/2),
    case cb_context:has_errors(ViewContext) of
        'true' -> ViewContext;
        'false' ->
            OtherDeviceDocs = cb_context:doc(ViewContext),
            purge_existing_subscriptions(Context, DeviceId, OtherDeviceDocs, OnPurgeSuccess)
    end.

-spec purge_existing_subscriptions(cb_context:context(), kz_term:ne_binary(), [kzd_devices:doc()], on_purge_success_callback()) ->
          cb_context:context().
purge_existing_subscriptions(_, _, [], OnPurgeSuccess) -> OnPurgeSuccess();
purge_existing_subscriptions(Context, DeviceId, [OtherDeviceDoc|OtherDeviceDocs], OnPurgeSuccess) ->
    OtherDeviceId = kz_doc:id(OtherDeviceDoc),
    OnSuccess = fun() -> purge_existing_subscriptions(Context, DeviceId, OtherDeviceDocs, OnPurgeSuccess) end,
    case OtherDeviceId of
        DeviceId ->
            %% Skipping purging the one we are planning to update, it's a waste and causes conflicts
            OnSuccess();
        _ -> purge_subscriptions(Context, OtherDeviceId, OtherDeviceDoc, OnSuccess)
    end.

-spec purge_subscriptions(cb_context:context(), kz_term:ne_binary(), kzd_devices:doc(), on_purge_success_callback()) ->
          cb_context:context().
purge_subscriptions(Context, OtherDeviceId, OtherDeviceDoc, OnSuccess) ->
    lager:debug("purging existing subscriptions in other device ~s", [OtherDeviceId]),
    OtherDeviceDoc1 = delete_subscriptions(OtherDeviceDoc),
    case kz_datamgr:save_doc(cb_context:account_db(Context), OtherDeviceDoc1) of
        {'ok', _} -> OnSuccess();
        {'error', Error} -> crossbar_doc:handle_datamgr_errors(Error, OtherDeviceId, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_json_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc Checks whether a push notification subscription exists for the specified
%% device. Returns 'not_exists' when the device doc does not exist or if it does
%% exist but there is no subscription.
%% @end
%%------------------------------------------------------------------------------
-spec subscription_does_exist(kz_term:ne_binary(), cb_context:context()) -> {'exists' | 'not_exists' | 'error', cb_context:context()}.
subscription_does_exist(DeviceId, Context) ->
    lager:debug("reading push notification subscription for device ~s", [DeviceId]),
    DeviceContext = crossbar_doc:load(DeviceId, Context, ?TYPE_CHECK_OPTION(kzd_devices:type())),
    case cb_context:has_errors(DeviceContext) of
        'true' -> {'error', DeviceContext};
        'false' -> {subscription_does_exist(cb_context:doc(DeviceContext)), DeviceContext}
    end.

-spec subscription_does_exist(kzd_devices:doc()) -> 'exists' | 'not_exists'.
subscription_does_exist(DeviceDoc) ->
    case subscriptions(DeviceDoc) of
        'undefined' -> 'not_exists';
        Subscriptions ->
            lager:debug("found push notification subscriptions: ~p", [Subscriptions]),
            'exists'
    end.

%%------------------------------------------------------------------------------
%% @doc Get subscriptions from a device doc.
%% @end
%%------------------------------------------------------------------------------
-spec subscriptions(kzd_devices:doc()) -> kz_json:object().
subscriptions(DeviceDoc) ->
    kz_json:get_json_value(?PUSH_NOTIFICATION_SUBSCRIPTIONS, DeviceDoc).

%%------------------------------------------------------------------------------
%% @doc Delete subscriptions from a device doc. Also deletes the pusher config
%% to keep it in sync with the subscriptions.
%% @end
%%------------------------------------------------------------------------------
-spec delete_subscriptions(kzd_devices:doc()) -> kzd_devices:doc().
delete_subscriptions(DeviceDoc) ->
    kz_json:delete_keys([?PUSH_KEY
                        ,?PUSH_NOTIFICATION_SUBSCRIPTIONS
                        ]
                       ,DeviceDoc
                       ).

%%------------------------------------------------------------------------------
%% @doc Format response for requests that return subscription data
%% @end
%%------------------------------------------------------------------------------
-spec subs_response(cb_context:context()) -> cb_context:context().
subs_response(DeviceContext) ->
    Subscriptions = subscriptions(cb_context:doc(DeviceContext)),
    DeviceContext1 = cb_context:set_doc(DeviceContext, Subscriptions),
    crossbar_util:response(Subscriptions, DeviceContext1).

%%------------------------------------------------------------------------------
%% @doc Format response for requests that delete push notification subscriptions
%% @end
%%------------------------------------------------------------------------------
-spec delete_response(cb_context:context()) -> cb_context:context().
delete_response(DeviceContext) ->
    RespData = cb_context:resp_data(DeviceContext),
    SubscriptionsRespData = kz_json:from_list([{<<"id">>, kz_doc:id(RespData)}
                                              ,{<<"_read_only">>, kz_json:get_json_value(<<"_read_only">>, RespData)}
                                              ]),
    crossbar_util:response(SubscriptionsRespData, DeviceContext).

%%------------------------------------------------------------------------------
%% @doc Respond with a "bad identifier" response, the same way it would be
%% returned for docs not found when using `crossbar_doc:load'.
%% @end
%%------------------------------------------------------------------------------
-spec response_device_not_found(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
response_device_not_found(DeviceId, DeviceContext) ->
    cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, DeviceId}]), DeviceContext).
