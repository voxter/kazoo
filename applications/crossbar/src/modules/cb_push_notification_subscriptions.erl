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

-define(SCHEMA_PUSH_NOTIFICATION_SUBSCRIPTIONS, <<"push_notification_subscriptions">>).

-define(BY_DEVICE, <<"push_notification_subscriptions/by_device">>).
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
    validate_push_notification_subscription(Context, App, DeviceId, cb_context:req_verb(Context)).

-spec validate_push_notification_subscription(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_push_notification_subscription(Context, App, DeviceId, ?HTTP_PUT) ->
    lager:debug("validating push notification subscription for PUT"),
    create(Context, App, DeviceId);
validate_push_notification_subscription(Context, App, DeviceId, ?HTTP_POST) ->
    lager:debug("validating push notification subscription for POST"),
    update(Context, App, DeviceId);
validate_push_notification_subscription(Context, _App, DeviceId, ?HTTP_DELETE) ->
    lager:debug("validating push notification subscription for DELETE"),
    read_doc_for_subscription(Context, DeviceId);
validate_push_notification_subscription(Context, _App, DeviceId, ?HTTP_GET) ->
    lager:debug("validating push notification subscription for GET"),
    read_doc_for_subscription(Context, DeviceId).

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
    OnPurgeSuccess = fun() -> maybe_save_subscription(Context) end,
    purge_existing_subscriptions_for_mobile_device(Context, DeviceId, OnPurgeSuccess).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _App, DeviceId) ->
    lager:debug("delete push notification subscription for device ~s", [DeviceId]),
    crossbar_doc:delete(Context).

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
        {'exists', SubscriptionContext} ->
            lager:error("database already contains push notification subscription for [~p, ~p]", [DeviceId, App]),
            crossbar_util:response_400(<<"subscription already exists for this device">>, <<"bad request">>, SubscriptionContext);
        {'not_exists', Context1} -> validate_subscription(Context1, App, DeviceId, 'undefined');
        {_Status, BadContext} -> BadContext
    end.

%%------------------------------------------------------------------------------
%% @doc Update an instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec update(cb_context:context(), path_token(), path_token()) -> cb_context:context().
update(Context, App, DeviceId) ->
    case subscription_does_exist(DeviceId, Context) of
        {'exists', SubscriptionContext} ->
            Subscription = cb_context:doc(Context),
            validate_subscription(SubscriptionContext, App, DeviceId, kz_doc:id(Subscription));
        {'not_exists', BadContext} -> response_not_found(DeviceId, BadContext);
        {_Status, BadContext} -> BadContext
    end.

%%------------------------------------------------------------------------------
%% @doc Read active push notification subscription for the given device
%% @end
%%------------------------------------------------------------------------------
-spec read_doc_for_subscription(cb_context:context(), path_token()) -> cb_context:context().
read_doc_for_subscription(Context, DeviceId) ->
    case subscription_does_exist(DeviceId, Context) of
        {'exists', SubscriptionContext} -> SubscriptionContext;
        {'not_exists', BadContext} -> response_not_found(DeviceId, BadContext);
        {_Status, BadContext} ->
            lager:debug("could not read document for push notifications"),
            BadContext
    end.

%%------------------------------------------------------------------------------
%% @doc Validate request data against the push notification subscriptions schema
%% @end
%%------------------------------------------------------------------------------
-spec validate_subscription(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> cb_context:context().
validate_subscription(Context, App, DeviceId, SubscriptionId) ->
    %% Set values that came from path
    Setters = [{fun kzd_push_notification_subscriptions:set_app_name/2, App}
              ,{fun kzd_push_notification_subscriptions:set_device_id/2, DeviceId}
              ],
    ReqData = lists:foldl(fun({Setter, Value}, ReqDataAcc) -> Setter(ReqDataAcc, Value) end
                         ,cb_context:req_data(Context)
                         ,Setters
                         ),
    lager:debug("set push notification subscription app_name/device_id: ~p", [ReqData]),
    Context1 = cb_context:set_req_data(Context, ReqData),
    OnSuccess = fun(C) ->
                        lager:debug("new push notification subscription validated successfully"),
                        on_successful_validation(SubscriptionId, C)
                end,
    cb_context:validate_request_data(?SCHEMA_PUSH_NOTIFICATION_SUBSCRIPTIONS, Context1, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Called when request has validated successfully, prepares document for saving in db
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_ne_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    NewDoc = lists:foldl(fun(F, Doc) -> F(Doc) end, cb_context:doc(Context),
                         [fun(X) -> kz_json:set_value(<<"pvt_user_id">>, cb_context:auth_user_id(Context), X) end
                         ,fun(X) -> kz_doc:set_type(X, <<"push_notification_subscription">>) end
                         ]),
    lager:debug("set push notification subscription doc filled with private values: ~p", [NewDoc]),
    cb_context:set_doc(Context, NewDoc);
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"push_notification_subscription">>)).

%%------------------------------------------------------------------------------
%% @doc Saves push notification subscription if it is new or has changed.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_save_subscription(cb_context:context()) -> cb_context:context().
maybe_save_subscription(SubscriptionContext) ->
    Subscription = cb_context:doc(SubscriptionContext),
    OldSubscription = cb_context:fetch(SubscriptionContext, 'db_doc', kz_json:new()),
    case kz_json:are_equal(Subscription, OldSubscription) of
        'true' -> SubscriptionContext;
        'false' -> save_subscription(SubscriptionContext)
    end.

%%------------------------------------------------------------------------------
%% @doc Saves push notification subscription.
%% @end
%%------------------------------------------------------------------------------
-spec save_subscription(cb_context:context()) -> cb_context:context().
save_subscription(SubscriptionContext) ->
    crossbar_doc:save(SubscriptionContext).

%%------------------------------------------------------------------------------
%% @doc Purge existing subscriptions for the mobile device.
%% @end
%%------------------------------------------------------------------------------
-type on_purge_success_callback() :: fun(() -> cb_context:context()).

-spec purge_existing_subscriptions_for_mobile_device(cb_context:context(), kz_term:ne_binary(), on_purge_success_callback()) ->
          cb_context:context().
purge_existing_subscriptions_for_mobile_device(SubscriptionContext, DeviceId, OnPurgeSuccess) ->
    Subscription = cb_context:doc(SubscriptionContext),
    MobileDeviceId = kzd_push_notification_subscriptions:mobile_device_id(Subscription),
    ViewContext = crossbar_doc:load_view(?BY_MOBILE_DEVICE, [{'key', MobileDeviceId}], SubscriptionContext, fun normalize_view_results/2),
    case cb_context:has_errors(ViewContext) of
        'true' -> ViewContext;
        'false' ->
            OtherSubscriptions = cb_context:doc(ViewContext),
            purge_existing_subscriptions(SubscriptionContext, DeviceId, OtherSubscriptions, OnPurgeSuccess)
    end.

-spec purge_existing_subscriptions(cb_context:context(), kz_term:ne_binary(), [kz_json:object()], on_purge_success_callback()) ->
          cb_context:context().
purge_existing_subscriptions(_, _, [], OnPurgeSuccess) -> OnPurgeSuccess();
purge_existing_subscriptions(SubscriptionContext, DeviceId, [OtherSubscription|OtherSubscriptions], OnPurgeSuccess) ->
    OtherDeviceId = kzd_push_notification_subscriptions:device_id(OtherSubscription),
    OnSuccess = fun() -> purge_existing_subscriptions(SubscriptionContext, DeviceId, OtherSubscriptions, OnPurgeSuccess) end,
    case OtherDeviceId of
        DeviceId ->
            %% Skipping purging the one we are planning to update, it's a waste and causes conflicts
            OnSuccess();
        _ -> purge_existing_subscription(SubscriptionContext, OtherSubscription, OnSuccess)
    end.

-spec purge_existing_subscription(cb_context:context(), kz_json:object(), on_purge_success_callback()) -> cb_context:context().
purge_existing_subscription(SubscriptionContext, OtherSubscription, OnSuccess) ->
    OtherSubscriptionId = kz_doc:id(OtherSubscription),
    lager:debug("purging existing subscription ~s", [OtherSubscriptionId]),
    OtherSubscriptionContext = cb_context:set_doc(SubscriptionContext, OtherSubscription),
    OtherSubscriptionContext1 = crossbar_doc:delete(OtherSubscriptionContext),
    case cb_context:has_errors(OtherSubscriptionContext1) of
        'true' ->
            %% TODO error message unable to purge a subscription
            OtherSubscriptionContext1;
        'false' -> OnSuccess()
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
        'false' ->
            {Status, Context1} = subscription_does_exist2(DeviceId, Context),
            RespEnvelope = kz_json:delete_key(<<"page_size">>, cb_context:resp_envelope(Context1)),
            {Status, cb_context:set_resp_envelope(Context1, RespEnvelope)}
    end.

-spec subscription_does_exist2(kz_term:ne_binary(), cb_context:context()) -> {'exists' | 'not_exists' | 'error', cb_context:context()}.
subscription_does_exist2(DeviceId, Context) ->
    ViewContext = crossbar_doc:load_view(?BY_DEVICE, [{'key', DeviceId}], Context, fun normalize_view_results/2),
    case {cb_context:has_errors(ViewContext), cb_context:doc(ViewContext)} of
        {'true', _} -> {'error', ViewContext};
        {'false', []} -> {'not_exists', Context};
        {'false', [Subscription|[]]} ->
            lager:debug("found push notification subscription: ~p", [Subscription]),
            SubscriptionContext = cb_context:store(crossbar_doc:handle_datamgr_success(Subscription, ViewContext), 'db_doc', Subscription),
            {'exists', SubscriptionContext};
        {'false', [_|_]} ->
            lager:error("database inconsistency: multiple docs for one mobile device push notification subscription"),
            %% TODO better error message
            {'error', crossbar_util:response('error', <<"datastore inconsistency: duplicate docs">>, Context)}
    end.

%%------------------------------------------------------------------------------
%% @doc Respond with a "bad identifier" response, the same way it would be
%% returned for docs not found when using `crossbar_doc:load'.
%% @end
%%------------------------------------------------------------------------------
-spec response_not_found(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
response_not_found(DeviceId, BadContext) ->
    cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, DeviceId}]), BadContext).
