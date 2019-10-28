%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communications Inc
%%% @doc Listing of all expected v1 callbacks
%%%
%%%
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_hero_releases).

-export([init/0
        ,allowed_methods/0, allowed_methods/2
        ,resource_exists/0, resource_exists/2
        ,authorize/1
        ,validate_resource/1, validate_resource/3
        ,validate/1, validate/3
        ,post/3
        ]).

-include("crossbar.hrl").

-define(ERS_AUTH_TOKEN_CACHE_KEY, 'ers_auth_token').

-define(MOD_CONFIG_CAT, <<"hero">>).

-define(ERS_CONFIG_KEY, <<"electron_release_server">>).
-define(ERS_CONFIG_KEY_BASE_URL, <<"base_url">>).
-define(ERS_CONFIG_KEY_USERNAME, <<"username">>).
-define(ERS_CONFIG_KEY_PASSWORD, <<"password">>).

-define(ERS_CONFIG_MAX_DEFER_TIME_S
       ,kapps_config:get_non_neg_integer(?MOD_CONFIG_CAT, [?ERS_CONFIG_KEY, <<"max_defer_time_s">>], 30 * ?SECONDS_IN_DAY)
       ).
-define(MAX_DEFER_TIME_FRIENDLY_TEXT, <<"30 days">>).

-define(PROMOTE, <<"promote">>).
-define(UPDATE_AVAILABILITY, <<"update_availability">>).
-define(UPDATE_NOTES, <<"update_notes">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.hero_releases">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.authorize.hero_releases">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.hero_releases">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate_resource.hero_releases">>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"*.validate.hero_releases">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.hero_releases">>, ?MODULE, 'post').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_VersionId, ?PROMOTE) ->
    [?HTTP_POST];
allowed_methods(_VersionId, ?UPDATE_AVAILABILITY) ->
    [?HTTP_POST];
allowed_methods(_VersionId, ?UPDATE_NOTES) ->
    [?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean() | {'stop', cb_context:context()}.
authorize(Context) ->
    case cb_context:is_account_admin(Context)
        orelse cb_context:is_superduper_admin(Context)
    of
        'true' -> 'true';
        'false' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end.

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource
%%
%% For example:
%%
%% ```
%%    /hero_releases => [].
%%    /hero_releases/foo => [<<"foo">>]
%%    /hero_releases/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_VersionId, ?PROMOTE) -> 'true';
resource_exists(_VersionId, ?UPDATE_AVAILABILITY) -> 'true';
resource_exists(_VersionId, ?UPDATE_NOTES) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns and Resource Ids are valid.
%%
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) ->
    case cb_context:account_id(Context) of
        'undefined' -> cb_context:add_system_error('not_found', Context);
        _ -> validate_ers_config_exists(Context)
    end.

-spec validate_resource(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_resource(Context, VersionId, _) ->
    %% All validation of list plus more
    Context1 = validate_resource(Context),
    case cb_context:resp_status(Context1) of
        'success' -> load_version(Context, VersionId);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /hero_releases mights load a list of hero_release objects
%% /hero_releases/123 might load the hero_preference object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    AccountId = cb_context:account_id(Context),
    case ers_api_req('get', <<"/api/version?flavor=", AccountId/binary>>) of
        {'ok', 200, _, RespJObjs} ->
            RespJObjs1 = lists:map(fun sanitize_version/1, RespJObjs),
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_resp_data/2, RespJObjs1}
                               ]);
        {'error', StatusCode, _, Body} -> handle_ers_error(Context, StatusCode, Body);
        {'error', _} -> crossbar_util:response_db_fatal(Context)
    end.

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, _VersionId, ?PROMOTE) ->
    check_version_already_released(Context);
validate(Context, _VersionId, ?UPDATE_AVAILABILITY) ->
    case cb_context:req_value(Context, <<"availability">>) of
        'undefined' -> missing_required_field_error(Context, <<"availability">>);
        Availability ->
            OnSuccess = fun() -> check_cap_future_availability(Context, Availability) end,
            validate_type(<<"availability">>
                         ,Availability
                         ,{<<"iso8601">>, fun kz_time:is_iso8601/1}
                         ,OnSuccess
                         ,Context
                         )
    end;
validate(Context, _VersionId, ?UPDATE_NOTES) ->
    case cb_context:req_value(Context, <<"notes">>) of
        'undefined' -> missing_required_field_error(Context, <<"notes">>);
        Notes ->
            OnSuccess = fun() -> cb_context:set_resp_status(Context, 'success') end,
            validate_type(<<"notes">>, Notes, {<<"string">>, fun is_binary/1}, OnSuccess, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db
%% save (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, VersionId, ?PROMOTE) ->
    NowISO8601 = kz_time:maybe_add_iso8601_ms_suffix(kz_time:iso8601(kz_time:now_s())),
    post_update_field(Context, VersionId, <<"availability">>, NowISO8601);
post(Context, VersionId, ?UPDATE_AVAILABILITY) ->
    Availability = cb_context:req_value(Context, <<"availability">>),
    post_update_field(Context, VersionId, <<"availability">>, Availability);
post(Context, VersionId, ?UPDATE_NOTES) ->
    Notes = cb_context:req_value(Context, <<"notes">>),
    post_update_field(Context, VersionId, <<"notes">>, Notes).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Fetch system-wide configuration for Electron Release Server (ERS).
%% @end
%%------------------------------------------------------------------------------
-spec ers_config() -> kz_term:api_object().
ers_config() ->
    kapps_config:get_json(?MOD_CONFIG_CAT, ?ERS_CONFIG_KEY).

%%------------------------------------------------------------------------------
%% @doc Validate that system-wide configuration for ERS exists.
%% @end
%%------------------------------------------------------------------------------
-spec validate_ers_config_exists(cb_context:context()) -> cb_context:context().
validate_ers_config_exists(Context) ->
    case ers_config() of
        'undefined' -> error_ers_not_configured(Context);
        ERSConfig -> validate_ers_config(Context, ERSConfig)
    end.

%%------------------------------------------------------------------------------
%% @doc Validate that system-wide configuration for ERS has the necessary fields
%% defined and that their types are correct.
%% @end
%%------------------------------------------------------------------------------
-spec validate_ers_config(cb_context:context(), kz_json:object()) -> cb_context:context().
validate_ers_config(Context, ERSConfig) ->
    Validator = fun(Key, JObj) -> kz_json:get_ne_binary_value(Key, JObj) =/= 'undefined' end,
    Validation = validate_ers_config_fields(ERSConfig
                                           ,[?ERS_CONFIG_KEY_BASE_URL
                                            ,?ERS_CONFIG_KEY_USERNAME
                                            ,?ERS_CONFIG_KEY_PASSWORD
                                            ]
                                           ,Validator
                                           ),

    case Validation of
        'true' -> Context;
        'false' -> error_ers_not_configured(Context)
    end.

-type ers_type_validator() :: fun((kz_json:key(), kz_json:object()) -> boolean()).

-spec validate_ers_config_fields(kz_json:object(), kz_term:ne_binaries(), ers_type_validator()) -> boolean().
validate_ers_config_fields(_, [], _) -> 'true';
validate_ers_config_fields(ERSConfig, [Key|Keys], Validator) ->
    case Validator(Key, ERSConfig) of
        'true' -> validate_ers_config_fields(ERSConfig, Keys, Validator);
        'false' -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Set the response to an error stating that release management is not
%% configured.
%% @end
%%------------------------------------------------------------------------------
-spec error_ers_not_configured(cb_context:context()) -> cb_context:context().
error_ers_not_configured(Context) ->
    cb_context:add_system_error('release_management_not_configured', Context).

%%------------------------------------------------------------------------------
%% @doc Load version data for a specified version into the context. Verifies
%% that the version belongs to the current account ID. A 404 is returned if the
%% current account does not own it to avoid leaking information about other
%% flavors.
%% @end
%%------------------------------------------------------------------------------
-spec load_version(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_version(Context, VersionId) ->
    case try_get_version(VersionId) of
        {'ok', VersionJObj} ->
            OnSuccess = fun() ->
                                cb_context:set_doc(Context, VersionJObj)
                        end,
            validate_flavor_ownership(
              cb_context:account_id(Context)
             ,kz_json:get_ne_binary_value([<<"flavor">>, <<"name">>], VersionJObj)
             ,OnSuccess
             ,Context
             );
        {'error', 'not_found'} -> cb_context:add_system_error('not_found', Context);
        {'error', _} -> crossbar_util:response_db_fatal(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Get data about a specific version from ERS.
%% @end
%%------------------------------------------------------------------------------
-spec try_get_version(kz_term:ne_binary()) -> {'ok', kz_json:json_object()} | {'error', any()}.
try_get_version(VersionId) ->
    case ers_api_req('get', <<"/api/version/", VersionId/binary>>) of
        {'ok', 200, _, RespJObj} -> {'ok', RespJObj};
        {'error', 404, _, _} -> {'error', 'not_found'};
        {'error', _, _, Body} -> {'error', decode_body(Body)};
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Validate that a version flavor belongs to a specific account ID.
%% @end
%%------------------------------------------------------------------------------
-type validation_success_callback() :: fun(() -> cb_context:context()).

-spec validate_flavor_ownership(kz_term:ne_binary(), kz_term:ne_binary(), validation_success_callback(), cb_context:context()) ->
                                       cb_context:context().
validate_flavor_ownership(AccountId, AccountId, OnSuccess, _) -> OnSuccess();
validate_flavor_ownership(AccountId, OwnerAccountId, _, Context) ->
    lager:info("version is owned by ~s not ~s", [OwnerAccountId, AccountId]),
    cb_context:add_system_error('not_found', Context).

%%------------------------------------------------------------------------------
%% @doc Validate that requested availability does not exceed the max defer time
%% range since version release.
%% @end
%%------------------------------------------------------------------------------
-spec check_cap_future_availability(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
check_cap_future_availability(Context, Availability) ->
    VersionCreatedAt = kz_json:get_ne_binary_value(<<"createdAt">>, cb_context:doc(Context)),
    VersionCreatedAtTs = iso8601_to_gregorian_seconds(VersionCreatedAt),
    AvailabilityTs = iso8601_to_gregorian_seconds(Availability),
    case AvailabilityTs > VersionCreatedAtTs + ?ERS_CONFIG_MAX_DEFER_TIME_S of
        'true' ->
            MaxAvailability = gregorian_seconds_to_iso8601(
                                VersionCreatedAtTs + ?ERS_CONFIG_MAX_DEFER_TIME_S
                               ),
            max_defer_exceeded_field_error(Context
                                          ,VersionCreatedAt
                                          ,MaxAvailability
                                          );
        'false' -> check_version_already_released(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Validate that the req is not trying to update the availability on a
%% version that is already rolled out/released.
%% @end
%%------------------------------------------------------------------------------
-spec check_version_already_released(cb_context:context()) -> cb_context:context().
check_version_already_released(Context) ->
    VersionAvailability = kz_json:get_ne_binary_value(<<"availability">>, cb_context:doc(Context)),
    VersionAvailabilityTs = iso8601_to_gregorian_seconds(VersionAvailability),
    case VersionAvailabilityTs < kz_time:current_tstamp() of
        'true' -> version_already_released_error(Context, VersionAvailability);
        'false' -> cb_context:set_resp_status(Context, 'success')
    end.

%%------------------------------------------------------------------------------
%% @doc Validate that a field type matches the expected type. The OnSuccess
%% callback is called if it matches. Otherwise, the context is updated with an
%% error about the type mismatch.
%% @end
%%------------------------------------------------------------------------------
-type type_validator() :: fun((any()) -> boolean()).

-spec validate_type(kz_term:ne_binary(), kz_json:json_term(), {kz_term:ne_binary(), type_validator()}, validation_success_callback(), cb_context:context()) ->
                           cb_context:context().
validate_type(Field, Value, {ExpectedType, Validator}, OnSuccess, Context) ->
    case Validator(Value) of
        'true' -> OnSuccess();
        'false' -> invalid_type_field_error(Context, Field, Value, ExpectedType)
    end.

%%------------------------------------------------------------------------------
%% @doc Update a context with an error about a type mismatch.
%% @end
%%------------------------------------------------------------------------------
-spec invalid_type_field_error(cb_context:context(), kz_term:ne_binary(), kz_json:json_term(), kz_term:ne_binary()) ->
                                      cb_context:context().
invalid_type_field_error(Context, Field, Value, ExpectedType) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Value did not match type(s): ", ExpectedType/binary>>}
            ,{<<"target">>, ExpectedType}
            ,{<<"value">>, Value}
            ]),
    cb_context:add_validation_error(Field, <<"type">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc Update a context with an error about a missing field in the req data.
%% @end
%%------------------------------------------------------------------------------
-spec missing_required_field_error(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
missing_required_field_error(Context, Field) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Field is required but missing">>}
            ,{<<"value">>, Field}
            ]),
    cb_context:add_validation_error(Field, <<"required">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc Update a context with an error about a version having already been
%% rolled out/released.
%% @end
%%------------------------------------------------------------------------------
-spec version_already_released_error(cb_context:context(), kz_term:ne_binary()) ->
                                            cb_context:context().
version_already_released_error(Context, VersionAvailability) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Availability cannot be changed after the version has been released">>}
            ,{<<"availability">>, VersionAvailability}
            ]),
    cb_context:add_validation_error(<<"availability">>, <<"invalid">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc Update a context with an error about the requested change of
%% availability exceeding the max defer time since the version's release.
%% @end
%%------------------------------------------------------------------------------
-spec max_defer_exceeded_field_error(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                            cb_context:context().
max_defer_exceeded_field_error(Context, VersionCreatedAt, MaxAvailability) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Availability cannot be more than "
                              ,(?MAX_DEFER_TIME_FRIENDLY_TEXT)/binary
                              ," after the version was created"
                             >>}
            ,{<<"created_at">>, VersionCreatedAt}
            ,{<<"maximum">>, MaxAvailability}
            ]),
    cb_context:add_validation_error(<<"availability">>, <<"maximum">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc POST a version update to ERS, with the specified field updated with a
%% new value.
%% @end
%%------------------------------------------------------------------------------
-spec post_update_field(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), any()) ->
                               cb_context:context().
post_update_field(Context, VersionId, Field, Value) ->
    JObj = prune_version_post_data(cb_context:doc(Context)),
    JObj1 = kz_json:set_value(Field, Value, JObj),
    case ers_authed_api_req('post', <<"/api/version/", VersionId/binary>>, JObj1) of
        {'ok', 200, _, RespJObj} -> handle_ers_version_success(Context, RespJObj);
        {'error', StatusCode, _, Body} -> handle_ers_error(Context, StatusCode, Body);
        {'error', _} -> crossbar_util:response_db_fatal(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Strip out properties from POST data that are not acceptable by the ERS
%% API.
%% @end
%%------------------------------------------------------------------------------
-spec prune_version_post_data(kz_json:object()) -> kz_json:object().
prune_version_post_data(JObj) ->
    %% Need to strip out assets from this payload, the API doesn't like it
    kz_json:delete_key(<<"assets">>, JObj).

%%------------------------------------------------------------------------------
%% @doc Perform an HTTP req against ERS without a body.
%% @end
%%------------------------------------------------------------------------------
-type ers_api_ret() :: {'ok', 200, kz_http:headers(), kz_json:json_term()} |
                       {'error', pos_integer(), kz_http:headers(), kz_json:json_term()} |
                       {'error', any()}.

-spec ers_api_req(kz_http:method(), kz_term:ne_binary()) -> ers_api_ret().
ers_api_req(Method, Route) ->
    ers_api_req(Method, Route, [], 'undefined').

%%------------------------------------------------------------------------------
%% @doc Perform an HTTP req against ERS with the specified headers and body.
%% @end
%%------------------------------------------------------------------------------
-spec ers_api_req(kz_http:method(), kz_term:ne_binary(), kz_term:proplist(), kz_json:api_object()) ->
                         ers_api_ret().
ers_api_req(Method, Route, Headers, Body) ->
    ERSConfig = ers_config(),
    BaseUrl = kz_json:get_ne_binary_value(?ERS_CONFIG_KEY_BASE_URL, ERSConfig),
    Url = kz_term:to_list(<<BaseUrl/binary, Route/binary>>),

    case Method of
        'get' -> handle_http_ret(kz_http:req(Method, Url, Headers));
        'post' ->
            Headers1 = [{"Content-Type", "application/json"} | Headers],
            handle_http_ret(kz_http:req(Method, Url, Headers1, kz_json:encode(Body)))
    end.

%%------------------------------------------------------------------------------
%% @doc Perform an HTTP req against ERS with the specified body. The request
%% automatically includes the secure route auth token or fetches one for use in
%% the request as necessary.
%% @end
%%------------------------------------------------------------------------------
-spec ers_authed_api_req(kz_http:method(), kz_term:ne_binary(), kz_json:object()) ->
                                ers_api_ret().
ers_authed_api_req(Method, Route, BodyJObj) ->
    ers_authed_api_req(Method, Route, BodyJObj, 1).

-spec ers_authed_api_req(kz_http:method(), kz_term:ne_binary(), kz_json:object(), kz_term:non_neg_integer()) ->
                                ers_api_ret().
ers_authed_api_req(Method, Route, BodyJObj, Retries) ->
    case kz_cache:fetch_local(?CACHE_NAME, {?MODULE, ?ERS_AUTH_TOKEN_CACHE_KEY}) of
        {'ok', Token} -> try_ers_authed_api_req(Method, Route, BodyJObj, Retries, Token);
        {'error', 'not_found'} -> ers_api_auth_and_repeat(Method, Route, BodyJObj)
    end.

%%------------------------------------------------------------------------------
%% @doc Perform an HTTP req against ERS with the specified body. If the request
%% fails due to a 403 and there are remaining retries, will re-auth with ERS and
%% replay the req.
%% @end
%%------------------------------------------------------------------------------
-spec try_ers_authed_api_req(kz_http:method(), kz_term:ne_binary(), kz_json:object(), kz_term:non_neg_integer(), kz_term:ne_binary()) ->
                                    ers_api_ret().
try_ers_authed_api_req(Method, Route, BodyJObj, Retries, Token) ->
    Headers = [{"Authorization", "Bearer " ++ kz_term:to_list(Token)}],
    lager:debug("BodyJObj ~p", [BodyJObj]),

    case ers_api_req(Method, Route, Headers, BodyJObj) of
        {'ok', 403, _, _} when Retries > 0 ->
            %% Re-attempt request in case the cached token had expired
            kz_cache:erase_local(?CACHE_NAME, {?MODULE, ?ERS_AUTH_TOKEN_CACHE_KEY}),
            lager:info("cleared cached ERS auth token, reauthenticating and replaying request"),
            ers_api_auth_and_repeat(Method, Route, BodyJObj);
        Ret -> handle_http_ret(Ret)
    end.

%%------------------------------------------------------------------------------
%% @doc Authenticate against ERS, acquiring a token. Then replay an
%% authenticated ERS req.
%% @end
%%------------------------------------------------------------------------------
-spec ers_api_auth_and_repeat(kz_http:method(), kz_term:ne_binary(), kz_json:object()) ->
                                     ers_api_ret().
ers_api_auth_and_repeat(Method, Route, BodyJObj) ->
    ERSConfig = ers_config(),

    case ers_api_req('post', <<"/api/auth/login">>
                    ,[{"Content-Type", "application/json"}]
                    ,kz_json:from_list([{<<"username">>, kz_json:get_ne_binary_value(?ERS_CONFIG_KEY_USERNAME, ERSConfig)}
                                       ,{<<"password">>, kz_json:get_ne_binary_value(?ERS_CONFIG_KEY_PASSWORD, ERSConfig)}
                                       ])
                    )
    of
        {'ok', 200, _, RespJObj} ->
            Token = kz_json:get_ne_binary_value(<<"token">>, RespJObj),
            lager:info("acquired new ERS auth token"),
            kz_cache:store_local(?CACHE_NAME
                                ,{?MODULE, ?ERS_AUTH_TOKEN_CACHE_KEY}
                                ,Token
                                ),
            try_ers_authed_api_req(Method, Route, BodyJObj, 0, Token);
        Ret -> handle_http_ret(Ret)
    end.

%%------------------------------------------------------------------------------
%% @doc Format responses from kz_http and decode JSON bodies where possible.
%% @end
%%------------------------------------------------------------------------------
-spec handle_http_ret(kz_http:ret()) -> ers_api_ret().
handle_http_ret({'ok', 200, RespHeaders, Body}) ->
    {'ok', 200, RespHeaders, decode_body(Body)};
handle_http_ret({'ok', StatusCode, RespHeaders, Body}) ->
    {'error', StatusCode, RespHeaders, decode_body(Body)};
handle_http_ret({'error', _}=E) -> E.

%%------------------------------------------------------------------------------
%% @doc If possible, decode the body of an ERS HTTP resp as JSON. Falls back to
%% returning the data as an Erlang binary.
%% @end
%%------------------------------------------------------------------------------
-spec decode_body(kz_term:text()) -> kz_json:json_term().
decode_body(Body) ->
    Body1 = kz_term:to_binary(Body),
    try kz_json:unsafe_decode(Body1) of
        JTerm -> JTerm
    catch
        {'invalid_json', _, _} -> Body1
    end.

%%------------------------------------------------------------------------------
%% @doc Set successful resp status and include sanitized resp data from the req
%% sent to ERS.
%% @end
%%------------------------------------------------------------------------------
-spec handle_ers_version_success(cb_context:context(), kz_json:object()) -> cb_context:context().
handle_ers_version_success(Context, JObj) ->
    JObj1 = sanitize_version(JObj),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, JObj1}
                       ]).

%%------------------------------------------------------------------------------
%% @doc Map generic HTTP errors from ERS API reqs to a Crossbar API resp.
%% @end
%%------------------------------------------------------------------------------
-spec handle_ers_error(cb_context:context(), pos_integer(), kz_json:json_term()) -> cb_context:context().
handle_ers_error(Context, StatusCode, Error) ->
    crossbar_util:response('error', 'undefined', StatusCode, Error, Context).

%%------------------------------------------------------------------------------
%% @doc Filter and modify fields from an ERS version model to those safe to
%% send to the Crossbar user.
%% @end
%%------------------------------------------------------------------------------
-spec sanitize_version(kz_json:object()) -> kz_json:object().
sanitize_version(JObj) ->
    Version = kz_json:get_ne_binary_value(<<"name">>, JObj),
    Assets = lists:map(fun(Asset) ->
                               sanitize_asset(
                                 Asset
                                ,kz_json:get_ne_binary_value([<<"flavor">>, <<"name">>], JObj)
                                ,Version
                                )
                       end
                      ,kz_json:get_list_value(<<"assets">>, JObj)
                      ),

    kz_json:from_list(
      [{<<"id">>, kz_json:get_ne_binary_value(<<"id">>, JObj)}
      ,{<<"assets">>, Assets}
      ,{<<"availability">>, kz_json:get_ne_binary_value(<<"availability">>, JObj)}
      ,{<<"channel">>, kz_json:get_ne_binary_value([<<"channel">>, <<"name">>], JObj)}
      ,{<<"created_at">>, kz_json:get_ne_binary_value(<<"createdAt">>, JObj)}
      ,{<<"name">>, Version}
      ,{<<"notes">>, kz_json:get_binary_value(<<"notes">>, JObj)}
      ]).

%%------------------------------------------------------------------------------
%% @doc Filter and modify fields from an ERS asset model to those safe to send
%% to the Crossbar user.
%% @end
%%------------------------------------------------------------------------------
-spec sanitize_asset(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
sanitize_asset(JObj, Flavor, Version) ->
    Platform = kz_json:get_ne_binary_value(<<"platform">>, JObj),
    BaseUrl = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, [?ERS_CONFIG_KEY, ?ERS_CONFIG_KEY_BASE_URL]),
    kz_json:from_list(
      [{<<"platform">>, Platform}
      ,{<<"download_url">>, <<BaseUrl/binary
                             ,"/download/flavor/", Flavor/binary
                             ,"/", Version/binary
                             ,"/", Platform/binary
                            >>}
      ]).

%%------------------------------------------------------------------------------
%% @doc Convert an ISO 8601 datetime string into a gregorian timestamp.
%% @end
%%------------------------------------------------------------------------------
-spec iso8601_to_gregorian_seconds(kz_term:ne_binary()) -> kz_time:gregorian_seconds().
iso8601_to_gregorian_seconds(ISO8601) ->
    kz_time:to_gregorian_seconds(
      kz_time:from_iso8601(
        kz_time:trim_iso8601_ms(ISO8601)
       )
     ,<<"UTC">>
     ).

%%------------------------------------------------------------------------------
%% @doc Convert a gregorian timestamp into an ISO 8601 datetime string.
%% @end
%%------------------------------------------------------------------------------
-spec gregorian_seconds_to_iso8601(kz_time:gregorian_seconds()) -> kz_term:ne_binary().
gregorian_seconds_to_iso8601(Ts) ->
    kz_time:maybe_add_iso8601_ms_suffix(
      kz_time:iso8601(Ts)
     ).
