%%%-------------------------------------------------------------------
%%% @author Cody Antcliffe <cody.antcliffe@ooma.com>
%%% @copyright (C) 2019-, Voxter Communications
%%% @doc Implements an RBAC solution
%%% @end
%%%-------------------------------------------------------------------
-module(cb_rbac).

%% API
-export([init/0
        ,authorize/1, authorize/2
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<"rbac">>).

-define(HTTP_METHODS_CONFIG_KEY, <<"http_methods">>).
-define(RESTRICTED_ENDPOINTS_CONFIG_KEY, <<"restricted_endpoints">>).
-define(FULL_ACCESS_USER_IDS_CONFIG_KEY, <<"full_access_user_ids">>).
-define(RBAC_CONFIG, kz_json:get_json_value(<<"default">>, kapps_config_doc:config_with_defaults(?MOD_CONFIG_CAT))).
%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize').

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource. Otherwise, request is blocked.
%% @end
%%------------------------------------------------------------------------------

%% We will either block the request using 'stop', or defer authorization to other modules
-type authorize_return() :: 'false' | {'stop', cb_context:context()}.

-spec authorize(cb_context:context()) -> authorize_return().
authorize(Context) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> 'false';
        'false' -> validate_access_rules(Context)
    end.

-spec authorize(cb_context:context(), path_token()) -> authorize_return().
authorize(Context, _Id) -> authorize(Context).

%%------------------------------------------------------------------------------
%% @doc Checks access rules to see if this request is authorized.
%% @end
%%------------------------------------------------------------------------------
-spec validate_access_rules(cb_context:context()) -> authorize_return().
validate_access_rules(Context) ->
    Config = ?RBAC_CONFIG,
    case check_if_user_has_full_access(Context, Config) of
        'true' -> 'false';
        'false' -> check_if_http_request_allowed(Context, Config)
    end.

%%------------------------------------------------------------------------------
%% @doc Checks if user id is included in the full_access_user_ids field
%% of the rbac config.
%% @end
%%------------------------------------------------------------------------------
-spec check_if_user_has_full_access(cb_context:context(), kz_json:objects()) -> boolean().
check_if_user_has_full_access(Context, Config) ->
    AuthUserId = cb_context:auth_user_id(Context),
    FullAccessUserIds = kz_json:get_list_value(?FULL_ACCESS_USER_IDS_CONFIG_KEY, Config, []),
    lists:member(AuthUserId, FullAccessUserIds).

%%------------------------------------------------------------------------------
%% @doc Check if this specific API request is permitted
%% Gets the list of restricted request endpoints, and checks if this request is
%% permitted or not.
%% @end
%%------------------------------------------------------------------------------
-spec check_if_http_request_allowed(cb_context:context(), kzd_system_configs:doc()) -> authorize_return().
check_if_http_request_allowed(Context, Config) ->
    %% Get the request URL
    Tokens = api_util:path_tokens(Context),
    RequestUrl = kz_binary:join(Tokens, <<"/">>),

    %% Get the RestrictedEndpoints
    ReqVerb = cb_context:req_verb(Context),
    HttpVerbRules = kz_json:get_json_value([?HTTP_METHODS_CONFIG_KEY, ReqVerb], Config, kz_json:new()),
    RestrictedEndpoints = kz_json:get_list_value(?RESTRICTED_ENDPOINTS_CONFIG_KEY, HttpVerbRules, []),

    %% Check if the request url is allowed
    case is_endpoint_blocked(RestrictedEndpoints, RequestUrl) of
        'false' -> 'false';
        'true' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end .

%%------------------------------------------------------------------------------
%% @doc Builds a proper regex using RestrictedEndpoint and then uses it to check
%% if the request url is blocked.
%% @end
%%------------------------------------------------------------------------------
-spec is_endpoint_blocked(kz_term:ne_binaries(), kz_term:ne_binary()) -> boolean().
is_endpoint_blocked([], _) -> 'false';
is_endpoint_blocked([RestrictedEndpoint|T], RequestUrl) ->
    %% Replace all * wildcards with *(.+), prepend with '^' and append '$'
    RegExp = ["^", re:replace(RestrictedEndpoint, "[*]", "*(.+)", ['global']), "$"],
    case re:run(RequestUrl, RegExp) of
        'nomatch' -> is_endpoint_blocked(T, RequestUrl);
        _ -> 'true'
    end.
