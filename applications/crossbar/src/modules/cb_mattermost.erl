%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%%
%%%
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_mattermost).

-export([init/0
        ,authorize/2
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ]).

-include("crossbar.hrl").

-define(USERS, <<"users_map">>).
-define(TEAMS, <<"teams_map">>).
-define(CB_LIST_TEAMS, <<"mate/listing_by_mattermost_team_id">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_module_sup:start_child('crossbar_mattermost'),
    _ = crossbar_bindings:bind(<<"*.authorize.mattermost">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.mattermost">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.mattermost">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.mattermost">>, ?MODULE, 'validate').

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, ?TEAMS) ->
    cb_context:is_superduper_admin(Context).

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?USERS) -> [?HTTP_GET];
allowed_methods(?TEAMS) -> [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource
%%
%% For example:
%%
%% ```
%%    /skels => [].
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true' | 'false'.
resource_exists(?USERS) -> 'true';
resource_exists(?TEAMS) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels mights load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    crossbar_mattermost:auth(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?USERS) ->
    crossbar_mattermost:users_summary(Context);
validate(Context, ?TEAMS) ->
    teams_summary(cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB)).

-spec teams_summary(cb_context:context()) -> cb_context:context().
teams_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST_TEAMS, [], Context, fun normalize_teams_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Normalizes the resuts of a view
%% @end
%%------------------------------------------------------------------------------
-spec normalize_teams_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_teams_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
