%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_hero_preferences).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ,post/1
        ]).

-include("crossbar.hrl").

-define(BY_USER, <<"hero_preferences/by_user">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.hero_preferences">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.hero_preferences">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.hero_preferences">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.hero_preferences">>, ?MODULE, 'post').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource
%%
%% For example:
%%
%% ```
%%    /hero_preferences => [].
%%    /hero_preferences/foo => [<<"foo">>]
%%    /hero_preferences/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /hero_preferences mights load a list of hero_preference objects
%% /hero_preferences/123 might load the hero_preference object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    case cb_context:auth_user_id(Context) of
        'undefined' ->
            lager:debug("User not authenticated, failing preferences request"),
            crossbar_util:response_401(Context);
        _User ->
            %% Does this override the request parameters
            lager:debug("Getting hero preferences for user"),
            ViewContext = crossbar_doc:load_view(?BY_USER, [{'key', cb_context:auth_user_id(Context)}], Context, fun normalize_view_results/2),
            validate_hero_preferences(ViewContext)
    end.

-spec validate_hero_preferences(cb_context:context()) -> cb_context:context().
validate_hero_preferences(Context) ->
    case cb_context:has_errors(Context) of
        'true' -> crossbar_util:response_db_fatal(Context);
        'false' -> validate_hero_preferences(Context, cb_context:req_verb(Context))
    end.

-spec validate_hero_preferences(cb_context:context(), http_method()) -> cb_context:context().
validate_hero_preferences(Context, ?HTTP_GET) ->
    lager:debug("validating hero preferences for user - GET"),
    get_or_default(Context, cb_context:doc(Context));
validate_hero_preferences(Context, ?HTTP_POST) ->
    lager:debug("validating hero preferences for user - POST"),
    create_or_update(Context, cb_context:doc(Context)).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db
%% save (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    lager:debug("Saving doc"),
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc Checks if the authenticated user has preferences already. If they
%% do not have preferences return the default preferences, otherwise
%% return their preferences.
%% @end
%%------------------------------------------------------------------------------
-spec get_or_default(cb_context:context(), kz_json:objects()) -> cb_context:context().
get_or_default(Context, []) ->
    lager:debug("No preferences found for user, returning default preferences"),
    BlankContext = cb_context:set_doc(Context, kz_json:new()),
    DefaultContext = default(BlankContext),
    cb_context:set_resp_data(DefaultContext, kz_doc:public_fields(cb_context:doc(DefaultContext)));
get_or_default(Context, [Doc|[]]) ->
    lager:debug("User has custom preferences, returning them"),
    cb_context:set_resp_data(Context, kz_doc:public_fields(Doc));
get_or_default(Context, List) ->
    lager:error("unanticipated view state for user preferences: user - ~s, view: ~p", [cb_context:auth_user_id(Context), List]),
    crossbar_util:response_db_fatal(Context).
%%------------------------------------------------------------------------------
%% @doc Checks if the authenticated user has preferences already. If they
%% do not have preferences create a new document and set the new
%% preferences, otherwise update their preferences.
%% @end
%%------------------------------------------------------------------------------
-spec create_or_update(cb_context:context(), kz_json:objects()) -> cb_context:context().
create_or_update(Context, []) ->
    lager:debug("No preferences found for user, creating object"),
    NewContext = cb_context:set_doc(Context, kz_json:new()),
    default(NewContext);
create_or_update(Context, [Doc|[]]) ->
    lager:debug("User has custom preferences, updating them: ~p", Doc),
    UpdatedContext = cb_context:set_doc(Context, Doc),
    update(kz_json:get_value(<<"_id">>, Doc), cb_context:set_resp_data(UpdatedContext, kz_doc:public_fields(Doc)));
create_or_update(Context, List) ->
    lager:error("unanticipated view state for user preferences: user - ~s, view: ~p", [cb_context:auth_user_id(Context), List]),
    crossbar_util:response_db_fatal(Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the default data
%% @end
%%------------------------------------------------------------------------------
-spec default(cb_context:context()) -> cb_context:context().
default(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"hero_preferences">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"hero_preferences">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    lager:debug("Create validated successfully"),
    NewDoc = lists:foldl(fun(F, Doc) -> F(Doc) end, cb_context:doc(Context),
                         [fun(X) -> kz_json:set_value(<<"pvt_user_id">>, cb_context:auth_user_id(Context), X) end
                         ,fun(X) -> kz_doc:set_type(X, <<"hero_preference">>) end
                         ]),
    lager:debug("Setting new document: ~p", [NewDoc]),
    cb_context:set_doc(Context, NewDoc);
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"hero_preference">>)).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
