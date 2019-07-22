%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_hero).

-include("crossbar.hrl").

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,post/2
        ]).

-define(APPS, <<"apps">>).
-define(MOD_CONFIG_CAT, <<"hero">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.hero">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.hero">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.hero">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.hero">>, ?MODULE, 'post').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_POST].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?APPS) ->
    [?HTTP_GET, ?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?APPS) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Props = kz_json:to_proplist(cb_context:req_data(Context)),
    case load_views(Context, Props) of
        {'ok', JObj} ->
            crossbar_util:response(JObj, Context)
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?APPS) ->
    validate_apps(Context, cb_context:req_verb(Context)).

-spec validate_apps(cb_context:context(), http_method()) -> cb_context:context().
validate_apps(Context, ?HTTP_GET) ->
    Apps = hero_util:get_apps(Context),
    crossbar_util:response(Apps, Context);
validate_apps(Context, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"hero_update_apps">>, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?APPS) ->
    Apps = cb_context:req_value(Context, ?APPS),
    update_apps(Context, Apps).

-spec update_apps(cb_context:context(), kz_json:api_json_term()) -> cb_context:context().
update_apps(Context, Apps) ->
    AccountId = cb_context:account_id(Context),
    Doc = kapps_account_config:set(AccountId, ?MOD_CONFIG_CAT, ?APPS, Apps),
    UpdatedApps = kz_json:get_value(?APPS, Doc),
    crossbar_util:response(UpdatedApps, Context).

load_views(Context, Views) ->
    load_views(Context, kz_json:new(), Views).
load_views(_Context, JObj, []) ->
    {'ok', JObj};
load_views(Context, JObj, [{View, IdRevJObj} | Remaining]) ->
    Design = <<"hero/", View/binary>>,
    lager:debug("loading view ~s", [Design]),
    %% TODO: restrict databases
    case kz_datamgr:get_results(cb_context:account_db(Context), Design) of
        {'ok', Data} ->
            Res = view_res_proplist(View, Data, IdRevJObj),
            NewJObj = kz_json:set_value(View, Res, JObj),
            load_views(Context, NewJObj, Remaining)
    end.

view_res_proplist(View, Data, IdRevJObj) ->
    view_res_proplist(View, Data, kz_json:to_proplist(IdRevJObj), kz_json:new()).
view_res_proplist(View, [X | Rest], IdRevProps, Accum) ->
    Value = kz_json:get_value(<<"value">>, X),
    Id = kz_doc:id(Value),
    Rev = kz_doc:revision(Value),
    NewAccum = case props:get_value(Id, IdRevProps) of
                   Rev -> Accum;
                   _ -> kz_json:set_value(Id, doc_data(View, Value), Accum)
               end,
    view_res_proplist(View, Rest, props:delete(Id, IdRevProps), NewAccum);
view_res_proplist(View, [], [{MissingId, _MissingRev} | Rest], Accum) ->
    view_res_proplist(View, [], Rest, kz_json:set_value(MissingId, <<"null">>, Accum));
view_res_proplist(_View, [], [], Accum) ->
    Accum.

doc_data(<<"user_statuses">>, Value) ->
    %% Leak pvt_user_id
    doc_data(<<>>, kz_json:set_value(<<"user_id">>, kz_json:get_value(<<"pvt_user_id">>, Value), Value));
doc_data(_View, Value) ->
    Rev = kz_doc:revision(Value),
    kz_json:set_value(<<"rev">>, Rev, kz_doc:public_fields(Value)).
