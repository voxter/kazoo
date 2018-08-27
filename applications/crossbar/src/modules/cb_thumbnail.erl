%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc Get thumbnails for user photos
%%%
%%%
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_thumbnail).

-export([init/0
        ,authenticate/1
        ,authorize/1
        ,allowed_methods/0
        ,content_types_provided/1
        ,resource_exists/0
        ,validate/1
        ,validate_users/3
        ]).

-include("crossbar.hrl").

-define(THUMBNAIL, <<"thumbnail">>).
-define(PHOTO, <<"photo">>).

-define(MOD_CONFIG_CAT, <<"crossbar.thumbnails">>).

-define(DIM_X, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"dim_x">>, 90)).
-define(DIM_Y, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"dim_y">>, 90)).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.thumbnail">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.thumbnail">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.thumbnail">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.thumbnail">>, ?MODULE, 'validate'),
    %% Hook into thumbnail change
    _ = crossbar_bindings:bind(<<"*.validate.users">>, ?MODULE, 'validate_users'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authenticate(req_nouns(), http_method()) -> boolean().
authenticate([{?THUMBNAIL, []}, {<<"users">>, [_UserId]}, {<<"accounts">>, [_AccountId]}], ?HTTP_GET) ->
    'true';
authenticate(_Nouns, _Verb) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), req_nouns(), http_method()) -> boolean().
authorize(_Context, [{?THUMBNAIL, []}, {<<"users">>, [_UserId]}, {<<"accounts">>, [_AccountId]}], ?HTTP_GET) ->
    'true';
authorize(_Context, _Nouns, _Verb) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    cb_context:set_content_types_provided(Context, [{'to_binary', [{<<"application">>, <<"octet-stream">>}
                                                                  ,{<<"application">>, <<"base64">>}
                                                                  ]}
                                                   ]).

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_thumbnail(Context, cb_context:req_verb(Context)).

validate_thumbnail(Context, ?HTTP_GET) ->
    validate_thumbnail(Context, cb_context:user_id(Context), ?HTTP_GET).
validate_thumbnail(Context, 'undefined', ?HTTP_GET) ->
    crossbar_util:response_faulty_request(Context);
validate_thumbnail(Context, UserId, ?HTTP_GET) ->
    lager:debug("loading user doc"),
    NewContext = crossbar_doc:load(UserId, Context, ?TYPE_CHECK_OPTION(kzd_user:type())),
    case cb_context:has_errors(NewContext) of
        'true' -> NewContext;
        'false' -> load_thumbnail(NewContext)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_thumbnail(cb_context:context()) -> cb_context:context().
load_thumbnail(Context) ->
    Attachments = kz_doc:attachments(cb_context:doc(Context)),
    load_thumbnail(Context, kz_json:get_value(?PHOTO, Attachments), kz_json:get_value(?THUMBNAIL, Attachments)).

-spec load_thumbnail(cb_context:context(), kz_json:object() | 'undefined', kz_json:object() | 'undefined') -> cb_context:context().
load_thumbnail(Context, 'undefined', _ThumbnailJObj) ->
    lager:debug("no photo, so no thumbnail"),
    crossbar_util:response_faulty_request(Context);
load_thumbnail(Context, _PhotoJObj, 'undefined') ->
    lager:debug("photo but no thumbnail, converting"),
    UserId = cb_context:user_id(Context),
    NewContext = crossbar_doc:load_attachment(UserId, ?PHOTO, ?TYPE_CHECK_OPTION(kzd_user:type()), Context),
    case cb_context:has_errors(NewContext) of
        'true' -> NewContext;
        'false' -> set_thumbnail(Context, cb_context:resp_data(NewContext))
    end;
load_thumbnail(Context, _PhotoJObj, _ThumbnailJObj) ->
    lager:debug("photo and thumbnail! returning existing"),
    NewContext = crossbar_doc:load_attachment(cb_context:doc(Context), ?THUMBNAIL, ?TYPE_CHECK_OPTION(kzd_user:type()), Context),
    set_resp_headers(NewContext).

-spec set_resp_headers(cb_context:context()) -> cb_context:context().
set_resp_headers(Context) ->
    Headers = #{<<"Content-Disposition">> => <<"attachment; filename=", (?THUMBNAIL)/binary>>
               ,<<"Content-Type">> => kz_doc:attachment_content_type(cb_context:doc(Context), ?THUMBNAIL)
               },
    cb_context:add_resp_headers(Context, Headers).

-spec set_thumbnail(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
set_thumbnail(Context, FullSizeImage) ->
    UserId = cb_context:user_id(Context),
    case convert_thumbnail(FullSizeImage) of
        {'ok', ThumbnailImage} ->
            Opts = [{'content_type', <<"application/octet-stream">>} | ?TYPE_CHECK_OPTION(kzd_user:type())],
            NewContext = crossbar_doc:save_attachment(UserId, ?THUMBNAIL, ThumbnailImage, Context, Opts),
            set_resp_headers(cb_context:set_resp_data(NewContext, ThumbnailImage));
        {'error', Reason} ->
            crossbar_util:response('error', <<"error">>, 500, Reason, Context)
    end.

-spec convert_thumbnail(kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()} | {'error', kz_term:ne_binary()}.
convert_thumbnail(FullSizeImage) ->
    InputFile = lib:nonl(os:cmd("mktemp")),
    OutputFile = lib:nonl(os:cmd("mktemp")),
    case file:write_file(InputFile, FullSizeImage) of
        'ok' ->
            case gm:convert(InputFile, OutputFile, [{'thumbnail', ?DIM_X, ?DIM_Y}]) of
                'ok' ->
                    Resp = case file:read_file(OutputFile) of
                               {'ok', _Content}=Ret ->
                                   Ret;
                               {'error', Reason} ->
                                   lager:warning("failed to read converted file ~s with reason", [OutputFile, Reason]),
                                   {'error', <<"error reading converted file">>}
                           end,
                    'ok' = file:delete(InputFile),
                    'ok' = file:delete(OutputFile),
                    Resp;
                {'error', Reason} ->
                    lager:warning("error converting file ~s with reason ~s", [InputFile, Reason]),
                    'ok' = file:delete(InputFile),
                    %% Probably won't succeed unless the conversion was partially successful
                    _ = file:delete(OutputFile),
                    {'error', <<"converting file">>}
            end;
        {'error', Reason} ->
            lager:warning("error writing file ~s with reason ~s", [InputFile, Reason]),
            {'error', <<"writing image to convert">>}
    end.

-spec validate_users(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_users(Context, UserId, ?PHOTO) ->
    validate_users_photo(Context, UserId, cb_context:req_verb(Context));
validate_users(Context, _UserId, _Thing) ->
    Context.

-spec validate_users_photo(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
validate_users_photo(Context, UserId, ?HTTP_POST) ->
    delete_thumbnail(Context, UserId);
validate_users_photo(Context, UserId, ?HTTP_DELETE) ->
    delete_thumbnail(Context, UserId);
validate_users_photo(Context, _UserId, _Method) ->
    Context.

-spec delete_thumbnail(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
delete_thumbnail(Context, UserId) ->
    %% Don't fail the whole API request if the thumbnail doesn't exist!
    _ = crossbar_doc:delete_attachment(UserId, ?THUMBNAIL, Context),
    Context.
