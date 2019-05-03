%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc Handles Mattermost requests for Kazoo
%%%
%%%
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_mattermost_account).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).

-define(CB_LIST_MATTERMOST, <<"users/mattermost_listing">>).

-define(MOD_CONFIG_CAT, <<"crossbar.mattermost">>).
-define(MATTERMOST_ID_KEY, <<"pvt_mattermost_id">>).
-define(MATTERMOST_EMAIL_KEY, <<"pvt_mattermost_email">>).
-define(MATTERMOST_PASSWORD_KEY, <<"pvt_mattermost_password">>).

-define(MATTERMOST_ROOT_URL, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"mattermost_url">>, <<"https://c-chat.conversanthq.com/api">>)).
-define(MATTERMOST_ADMIN_TOKEN_KEY, <<"mattermost_admin_token">>).
-define(MATTERMOST_ADMIN_TOKEN, kapps_config:get_binary(?MOD_CONFIG_CAT, ?MATTERMOST_ADMIN_TOKEN_KEY)).
-define(MATTERMOST_ADMIN_USER, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"mattermost_admin_user">>, <<"change_me">>)).
-define(MATTERMOST_ADMIN_PASSWORD, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"mattermost_admin_password">>, <<"change_me">>)).
-define(MATTERMOST_NOTIFY_PROPS, kapps_config:get_json(?MOD_CONFIG_CAT, <<"mattermost_notify_props">>, kz_json:from_list([{<<"push">>, <<"all">>}
                                                                                                                         ,{<<"push_status">>, <<"online">>}
                                                                                                                         ]))).


-record(state, {}).
-type state() :: #state{}.

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(AccountId) ->
    lager:info("starting crossbar_mattermost_account for ~s", [AccountId]),
    gen_server:start_link(?SERVER, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'auth', Context}, _From, State) ->
    put_callid(Context),
    NewContext = ensure_team_exists(Context, fun(C) -> auth(C) end),
    reset_callid(),
    {'reply', NewContext, State};
handle_call({'users_summary', Context}, _From, State) ->
    put_callid(Context),
    NewContext = ensure_team_exists(Context, fun(C) -> users_summary(C) end),
    reset_callid(),
    {'reply', NewContext, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("crossbar_mattermost_account terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
-type context_callback() :: fun((cb_context:context()) -> cb_context:context()).

-spec ensure_team_exists(cb_context:context(), context_callback()) -> cb_context:context().
ensure_team_exists(Context, OnSuccess) ->
    AccountDoc = cb_context:account_doc(Context),
    TeamId = kz_json:get_value(?MATTERMOST_ID_KEY, AccountDoc),
    ensure_team_exists(Context, OnSuccess, TeamId).

-spec ensure_team_exists(cb_context:context(), context_callback(), kz_term:api_binary()) -> cb_context:context().
ensure_team_exists(Context, OnSuccess, 'undefined') ->
    AccountId = cb_context:account_id(Context),
    JObj = kz_json:from_list([{<<"name">>, AccountId}
                             ,{<<"display_name">>, AccountId}
                              %% Invite only
                             ,{<<"type">>, <<"I">>}
                             ]),
    lager:debug("creating mattermost team for account ~p", [AccountId]),
    case mattermost_authed_req('post', "/v4/teams", JObj) of
        {'ok', 201, _, EncodedResp} ->
            TeamId = kz_json:get_value(<<"id">>, kz_json:decode(EncodedResp)),
            AccountDoc = kz_json:set_value(?MATTERMOST_ID_KEY
                                          ,TeamId
                                          ,cb_context:account_doc(Context)
                                          ),
            case kzd_accounts:save(AccountDoc) of
                {'ok', _} ->
                    ensure_team_exists(Context, OnSuccess, TeamId);
                _Err ->
                    lager:error("failed to set mattermost team id on account doc, but the team was created"),
                    lager:error("account ~p will be unable to chat until this is resolved", [AccountId]),
                    crossbar_util:response_conflicting_docs(Context)
            end;
        {'ok', 400, _, EncodedResp} ->
            case kz_json:get_value(<<"id">>, kz_json:decode(EncodedResp)) of
                <<"store.sql_team.save.domain_exists.app_error">> ->
                    lager:error("team already exists!"),
                    crossbar_util:response('error', <<"team already exists">>, Context);
                _ ->
                    lager:error("mattermost team creation with status 400"),
                    lager:info("failing resp data ~p", [EncodedResp]),
                    crossbar_util:response('error', <<"error creating team">>, Context)
            end;
        Resp ->
            lager:error("mattermost team creation failed for an unknown reason"),
            lager:debug("failing resp ~p", [Resp]),
            crossbar_util:response('error', <<"error creating team">>, Context)
    end;
ensure_team_exists(Context, OnSuccess, TeamId) ->
    OnSuccess(cb_context:store(Context, 'team_id', TeamId)).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec users_summary(cb_context:context()) -> cb_context:context().
users_summary(Context) ->
    ViewContext = crossbar_doc:load_view(?CB_LIST_MATTERMOST, [], Context, fun normalize_users_view_results/2),
    {UsersExist, UsersNotExist} = lists:partition(fun mattermost_user_exists/1, cb_context:doc(ViewContext)),
    %% Create mattermost users which don't already exist in parallel
    Created = plists:map(fun(U) -> user_summary(Context, U) end, UsersNotExist),
    %% Combine those which already existed with newly created
    Resp = [user_summary(Context, U) || U <- UsersExist] ++ Created,
    crossbar_util:response(Resp, ViewContext).

-spec user_summary(cb_context:context(), kzd_user:doc()) -> kz_json:object().
user_summary(Context, PartialUserDoc) ->
    user_summary(Context, PartialUserDoc, mattermost_user_exists(PartialUserDoc)).

-spec user_summary(cb_context:context(), kzd_user:doc(), boolean()) -> kz_json:object().
user_summary(_Context, PartialUserDoc, 'true') ->
    kz_json:from_list([{<<"user_id">>, kz_doc:id(PartialUserDoc)}
                      ,{<<"mattermost_user_id">>, kz_json:get_value(?MATTERMOST_ID_KEY, PartialUserDoc)}
                      ]);
user_summary(Context, PartialUserDoc, 'false') ->
    case ensure_mattermost_user(Context, kz_doc:id(PartialUserDoc)) of
        {'ok', UserDoc} ->
            user_summary(Context, UserDoc, 'true');
        {'error', Reason} ->
            kz_json:from_list([{<<"user_id">>, kz_doc:id(PartialUserDoc)}
                              ,{<<"error">>, Reason}
                              ])
    end.

%% Partial user docs are fine - pvt_mattermost_id
-spec mattermost_user_exists(kzd_user:doc()) -> boolean().
mattermost_user_exists(UserDoc) ->
    kz_json:get_value(?MATTERMOST_ID_KEY, UserDoc) =/= 'undefined'.

%%------------------------------------------------------------------------------
%% @doc Normalizes the resuts of a view
%% @end
%%------------------------------------------------------------------------------
-spec normalize_users_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_users_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec auth(cb_context:context()) -> cb_context:context().
auth(Context) ->
    auth(Context, cb_context:auth_user_id(Context)).

-spec auth(cb_context:context(), kz_term:ne_binary() | kzd_user:doc()) -> cb_context:context().
auth(Context, UserId) when is_binary(UserId) ->
    case ensure_mattermost_user(Context, UserId) of
        {'ok', Doc} -> auth(Context, Doc);
        {'error', _Error} -> crossbar_util:response('error', <<"error setting up chat user">>, 500, Context)
    end;
auth(Context, UserDoc) ->
    DeviceId = cb_context:req_param(Context, <<"device_id">>),
    UserAgent = cb_context:req_header(Context, <<"user-agent">>),
    case auth_mattermost_user(kz_json:get_value(?MATTERMOST_EMAIL_KEY, UserDoc), kz_json:get_value(?MATTERMOST_PASSWORD_KEY, UserDoc), DeviceId, UserAgent) of
        {'ok', Token} ->
            Resp = kz_json:from_list([{<<"token">>, Token}
                                     ,{<<"team_id">>, cb_context:fetch(Context, 'team_id')}
                                     ]),
            crossbar_util:response(Resp, Context);
        {'error', _Resp} ->
            crossbar_util:response('error', <<"error authenticating chat user">>, 500, Context)
    end.

-spec ensure_mattermost_user(cb_context:context(), kzd_user:doc() | kz_term:ne_binary()) -> {'ok', kzd_user:doc()} | {'error', atom()}.
ensure_mattermost_user(Context, UserId) when is_binary(UserId) ->
    {'ok', UserDoc} = kzd_user:fetch(cb_context:account_db(Context), UserId),
    ensure_mattermost_user(Context, UserDoc);
ensure_mattermost_user(Context, UserDoc) ->
    ensure_mattermost_user(Context, UserDoc, kz_json:get_value(?MATTERMOST_ID_KEY, UserDoc)).

-spec ensure_mattermost_user(cb_context:context(), kzd_user:doc(), kz_term:api_binary()) -> {'ok', kzd_user:doc()} | {'error', atom()}.
ensure_mattermost_user(Context, UserDoc, 'undefined') ->
    create_user(Context, UserDoc);
ensure_mattermost_user(_Context, UserDoc, _MattermostId) ->
    {'ok', UserDoc}.

-spec create_user(cb_context:context(), kzd_user:doc()) -> {'ok', kzd_user:doc()} | {'error', atom()}.
create_user(Context, UserDoc) ->
    Password = base64:encode(crypto:strong_rand_bytes(20)),
    ReqData = kz_json:from_list([{<<"email">>, generate_email(UserDoc)}
                                ,{<<"username">>, kz_doc:id(UserDoc)}
                                ,{<<"password">>, Password}
                                ,{<<"notify_props">>, ?MATTERMOST_NOTIFY_PROPS}
                                ]),
    case mattermost_authed_req('post', "/v3/users/create", ReqData) of
        {'ok', 200, _, RespData} ->
            lager:debug("successfully created mattermost user in mattermost"),
            %% Add the password to the response before passing it along (to match the false info in the docs)
            add_user_to_team(Context, UserDoc, kz_json:set_value(<<"password">>, Password, kz_json:decode(RespData)));
        {'ok', 400, _, RespData}=Resp ->
            case kz_json:get_value(<<"id">>, kz_json:decode(RespData)) of
                <<"store.sql_user.save.username_exists.app_error">> ->
                    lager:error("user already exists in mattermost"),
                    {'error', 'chat_user_already_exists'};
                _ ->
                    lager:error("failed to create mattermost user with response ~p", [Resp]),
                    {'error', 'create_chat_user'}
            end;
        Resp ->
            lager:error("failed to create mattermost user with response ~p", [Resp]),
            {'error', 'create_chat_user'}
    end.

-spec generate_email(kzd_user:doc()) -> kz_term:ne_binary().
generate_email(UserDoc) ->
    kz_term:to_binary([kz_doc:id(UserDoc), <<"@">>, kz_doc:id(UserDoc)]).

-spec add_user_to_team(cb_context:context(), kzd_user:doc(), kz_json:object()) -> {'ok', kzd_user:doc()} | {'error', atom()}.
add_user_to_team(Context, UserDoc, MattermostUserData) ->
    ReqData = kz_json:from_list([{<<"user_id">>, kz_json:get_value(<<"id">>, MattermostUserData)}]),
    Path = "/v3/teams/" ++ kz_term:to_list(cb_context:fetch(Context, 'team_id')) ++ "/add_user_to_team",
    case mattermost_authed_req('post', Path, ReqData) of
        {'ok', 200, _, _RespData} ->
            lager:debug("successfully added mattermost user to team"),
            update_user_kazoo(Context, UserDoc, MattermostUserData);
        Resp ->
            lager:error("failed to add mattermost user to team with response ~p", [Resp]),
            {'error', 'add_user_to_team'}
    end.

-spec update_user_kazoo(cb_context:context(), kzd_user:doc(), kz_json:object()) -> {'ok', kzd_user:doc()} | {'error', atom()}.
update_user_kazoo(Context, UserDoc, MattermostUserData) ->
    Updates = [{?MATTERMOST_ID_KEY, kz_json:get_value(<<"id">>, MattermostUserData)}
              ,{?MATTERMOST_EMAIL_KEY, kz_json:get_value(<<"email">>, MattermostUserData)}
              ,{?MATTERMOST_PASSWORD_KEY, kz_json:get_value(<<"password">>, MattermostUserData)}
              ],
    kz_datamgr:update_doc(cb_context:account_db(Context), kz_doc:id(UserDoc), [{'update', Updates}]).

-spec mattermost_authed_req(atom(), string(), kz_json:term()) -> kz_http:resp().
mattermost_authed_req(Method, Path, Data) ->
    AuthToken = ?MATTERMOST_ADMIN_TOKEN,
    case AuthToken =/= 'undefined'
        andalso mattermost_api_req(Method, Path, Data, [auth_token_header(AuthToken)]) of
        'false' -> auth_and_retry(Method, Path, Data);
        {'ok', 401, _, _} -> auth_and_retry(Method, Path, Data);
        Resp -> Resp
    end.

-spec auth_and_retry(atom(), string(), kz_json:object()) -> kz_http:resp().
auth_and_retry(Method, Path, Data) ->
    {'ok', Token} = auth_mattermost_user(?MATTERMOST_ADMIN_USER, ?MATTERMOST_ADMIN_PASSWORD),
    kapps_config:set_default(?MOD_CONFIG_CAT, ?MATTERMOST_ADMIN_TOKEN_KEY, Token),
    mattermost_api_req(Method, Path, Data, [auth_token_header(Token)]).

-spec auth_token_header(kz_term:ne_binary()) -> {string(), string()}.
auth_token_header(Token) when is_binary(Token) ->
    {"Authorization", kz_term:to_list(<<"Bearer ", Token/binary>>)}.

-spec auth_mattermost_user(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()} | {'error', kz_http:resp()}.
auth_mattermost_user(LoginId, Password) ->
    auth_mattermost_user(LoginId, Password, 'undefined', 'undefined').

-spec auth_mattermost_user(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary()) -> {'ok', kz_term:ne_binary()} | {'error', kz_http:resp()}.
auth_mattermost_user(LoginId, Password, DeviceId, UserAgent) ->
    JObj = kz_json:from_list(props:filter_undefined([{<<"login_id">>, LoginId}
                                                    ,{<<"password">>, Password}
                                                    ,{<<"device_id">>, DeviceId}
                                                    ])),
    ReqHeaders = props:filter_undefined([{"User-Agent", UserAgent}]),
    case mattermost_api_req('post', "/v3/users/login", JObj, ReqHeaders) of
        {'ok', 200, RespHeaders, _TokenDataEncoded} ->
            {'ok', kz_term:to_binary(props:get_value("token", RespHeaders))};
        Resp ->
            lager:error("failed to authenticate user ~p with response ~p", [LoginId, Resp]),
            {'error', Resp}
    end.

-spec mattermost_api_req(atom(), string(), kz_json:term(), kz_term:proplist()) -> kz_http:resp().
mattermost_api_req(Method, Path, Data, AdditionalHeaders) ->
    Headers = [{"Accept", "application/json"}
              ,{"Content-Type", "application/json"}
               | AdditionalHeaders],
    Url = kz_term:to_list(?MATTERMOST_ROOT_URL) ++ Path,
    EncodedData = kz_json:encode(Data),
    lager:debug("sending request to ~p with data ~p", [Url, EncodedData]),
    lager:debug("headers ~p", [Headers]),
    kz_http:req(Method, Url, Headers, EncodedData, []).

-spec put_callid(cb_context:context()) -> 'ok'.
put_callid(Context) ->
    kz_util:put_callid(cb_context:req_id(Context)).

-spec reset_callid() -> 'ok'.
reset_callid() ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID).
