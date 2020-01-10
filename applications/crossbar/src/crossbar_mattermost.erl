%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc Handles Mattermost requests for Kazoo
%%%
%%%
%%% @author Max Lay
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_mattermost).
-behaviour(gen_server).

-export([start_link/0]).

%% called by cb_mattermost
-export([auth/1
        ,users_summary/1
        ]).

%% called by crossbar_mattermost_account
-export([user_summary/1
        ,auth_mattermost_user/2
        ,mattermost_api_req/4
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

%% TODO: remove these
-export([auth_maybe_create_user/1, handle_user_creation_result/3]).

-include("crossbar.hrl").
-include("crossbar_mattermost.hrl").

-define(SERVER, ?MODULE).
-define(PROC_TABLE, 'mattermost_servers').

-define(CB_LIST_MATTERMOST, <<"users/mattermost_listing">>).

-define(MATTERMOST_ROOT_URL, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"mattermost_url">>, <<"https://c-chat.conversanthq.com/api">>)).

-record(state, {}).
-type state() :: #state{}.

%%%=============================================================================
%%% Public methods
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Attempt to authenticate a mattermost user.
%% @end
%%------------------------------------------------------------------------------
-spec auth(cb_context:context()) -> cb_context:context().
auth(Context) ->
    do_with_team_id(Context, fun auth_maybe_create_user/1).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec users_summary(cb_context:context()) -> cb_context:context().
users_summary(Context) ->
    do_with_team_id(Context, fun get_users_summary/1).

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    lager:info("starting crossbar_mattermost"),
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc Convert a user doc into the expected return type for a user summary.
%% @end
%%------------------------------------------------------------------------------
-spec user_summary(kzd_user:doc()) -> kz_json:object().
user_summary(PartialUserDoc) ->
    kz_json:from_list([{<<"user_id">>, kz_doc:id(PartialUserDoc)}
                      ,{<<"mattermost_user_id">>, kz_json:get_ne_binary_value(?MATTERMOST_ID_KEY, PartialUserDoc)}
                      ]).

%%------------------------------------------------------------------------------
%% @doc Authenticate against Mattermost with the given credentials.
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Perform a custom HTTP request against the Mattermost API.
%% @end
%%------------------------------------------------------------------------------
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

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    %% We will be very careful to handle errors in handle_info
    process_flag('trap_exit', 'true'),
    lager:debug("starting process table"),
    ?PROC_TABLE = ets:new(?PROC_TABLE, ['set', 'named_table']),
    lager:debug("started process table"),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'get_account_server', AccountId, ReqId}, _From, State) ->
    kz_util:put_callid(ReqId),
    Result = case ets:lookup(?PROC_TABLE, AccountId) of
                 [{AccountId, Pid}] ->
                     lager:debug("found ~p for account ~s", [Pid, AccountId]),
                     {'reply', Pid, State};
                 [] ->
                     {'ok', Pid} = crossbar_mattermost_account:start_link(AccountId),
                     lager:debug("started ~p for account ~s", [Pid, AccountId]),
                     ets:insert(?PROC_TABLE, {AccountId, Pid}),
                     {'reply', Pid, State}
             end,
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    Result;
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
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:debug("PID ~p died with reason ~p", [Pid, Reason]),
    case ets:match_object(?PROC_TABLE, {'$1', Pid}) of
        [{AccountId, Pid}] ->
            lager:error("crossbar_mattermost_account died for account ~s", [AccountId]),
            ets:delete(?PROC_TABLE, AccountId),
            {'noreply', State};
        [] ->
            lager:error("linked process we don't know about died"),
            {'stop', 'normal', State}
    end;
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
    lager:error("crossbar_mattermost terminating: ~p", [_Reason]).

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

%%------------------------------------------------------------------------------
%% @doc Call `Method' with arguments as [gen server] ++ Args ++ [Context]. If
%% a timeout is thrown, such as when using `gen_server:call', it will be caught
%% and a failed `cb_context' will be returned.
%% @end
%%------------------------------------------------------------------------------
-spec call(fun(), [any()], cb_context:context()) ->
          {'ok', any()} | {'timeout', cb_context:context()}.
call(Method, Args, Context) ->
    AccountId = cb_context:account_id(Context),
    AccountServer = gen_server:call(?SERVER, {'get_account_server', AccountId, cb_context:req_id(Context)}),
    try apply(Method, [AccountServer | Args ++ [Context]]) of
        Result -> {'ok', Result}
    catch
        _:{'timeout', _} ->
            lager:error("request to mattermost gen_server for account ~s timed out", [AccountId]),
            {'timeout', crossbar_util:response_datastore_timeout(Context)}
    end.

%%------------------------------------------------------------------------------
%% @doc Like `call/3', timeouts return a failed `cb_context'. However,
%% successful calls will cause the `OnSuccess' callback to be executed with the
%% result from the call.
%% @end
%%------------------------------------------------------------------------------
-type call_handle_timeout_success_callback() :: fun((cb_context:context()) -> cb_context:context()).

-spec call_handle_timeout(fun(), [any()], cb_context:context(), call_handle_timeout_success_callback()) ->
          cb_context:context().
call_handle_timeout(Method, Args, Context, OnSuccess) ->
    case call(Method, Args, Context) of
        {'ok', Result} -> OnSuccess(Result);
        {'timeout', Context1} -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Wraps a fun call, ensuring that it is being executed on a context that
%% has been mapped to a MM team id. If not, handles and returns error cases.
%% @end
%%------------------------------------------------------------------------------
-spec do_with_team_id(cb_context:context(), call_handle_timeout_success_callback()) -> cb_context:context().
do_with_team_id(Context, Fun) ->
    call_handle_timeout(
      fun crossbar_mattermost_account:update_context_with_team_id/2
     ,[]
     ,Context
     ,fun(Context1) ->
              case cb_context:has_errors(Context1) of
                  'true' -> Context1;
                  'false' ->
                      lager:debug("using team id ~s", [cb_context:fetch(Context1, 'team_id')]),
                      Fun(Context1)
              end
      end
     ).

%%------------------------------------------------------------------------------
%% @doc Check whether the user has been verified to have an associated MM user.
%% Perform auth if so, otherwise attempt to create the user's MM user.
%% @end
%%------------------------------------------------------------------------------
-spec auth_maybe_create_user(cb_context:context()) -> cb_context:context().
auth_maybe_create_user(Context) ->
    AccountDb = cb_context:account_db(Context),
    AuthUserId = cb_context:auth_user_id(Context),
    {'ok', UserDoc} = kzd_user:fetch(AccountDb, AuthUserId),
    case mattermost_user_ready(UserDoc) of
        'true' -> auth_mm(Context, UserDoc);
        'false' -> wait_for_mm_user_creation(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns true if the user has MM credentials and has been associated with
%% the account team.
%% @end
%%------------------------------------------------------------------------------
-spec mattermost_user_ready(kzd_user:doc()) -> boolean().
mattermost_user_ready(UserDoc) ->
    kz_json:is_defined(?MATTERMOST_ID_KEY, UserDoc)
        andalso kz_json:is_defined(?MATTERMOST_TEAM_ID_KEY, UserDoc).

%%------------------------------------------------------------------------------
%% @doc Attempt to create the auth user's MM user.
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_mm_user_creation(cb_context:context()) -> cb_context:context().
wait_for_mm_user_creation(Context) ->
    AuthUserId = cb_context:auth_user_id(Context),

    call_handle_timeout(
      fun crossbar_mattermost_account:create_mm_users/3
     ,[[AuthUserId]]
     ,Context
     ,fun({_, EachUserResults}) ->
              handle_user_creation_result(Context, AuthUserId, EachUserResults)
      end
     ).

%%------------------------------------------------------------------------------
%% @doc Look through user creation results provided by the account gen_server.
%% If a successful user creation result is found for the auth user, go ahead and
%% perform auth. There may be multiple user creation results because the
%% gen_server now aggregates a unique list of users for which creation has been
%% requested and does them all before notifying the requestors of the outcomes.
%% @end
%%------------------------------------------------------------------------------
-spec handle_user_creation_result(cb_context:context(), kz_term:ne_binary(), kz_json:objects()) ->
          cb_context:context().
handle_user_creation_result(Context, _, []) ->
    crossbar_util:response('error', <<"error setting up chat user">>, 500, Context);
handle_user_creation_result(Context, AuthUserId, [JObj|JObjs]) ->
    case {kz_json:get_ne_binary_value(<<"user_id">>, JObj) =:= AuthUserId
         ,kz_json:is_defined(<<"error">>, JObj)
         }
    of
        {'true', 'false'} ->
            AccountDb = cb_context:account_db(Context),
            {'ok', UserDoc} = kzd_user:fetch(AccountDb, AuthUserId),
            auth_mm(Context, UserDoc);
        _ -> handle_user_creation_result(Context, AuthUserId, JObjs)
    end.

%%------------------------------------------------------------------------------
%% @doc Auth against Mattermost and respond to the HTTP req with the user's MM
%% token and team ID.
%% @end
%%------------------------------------------------------------------------------
-spec auth_mm(cb_context:context(), kzd_user:doc()) -> cb_context:context().
auth_mm(Context, UserDoc) ->
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

%%------------------------------------------------------------------------------
%% @doc Load summary data for all MM users in the account. MM users are created
%% on-the-fly for those that don't already exist.
%% @end
%%------------------------------------------------------------------------------
-spec get_users_summary(cb_context:context()) -> cb_context:context().
get_users_summary(Context) ->
    ViewContext = crossbar_doc:load_view(?CB_LIST_MATTERMOST, [], Context, fun normalize_users_view_results/2),
    {UsersExist, UsersNotExist} = lists:partition(fun mattermost_user_ready/1, cb_context:doc(ViewContext)),
    UserIdsNotExist = lists:map(fun(UserNotExist) -> kz_doc:id(UserNotExist) end, UsersNotExist),

    %% Create mattermost users which don't already exist
    call_handle_timeout(
      fun crossbar_mattermost_account:create_mm_users/3
     ,[UserIdsNotExist]
     ,Context
     ,fun({Success, EachUserResults}) ->
              %% Combine those which already existed with newly created
              Resp = [user_summary(U) || U <- UsersExist] ++ EachUserResults,

              case Success of
                  'true' -> crossbar_util:response(Resp, ViewContext);
                  'false' ->
                      %% TODO: some sort of error message saying some users weren't successfully created
                      crossbar_util:response(Resp, ViewContext)
              end
      end
     ).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of the users/mattermost_listing view
%% @end
%%------------------------------------------------------------------------------
-spec normalize_users_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_users_view_results(JObj, Acc) ->
    [kz_json:get_json_value(<<"value">>, JObj) | Acc].
