%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc Handles Mattermost requests for Kazoo
%%%
%%%
%%% @author Max Lay
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_mattermost_account).
-behaviour(gen_server).

-export([start_link/1]).

%% called by crossbar_mattermost
-export([update_context_with_team_id/2
        ,create_mm_users/3
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("crossbar.hrl").
-include("crossbar_mattermost.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNT_TIMEOUT, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"timeout_ms">>, 30000)).
-define(MATTERMOST_ADMIN_TOKEN_KEY, <<"mattermost_admin_token">>).
-define(MATTERMOST_ADMIN_TOKEN, kapps_config:get_binary(?MOD_CONFIG_CAT, ?MATTERMOST_ADMIN_TOKEN_KEY)).
-define(MATTERMOST_ADMIN_USER, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"mattermost_admin_user">>, <<"change_me">>)).
-define(MATTERMOST_ADMIN_PASSWORD, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"mattermost_admin_password">>, <<"change_me">>)).
-define(MATTERMOST_NOTIFY_PROPS, kapps_config:get_json(?MOD_CONFIG_CAT, <<"mattermost_notify_props">>, kz_json:from_list([{<<"push">>, <<"all">>}
                                                                                                                         ,{<<"push_status">>, <<"online">>}
                                                                                                                         ]))).

-record(state, {completed_user_creations = #{} :: #{kz_term:ne_binary() => kz_json:object()} %% UserId => UserDoc
               ,pending_user_creations = sets:new() :: sets:set(kz_term:ne_binary())         %% Users yet to be created
               ,user_creation_pids = [] :: [pid()]                                           %% Procs waiting for a user creation result
               ,user_creation_worker :: kz_term:api_pid()                                    %% Worker performing the MM API reqs to create users
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(AccountId) ->
    lager:info("starting crossbar_mattermost_account for ~s", [AccountId]),
    gen_server:start_link(?SERVER, [], []).

%%------------------------------------------------------------------------------
%% @doc Updates the context with the team ID for the account in the context.
%% Creates the team if it does not exist.
%% @end
%%------------------------------------------------------------------------------
-spec update_context_with_team_id(pid(), cb_context:context()) -> cb_context:context().
update_context_with_team_id(Srv, Context) ->
    gen_server:call(Srv, {'update_context_with_team_id', Context}, ?ACCOUNT_TIMEOUT).

%%------------------------------------------------------------------------------
%% @doc Requests creation of the Mattermost users corresponding to the supplied
%% Kazoo user IDs.
%% @end
%%------------------------------------------------------------------------------
-spec create_mm_users(pid(), kz_term:ne_binaries(), cb_context:context()) -> {boolean(), kz_json:objects()}.
create_mm_users(Srv, UserIds, Context) ->
    gen_server:call(Srv, {'create_mm_users', UserIds, Context}, ?ACCOUNT_TIMEOUT).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag('trap_exit', 'true'),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'update_context_with_team_id', Context}, _From, State) ->
    put_callid(Context),
    NewContext = pvt_update_context_with_team_id(Context),
    reset_callid(),
    {'reply', NewContext, State};
handle_call({'create_mm_users', UserIds, Context}, From, #state{completed_user_creations=CUCs
                                                               ,pending_user_creations=PUCs
                                                               ,user_creation_pids=UCPids
                                                               ,user_creation_worker=UCWorker
                                                               }=State) ->
    put_callid(Context),

    %% All jobs that are pending or fulfilled, but not yet delivered to requestors
    AlreadySubmitted = sets:union(sets:from_list(maps:keys(CUCs)), PUCs),
    %% New users that haven't been requested since the last delivery
    ExtraUserIds = sets:subtract(sets:from_list(UserIds), AlreadySubmitted),
    lager:info("~p requested creation of extra users: ~p", [From, sets:to_list(ExtraUserIds)]),
    %% New set of unfulfilled user creation jobs
    PUCs1 = sets:union(PUCs, ExtraUserIds),

    reset_callid(),

    %% Spawn a new user creation worker if there isn't one running
    ReqId = cb_context:req_id(Context),
    AccountDb = cb_context:account_db(Context),
    TeamId = cb_context:fetch(Context, 'team_id'),
    UCWorker1 = maybe_spawn_worker(UCWorker, ReqId, AccountDb, TeamId, sets:to_list(PUCs1)),

    {'noreply', State#state{pending_user_creations=PUCs1
                           ,user_creation_pids=[From | UCPids]
                           ,user_creation_worker=UCWorker1
                           }};
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
handle_info({'batch', {ReqId, _, _}=WorkerArgs, CreateResults}, State) ->
    kz_util:put_callid(ReqId),
    {'noreply', handle_batch_results(WorkerArgs, CreateResults, State)};
handle_info({'EXIT', UCWorker, Reason}, #state{user_creation_worker=UCWorker}=State) ->
    lager:error("user creation worker ~p exited before it was done (reason: ~s)", [UCWorker, Reason]),
    %% TODO: it might be nice for the worker to deliver results as it goes so that if it crashes,
    %% the undelivered results are not lost
    {'noreply', State#state{user_creation_worker='undefined'}};
handle_info({'EXIT', _, _}, State) ->
    {'noreply', State};
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

%%------------------------------------------------------------------------------
%% @doc Updates `Context' with the MM team ID associated with the auth account.
%% The MM team is automatically created it if doesn't exist yet.
%% @end
%%------------------------------------------------------------------------------
-spec pvt_update_context_with_team_id(cb_context:context()) -> cb_context:context().
pvt_update_context_with_team_id(Context) ->
    AccountDoc = cb_context:account_doc(Context),
    TeamId = kz_json:get_value(?MATTERMOST_ID_KEY, AccountDoc),
    pvt_update_context_with_team_id(Context, TeamId).

-spec pvt_update_context_with_team_id(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
pvt_update_context_with_team_id(Context, 'undefined') ->
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
                    pvt_update_context_with_team_id(Context, TeamId);
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
pvt_update_context_with_team_id(Context, TeamId) ->
    cb_context:set_resp_status(cb_context:store(Context, 'team_id', TeamId), 'success').

%%------------------------------------------------------------------------------
%% @doc Create a MM user. If the user already exists, makes sure they are a
%% member of `TeamId'.
%% @end
%%------------------------------------------------------------------------------
-spec prepare_user(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kzd_user:doc()} | {'error', kzd_user:doc(), atom()}.
prepare_user(AccountDb, TeamId, UserId) ->
    {'ok', UserDoc} = kzd_user:fetch(AccountDb, UserId),
    case teamless(UserDoc) of
        'true' ->
            %% This can happen if the user was created successfully, but the team assignment req failed
            lager:debug("user ~s was teamless, add them to ~s", [UserId, TeamId]),
            MMUserId = kz_json:get_ne_binary_value(?MATTERMOST_ID_KEY, UserDoc),
            wrap(add_user_to_team(AccountDb, TeamId, UserId, MMUserId), UserDoc);
        'false' ->
            wrap(create_user(AccountDb, TeamId, UserDoc), UserDoc)
    end.

-spec teamless(kzd_user:doc()) -> boolean().
teamless(UserDoc) ->
    kz_json:is_defined(?MATTERMOST_ID_KEY, UserDoc)
        andalso not kz_json:is_defined(?MATTERMOST_TEAM_ID_KEY, UserDoc).

wrap({'ok', _}=OK, _) -> OK;
wrap({'error', Reason}, UserDoc) -> {'error', UserDoc, Reason}.

-spec create_user(kz_term:ne_binary(), kz_term:ne_binary(), kzd_user:doc()) -> {'ok', kzd_user:doc()} | {'error', atom()}.
create_user(AccountDb, TeamId, UserDoc) ->
    UserId = kz_doc:id(UserDoc),

    lager:info("attempting to create user ~s(~s) in MM team ~s", [UserId, AccountDb, TeamId]),

    Password = base64:encode(crypto:strong_rand_bytes(20)),
    ReqData = kz_json:from_list([{<<"email">>, generate_email(UserDoc)}
                                ,{<<"username">>, UserId}
                                ,{<<"password">>, Password}
                                ,{<<"notify_props">>, ?MATTERMOST_NOTIFY_PROPS}
                                ]),
    case mattermost_authed_req('post', "/v3/users/create", ReqData) of
        {'ok', 200, _, RespData} ->
            lager:debug("successfully created mattermost user in mattermost"),
            %% Add the password to the response before passing it along (to match the false info in the docs)
            update_user_kazoo(AccountDb, TeamId, UserId, kz_json:set_value(<<"password">>, Password, kz_json:decode(RespData)));
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

-spec add_user_to_team(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kzd_user:doc()} | {'error', atom()}.
add_user_to_team(AccountDb, TeamId, UserId, MMUserId) ->
    ReqData = kz_json:from_list([{<<"user_id">>, MMUserId}]),
    Path = "/v3/teams/" ++ kz_term:to_list(TeamId) ++ "/add_user_to_team",
    case mattermost_authed_req('post', Path, ReqData) of
        {'ok', 200, _, _RespData} ->
            lager:debug("successfully added mattermost user to team"),
            Updates = [{?MATTERMOST_TEAM_ID_KEY, TeamId}],
            kz_datamgr:update_doc(AccountDb, UserId, [{'update', Updates}]);
        Resp ->
            lager:error("failed to add mattermost user to team with response ~p", [Resp]),
            {'error', 'add_user_to_team'}
    end.

-spec update_user_kazoo(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          {'ok', kzd_user:doc()} | {'error', atom()}.
update_user_kazoo(AccountDb, TeamId, UserId, MattermostUserData) ->
    MMUserId = kz_json:get_ne_binary_value(<<"id">>, MattermostUserData),
    Updates = [{?MATTERMOST_ID_KEY, MMUserId}
              ,{?MATTERMOST_EMAIL_KEY, kz_json:get_value(<<"email">>, MattermostUserData)}
              ,{?MATTERMOST_PASSWORD_KEY, kz_json:get_value(<<"password">>, MattermostUserData)}
              ],
    case kz_datamgr:update_doc(AccountDb, UserId, [{'update', Updates}]) of
        {'ok', _} ->
            add_user_to_team(AccountDb, TeamId, UserId, MMUserId);
        {'error', Reason}=E ->
            lager:error("failed to set credentials on user ~s(~s) (reason: ~p), but the MM user was created", [UserId, AccountDb, Reason]),
            lager:error("user ~s will be unable to chat until this is resolved", [UserId]),
            E
    end.

-spec mattermost_authed_req(atom(), string(), kz_json:term()) -> kz_http:resp().
mattermost_authed_req(Method, Path, Data) ->
    AuthToken = ?MATTERMOST_ADMIN_TOKEN,
    case AuthToken =/= 'undefined'
        andalso crossbar_mattermost:mattermost_api_req(Method, Path, Data, [auth_token_header(AuthToken)]) of
        'false' -> auth_and_retry(Method, Path, Data);
        {'ok', 401, _, _} -> auth_and_retry(Method, Path, Data);
        Resp -> Resp
    end.

-spec auth_and_retry(atom(), string(), kz_json:object()) -> kz_http:resp().
auth_and_retry(Method, Path, Data) ->
    {'ok', Token} = crossbar_mattermost:auth_mattermost_user(?MATTERMOST_ADMIN_USER, ?MATTERMOST_ADMIN_PASSWORD),
    kapps_config:set_default(?MOD_CONFIG_CAT, ?MATTERMOST_ADMIN_TOKEN_KEY, Token),
    crossbar_mattermost:mattermost_api_req(Method, Path, Data, [auth_token_header(Token)]).

-spec auth_token_header(kz_term:ne_binary()) -> {string(), string()}.
auth_token_header(Token) when is_binary(Token) ->
    {"Authorization", kz_term:to_list(<<"Bearer ", Token/binary>>)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
maybe_spawn_worker('undefined', ReqId, AccountDb, TeamId, PUCs1) ->
    spawn_worker({ReqId, AccountDb, TeamId}, PUCs1);
maybe_spawn_worker(ExistingWorker, _, _, _, _) ->
    ExistingWorker.

%%------------------------------------------------------------------------------
%% @doc Spawn a user creation worker and link to the calling process.
%% @end
%%------------------------------------------------------------------------------
spawn_worker({ReqId, AccountDb, TeamId}=WorkerArgs, PUCs) ->
    Parent = self(),
    spawn_link(fun() ->
                       kz_util:put_callid(ReqId),
                       lager:debug("started a user creation worker in ~p", [self()]),
                       %% TODO: possible optimization to use view with minimal data to avoid fetch of each user doc
                       CreateResults = [prepare_user(AccountDb, TeamId, UserId) || UserId <- PUCs],
                       Parent ! {'batch', WorkerArgs, CreateResults}
               end).

%%------------------------------------------------------------------------------
%% @doc Handle user creation results returned by a user creation worker. If all
%% pending jobs are done, the requestors are notified of the results. If there
%% are new pending jobs since the worker started, another worker is started for
%% those jobs.
%% @end
%%------------------------------------------------------------------------------
handle_batch_results(WorkerArgs, [], #state{pending_user_creations=PUCs}=State) ->
    case sets:size(PUCs) of
        0 -> notify_user_creation_pids(State);
        _ ->
            lager:debug("there are more pending jobs, spawning another user creation worker"),
            UCWorker = spawn_worker(WorkerArgs, sets:to_list(PUCs)),
            State#state{user_creation_worker=UCWorker}
    end;
handle_batch_results(WorkerArgs
                    ,[CreateResult|CreateResults]
                    ,#state{completed_user_creations=CUCs
                           ,pending_user_creations=PUCs
                           }=State) ->
    JObj = user_summary(CreateResult),
    UserId = kz_json:get_ne_binary_value(<<"user_id">>, JObj),
    %% Update map of results by user ID
    CUCs1 = CUCs#{UserId => JObj},
    %% Remove user ID from pending jobs
    PUCs1 = sets:del_element(UserId, PUCs),
    State1 = State#state{completed_user_creations=CUCs1
                        ,pending_user_creations=PUCs1
                        },
    handle_batch_results(WorkerArgs, CreateResults, State1).

%%------------------------------------------------------------------------------
%% @doc Notify all waiting requestors of completed user creations.
%% @end
%%------------------------------------------------------------------------------
notify_user_creation_pids(#state{completed_user_creations=CUCs
                                ,user_creation_pids=UCPids
                                }=State) ->
    lager:debug("notifying ~b requestors of ~b created users", [length(UCPids), maps:size(CUCs)]),
    lists:foreach(fun(UCPid) ->
                          %% TODO: fix reply value based on all or only some successful
                          gen_server:reply(UCPid, {'true', maps:values(CUCs)})
                  end, UCPids),
    State#state{completed_user_creations=#{}
               ,user_creation_pids=[]
               ,user_creation_worker='undefined'
               }.

%%------------------------------------------------------------------------------
%% @doc Format user creation results.
%% @end
%%------------------------------------------------------------------------------
user_summary({'ok', UserDoc}) ->
    crossbar_mattermost:user_summary(UserDoc);
user_summary({'error', PartialUserDoc, Reason}) ->
    kz_json:from_list([{<<"user_id">>, kz_doc:id(PartialUserDoc)}
                      ,{<<"error">>, Reason}
                      ]).

-spec put_callid(cb_context:context()) -> 'ok'.
put_callid(Context) ->
    kz_util:put_callid(cb_context:req_id(Context)).

-spec reset_callid() -> 'ok'.
reset_callid() ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID).
