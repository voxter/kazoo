%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc Handles Mattermost requests for Kazoo
%%%
%%%
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_mattermost).
-behaviour(gen_server).

-export([start_link/0
        ,auth/1
        ,users_summary/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).
-define(PROC_TABLE, 'mattermost_servers').

-define(MOD_CONFIG_CAT, <<"crossbar.mattermost">>).
-define(ACCOUNT_TIMEOUT, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"timeout_ms">>, 30000)).

-record(state, {}).
-type state() :: #state{}.

%%%=============================================================================
%%% Public methods
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec auth(cb_context:context()) -> cb_context:context().
auth(Context) ->
    call('auth', Context).

-spec users_summary(cb_context:context()) -> cb_context:context().
users_summary(Context) ->
    call('users_summary', Context).

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    lager:info("starting crossbar_mattermost"),
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

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
    process_flag('trap_exit', true),
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
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec call(atom(), cb_context:context()) -> cb_context:context().
call(Method, Context) ->
    AccountId = cb_context:account_id(Context),
    AccountServer = gen_server:call(?SERVER, {'get_account_server', AccountId, cb_context:req_id(Context)}),
    try gen_server:call(AccountServer, {Method, Context}, ?ACCOUNT_TIMEOUT) of
        NewContext -> NewContext
    catch
        _:{'timeout', _} ->
            lager:error("request to mattermost gen_server for account ~s timed out", [AccountId]),
            crossbar_util:response_datastore_timeout(Context)
    end.
