%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% @author Ben Partridge
%%% @end
%%%-----------------------------------------------------------------------------
-module(nv_fcm_server_sup).

-behaviour(supervisor).

-include("navi.hrl").

%% API
-export([start_link/2
        ,get_living_children/1
        ,push/5
        ]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_json:object(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(ServerConfig, MyName) ->
    supervisor:start_link({'local', kz_term:to_atom(MyName, 'true')}, ?MODULE, [ServerConfig]).

-spec push(supervisor:sup_ref(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
push(Super, _ServerName, RegistrationId, Msg, ExtraParameters) ->
    %% Should be exactly one child if it has not crashed, in which case we will have restarted it
    [{_Id, ChildPid}] = get_living_children(Super),
    nv_fcm_server:push(ChildPid, RegistrationId, Msg, ExtraParameters),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc gets the supervisor's child. Can be used to determine
%% if the notification server is alive
%% @end
%%------------------------------------------------------------------------------
-spec get_living_children(supervisor:sup_ref()) -> [{supervisor:child_id(), supervisor:child()}].
get_living_children(Super) ->
    [{Id, Pid} || {Id, Pid, _Type, _Modules} <- supervisor:which_children(Super), Pid =/= 'undefined'].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([ServerConfig]) ->
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [?WORKER_ARGS('nv_fcm_server', [ServerConfig])]}}.
