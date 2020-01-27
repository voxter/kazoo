%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% @author Ben Partridge
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(navi_module_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,start_server/1
        ,get_living_children/0
        ,push/3
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("navi.hrl").

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @doc Starts a new push server process for the given notification server
%% configuration.
%% @end
%%------------------------------------------------------------------------------
-spec start_server(kz_json:object()) -> kz_types:sup_startchild_ret().
start_server(ServerConfig) ->
    supervisor:start_child(?SERVER, process_server_config(ServerConfig)).

%%------------------------------------------------------------------------------
%% @doc gets the supervisor's children. Can be used to determine
%% if a specific notification server is active
%% @end
%%------------------------------------------------------------------------------
-spec get_living_children() -> [{atom(),atom(),atom()}].
get_living_children() ->
    [{Id, Pid, Module} || {Id, Pid, _Type, Module} <- supervisor:which_children(?SERVER), Pid =/= 'undefined'].

%%------------------------------------------------------------------------------
%% @doc Determines if there is a notification server for the supplied
%% notification type and app. If there is, send the notification.
%% @end
%%------------------------------------------------------------------------------
-spec push(kz_json:object(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok' | 'error'.
push(Registration, Msg, ExtraParameters) ->
    ProcName = get_process_name(Registration),
    lager:debug("Finding process to send push notification to: ~s", [ProcName]),
    case [Pid || {Id, Pid, _Module} <- get_living_children(), Id =:= ProcName] of
        [Pid] ->
            %% Child process exists
            lager:debug("Sending a ~p notification", [ProcName]),
            Supervisor = supervisor_module(Registration),
            RegistrationId = kz_json:get_value(<<"notification_registration_id">>, Registration),
            %% Supervisor shouldn't need both Pid and ProcName, but we don't know which they'll need!
            Supervisor:push(Pid, ProcName, RegistrationId, Msg, ExtraParameters),
            'ok';
        [] ->
            %% Child process does not exit
            lager:error("No process found for notification server: ~p", [ProcName]),
            'error'
    end.

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, get_children_specs()}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_children_specs() -> [supervisor:child_spec()].
get_children_specs() ->
    %% Get all notification service configs from db and create supervisor process for every
    %% notification service required
    [process_server_config(ServerConfig) || ServerConfig <- kapps_config:get_jsons(?CONFIG_CAT, <<"notification_servers">>, [])].

-spec process_server_config(kz_json:object()) -> supervisor:child_spec().
process_server_config(ServerConfig) ->
    Name = get_process_name(ServerConfig),
    lager:debug("Initialising notification server: ~p", [Name]),
    Id = kz_term:to_atom(Name, 'true'),
    ?SUPER_NAME_ARGS_TYPE(Id, supervisor_module(ServerConfig), [ServerConfig, Name], 'temporary').

%% Works for server config or subscription document

-spec get_process_name(kz_json:object()) -> atom().
get_process_name(ServerConfig) ->
    AppName = kz_json:get_first_defined([<<"app_name">>, <<"pvt_app_name">>], ServerConfig),
    NotificationType = kz_json:get_value(<<"notification_type">>, ServerConfig),
    get_process_name(ServerConfig, AppName, NotificationType).

-spec get_process_name(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> atom().
get_process_name(ServerConfig, AppName, <<"fcm">>) ->
    %% We will have a FCM server per platform (android/ios)
    %% Default to Android to avoid breaking old configs
    Platform = kz_json:get_value(<<"platform">>, ServerConfig, <<"android">>),
    kz_term:to_atom(kz_term:to_binary(io_lib:format("nv_~s_fcm_~s", [AppName, Platform])), 'true');
get_process_name(_ServerConfig, AppName, NotificationType) ->
    kz_term:to_atom(kz_term:to_binary(io_lib:format("nv_~s_~s", [AppName, NotificationType])), 'true').

-spec supervisor_module(kz_json:object() | kz_term:ne_binary()) -> module().
%% Works for server config or subscription document
supervisor_module(NotificationType) when is_binary(NotificationType) ->
    kz_term:to_atom(kz_term:to_binary(io_lib:format("nv_~s_server_sup", [NotificationType])), 'true');
supervisor_module(ServerConfig) ->
    supervisor_module(kz_json:get_value(<<"notification_type">>, ServerConfig)).
