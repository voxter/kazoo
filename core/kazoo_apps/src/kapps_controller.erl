%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_controller).

%% API
-export([start_link/0
        ,ready/0
        ,start_app/1
        ,start_default_apps/0
        ,stop_app/1
        ,restart_app/1
        ,running_apps/0, running_apps/1
        ,app_running/1
        ,list_apps/0
        ,start_which_kapps/0
        ]).

-include("kazoo_apps.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts kazoo applications, initializing databases and create or update
%% views and all system schemas.
%%
%% The first step is to initializing the required databases. This done by
%% checking if the `accounts' database is exists, if not creating required
%% databases. Then core databases views will be updated by reading from
%% file system, and putting or updating all account related view definitions
%% in `system_data' database by reading them from file system.
%%
%% If `crossbar' is in the list of applications to start, `crossbar' will update
%% other system databases views and will put new and updated system schemas (if any)
%% from file system into `system_schema' database during its own start up.
%%
%% Multiple methods will be attempted to get a list of which application to start.
%% If any one of them results in a list of applications, those applications are
%% considered to start and no further method will be attempted.
%%
%% These methods will be attempted to get which application to start
%% exactly in this order:
%%
%% <ul>
%% <li>From environment variable `KAZOO_APPS', application list can be separated
%% by comma or space</li>
%% <li>Read `kapps' in `system_config/kapps_controller' for this specific Kazoo
%% node.</li>
%% <li>Using Erlang node name, if the it is named after one of the Kazoo
%% applications. Newer and safer way is to read Erlang application environment
%% variable `is_kazoo_app'. If this environment variable is not set it fall back
%% to check the application's `.app' and read applications key, if the name of
%% the node is member of this list. In this method only the application with
%% bathing Erlang node's name will be started</li>
%% <li>Read default `kapps' from `system_config/kapps_controller', this is the
%% last method and always starts default set of application.</li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _ = kz_util:spawn(fun initialize_kapps/0),
    'ignore'.

-spec ready() -> boolean().
ready() ->
    ready('false').

-spec ready(boolean()) -> boolean().
ready(Verbose) ->
    Configured = [kz_term:to_binary(App) || App <- start_which_kapps(Verbose)],
    Running = [kz_term:to_binary(App) || App <- running_apps(), is_atom(App)],

    0 =:= sets:size(
            sets:subtract(sets:from_list(Configured)
                         ,sets:from_list(Running)
                         )
           ).

-spec start_default_apps() -> [{atom(), 'ok' | {'error', any()}}].
start_default_apps() ->
    [{App, start_app(App)} || App <- ?DEFAULT_KAPPS].

-spec start_app(atom() | nonempty_string() | kz_term:ne_binary()) ->
                       {'ok', kz_term:atoms()} |
                       {'error', any()}.
start_app(App) when is_atom(App) ->
    case application:ensure_all_started(App) of
        {'ok', _}=OK ->
            kz_nodes_bindings:bind(App),
            OK;
        {'error', _E}=E ->
            lager:error("~s could not start: ~p", [App, _E]),
            E
    end;
start_app(App) ->
    start_app(kz_term:to_atom(App, 'true')).

-spec stop_app(atom() | nonempty_string() | kz_term:ne_binary()) -> 'ok' | {'error', any()}.
stop_app(App) when is_atom(App) ->
    case application:stop(App) of
        'ok' ->
            kz_nodes_bindings:bind(App),
            lager:info("stopped kazoo application ~s", [App]);
        {'error', {'not_started', App}} ->
            lager:error("~s is not currently running", [App]);
        {'error', _E}=Err ->
            lager:error("error stopping applicaiton ~s: ~p", [App, _E]),
            Err
    end;
stop_app(App) ->
    stop_app(kz_term:to_atom(App)).

-spec restart_app(atom() | nonempty_string() | kz_term:ne_binary()) -> 'ok' | {'error', any()}.
restart_app(App) when is_atom(App) ->
    lager:info("restarting kazoo application ~s", [App]),
    _ = stop_app(App),
    start_app(App);
restart_app(App) ->
    restart_app(kz_term:to_atom(App, 'true')).

-spec running_apps() -> kz_term:atoms() | string().
running_apps() ->
    running_apps('false').

-spec running_apps(boolean()) -> kz_term:atoms() | string().
running_apps(Verbose) ->
    case kz_term:is_true(Verbose) of
        'true' -> running_apps_verbose();
        'false' -> running_apps_list()
    end.

-spec running_apps_verbose() -> kz_term:atoms() | string().
running_apps_verbose() ->
    case get_running_apps() of
        [] -> "kapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses";
        Resp ->
            lists:sort(
              [kz_term:to_binary(io_lib:format("~s(~s): ~s~n", [App, Vsn, Desc]))
               || {App, Desc, Vsn} <- Resp
              ]
             )
    end.

-spec get_running_apps() -> [{atom(), string(), _}].
get_running_apps() ->
    get_running_apps('false').

-spec get_running_apps(boolean()) -> [{atom(), string(), _}].
get_running_apps(IncludeHidden) ->
    [AppData
     || {App, _Desc, _Vsn}=AppData <- application:which_applications(),
        IncludeHidden
            orelse is_kapp(App)
    ].

-spec running_apps_list() -> kz_term:atoms() | string().
running_apps_list() ->
    running_apps_list('false').

-spec running_apps_list(boolean()) -> kz_term:atoms() | string().
running_apps_list(IncludeHidden) ->
    case get_running_apps(IncludeHidden) of
        [] -> "kapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses";
        Resp -> lists:sort([App || {App, _Desc, _Vsn} <- Resp])
    end.

-spec app_running(atom() | kz_term:text()) -> boolean().
app_running(AppName) when is_atom(AppName) ->
    case [App
          || App <- running_apps_list('true'),
             AppName =:= App
         ]
    of
        [] -> false;
        _ -> true
    end;
app_running(AppName) ->
    try
        app_running(kz_term:to_atom(AppName))
    catch
        'error':'badarg' -> 'false'
    end.

-spec initialize_kapps() -> 'ok'.
initialize_kapps() ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    _New = kz_datamgr:init_dbs(),
    _ = kapps_maintenance:init_system(),
    kapps_config:migrate(),
    ToStart = lists:sort(fun sysconf_first/2, [kz_term:to_atom(KApp, 'true') || KApp <- start_which_kapps()]),
    Started = [KApp || KApp <- ToStart,
                       {'ok',_} <- [start_app(KApp)]
              ],
    lager:notice("auto-started kapps ~p", [lists:sort(Started)]).

-spec start_which_kapps() -> [kz_term:ne_binary() | atom() | nonempty_string()].
start_which_kapps() ->
    start_which_kapps('true').

start_which_kapps(Verbose) ->
    Routines = [fun maybe_start_from_env/1
               ,fun maybe_start_from_node_config/1
               ,fun maybe_start_from_node_name/1
               ,fun start_from_default_config/1
               ],
    lists:foldl(fun(F, 'false') -> F(Verbose);
                   (_, Apps) -> Apps
                end
               ,'false'
               ,Routines
               ).

-spec maybe_start_from_env(boolean()) -> 'false' | [nonempty_string()].
maybe_start_from_env(Verbose) ->
    case os:getenv("KAZOO_APPS", "noenv") of
        "noenv" -> 'false';
        KazooApps ->
            log_verbose(Verbose
                       ,"starting applications specified in environment variable KAZOO_APPS: ~s"
                       ,[KazooApps]
                       ),
            string:tokens(KazooApps, ", ")
    end.

log_verbose('false', _Fmt) -> 'ok';
log_verbose('true', Fmt) ->
    lager:info(Fmt).

log_verbose('false', _Fmt, _Args) -> 'ok';
log_verbose('true', Fmt, Args) ->
    lager:info(Fmt, Args).

-spec maybe_start_from_node_name(boolean()) -> 'false' | kz_term:atoms().
maybe_start_from_node_name(Verbose) ->
    KApp = kapp_from_node_name(),
    case is_kapp(KApp) of
        'false' -> 'false';
        _Else ->
            log_verbose(Verbose, "starting application based on node name: ~s", [KApp]),
            [KApp]
    end.

-spec maybe_start_from_node_config(boolean()) -> 'false' | [kz_term:ne_binary() | atom()].
maybe_start_from_node_config(Verbose) ->
    case kapps_config:get_node_value(?MODULE, <<"kapps">>) of
        'undefined' -> 'false';
        KazooApps ->
            log_verbose(Verbose
                       ,"starting applications configured specifically for this node: ~s"
                       ,[kz_binary:join(KazooApps, <<", ">>)]
                       ),
            KazooApps
    end.

-spec start_from_default_config(boolean()) -> 'false' | [kz_term:ne_binary() | atom()].
start_from_default_config(Verbose) ->
    log_verbose(Verbose, "starting applications from default configuration"),
    kapps_config:get(?MODULE, <<"kapps">>, ?DEFAULT_KAPPS).

-spec kapp_from_node_name() -> atom().
kapp_from_node_name() ->
    kz_term:to_atom(hd(binary:split(kz_term:to_binary(node()), <<$@>>)), 'true').

-spec sysconf_first(atom(), atom()) -> boolean().
sysconf_first('sysconf', _) -> 'true';
sysconf_first(_, 'sysconf') -> 'false';
sysconf_first(_, _) -> 'true'.

-spec list_apps() -> kz_term:atoms().
list_apps() ->
    [App || {App, _, _} <- get_running_apps()].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_kapp(atom()) -> boolean().
is_kapp(App) ->
    case application:get_env(App, 'is_kazoo_app') of
        {'ok', 'true'} -> 'true';
        _ -> has_me_as_dep(App)
    end.

%% This is the old way of detecting a "kazoo app" vs "core app"/"dep app"/"otp app"
%% This doesn't really work as core libs can have kazoo_apps as a dep (looking at you
%% kapps_util!).
-spec has_me_as_dep(atom()) -> boolean().
has_me_as_dep(App) ->
    case application:get_key(App, 'applications') of
        {'ok', Deps} -> lists:member(?APP, Deps);
        'undefined' ->
            %% Race condition sometimes prevents from reading application key
            'non_existing' =/= code:where_is_file(atom_to_list(App) ++ ".app")
    end.
