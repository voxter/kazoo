%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Daniel Finke
%%%-------------------------------------------------------------------
-module(amimulator).

-include_lib("kazoo/include/kz_types.hrl").

-export([start_link/0, before_stop/0, stop/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    amimulator_supersup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Perform pre-shutdown cleanup
%%		This includes closing listening sockets for the translator
%% @end
%%--------------------------------------------------------------------
-spec before_stop() -> 'ok'.
before_stop() ->
	amimulator_supersup:before_stop(),
	'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    kazoo_apps_deps:ensure(?MODULE), % if started by the kazoo_controller, this will exist
    _ = [kz_util:ensure_started(App) || App <- ['crypto'
                                               ,'inets'
                                               ,'lager'
                                               ,'kazoo_amqp'
                                               ,'kazoo_couch'
                                               ,'kazoo_bindings'
                                               ,'ranch'
                                               ,'cowboy'
                                               ,'public_key'
                                               ,'ssl'
                                               ,'socketio'
                                               ]],
    'ok'.
