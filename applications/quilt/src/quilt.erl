%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc Asterisk queue_log translator for Kazoo
%%%
%%% @author Lucas Bussey
%%% @end
%%%-----------------------------------------------------------------------------
-module(quilt).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start_link/0
        ,start/0
        ,stop/0
        ]).

%%------------------------------------------------------------------------------
%% @doc Starts the app for inclusion in a supervisor tree
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    quilt_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Starts the application
%% @end
%%------------------------------------------------------------------------------
-spec start() -> 'ok' | {'error', _}.
start() ->
    application:start(?MODULE).

%%------------------------------------------------------------------------------
%% @doc Stop the app
%% @end
%%------------------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    exit(whereis('quilt_listener'), 'shutdown'),
    'ok'.
