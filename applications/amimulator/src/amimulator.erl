%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(amimulator).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start_link/0, before_stop/0, stop/0]).

%%------------------------------------------------------------------------------
%% @doc Starts the app for inclusion in a supervisor tree
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    amimulator_supersup:start_link().

%%------------------------------------------------------------------------------
%% @doc Perform pre-shutdown cleanup
%%              This includes closing listening sockets for the translator
%% @end
%%------------------------------------------------------------------------------
-spec before_stop() -> 'ok'.
before_stop() ->
    amimulator_supersup:before_stop(),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Stop the app
%% @end
%%------------------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    'ok'.
