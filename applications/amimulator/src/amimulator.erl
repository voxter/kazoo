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
    amimulator_supersup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Perform pre-shutdown cleanup
%%              This includes closing listening sockets for the translator
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
