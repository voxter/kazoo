%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(amimulator_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, prep_stop/1, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour
%% @end
%%------------------------------------------------------------------------------
-spec start(term(), term()) ->
                   {'ok', pid()} |
                   {'error', kz_types:startlink_err()}.
start(_Type, _Args) ->
    amimulator:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application prep_stop behaviour
%% @end
%%------------------------------------------------------------------------------
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    amimulator:before_stop(),
    State.

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour
%% @end
%%------------------------------------------------------------------------------
-spec stop(term()) -> 'ok'.
stop(_State) ->
    amimulator:stop().
