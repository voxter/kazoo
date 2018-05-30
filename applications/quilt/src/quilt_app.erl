%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc Asterisk queue_log translator for Kazoo
%%%
%%% @author Lucas Bussey
%%% @end
%%%-----------------------------------------------------------------------------
-module(quilt_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour
%% @end
%%------------------------------------------------------------------------------
-spec start(term(), term()) ->
                   {'ok', pid()} |
                   {'error', kz_types:startlink_err()}.
start(_Type, _Args) -> quilt:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour
%% @end
%%------------------------------------------------------------------------------
-spec stop(term()) -> 'ok'.
stop(_State) -> quilt:stop().
