%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc Sets up a proxy for the mattermost push notification service.
%%%
%%% @author Ben Partridge
%%% @end
%%%-----------------------------------------------------------------------------
-module(mate_proxy).

-export([start_link/0
        ]).

-define(MATE_PORT, 7886).
-define(MATE_PATH, "/api/v1/send_push").
-define(NUM_RESPONDERS, 100).
-include("mate.hrl").

%%------------------------------------------------------------------------------
%% @doc Starts and links the process
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    Dispatch = cowboy_router:compile([{'_', [{?MATE_PATH, 'mate_handler', []}]}]),

    {'ok', _} = cowboy:start_clear('http'
                                  ,[{'port', kapps_config:get_integer(?CONFIG_CAT, <<"port">>, ?MATE_PORT)}
                                   ,{'num_acceptors', ?NUM_RESPONDERS}
                                   ]
                                  ,#{'env' => #{'dispatch' => Dispatch}}
                                  ),
    {'ok', self()}.
