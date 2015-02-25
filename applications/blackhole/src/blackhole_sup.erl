%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Karl Anderson
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
-module(blackhole_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("blackhole.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE('blackhole_cache')
                   ,?WORKER('blackhole_listener')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _Dispatch = cowboy_router:compile([
                                      {'_', [{"/socket.io/1/[...]"
                                             ,'socketio_handler'
                                             ,[socketio_session:configure([{'heartbeat', 5000}
                                                                          ,{'heartbeat_timeout', 30000}
                                                                          ,{'session_timeout', 30000}
                                                                          ,{'callback', 'blackhole_socket_callback'}
                                                                          ,{'protocol', 'socketio_data_protocol'}
                                                                          ])]}
                                            ]
                                      }
                                     ]),
    _Port = whapps_config:get_integer(<<"blackhole">>, <<"port">>, 5555),
    %{'ok', _} = cowboy:start_http('socketio_http_listener', 100, [{'port', Port}],
    %                              [{'env', [{'dispatch', Dispatch}]}]),
    %Ret = supervisor:start_link({'local', ?MODULE}, ?MODULE, []),
    
    % Spawn separate server for operating AMI stream
    supervisor:start_link(?MODULE, ami).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
init(ami) ->
    {'ok', {
        {one_for_one, 1, 60}, [{
            blackhole_ami_serv, {
                blackhole_ami_serv, start_link, []
            },
            permanent,
            10000,
            worker,
            [blackhole_ami_serv]
        }]
    }};
    
init([]) ->
    lager:debug("Loading blackhole children"),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    {'ok', {SupFlags, ?CHILDREN}}.
