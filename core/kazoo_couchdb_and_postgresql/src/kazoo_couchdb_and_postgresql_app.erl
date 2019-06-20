-module(kazoo_couchdb_and_postgresql_app).

-behaviour(application).

%%-include("kz_couchdb_and_postgresql.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    lager:debug("starting application 'kazoo_couchdb_and_postgresql'"),
    kazoo_couchdb_and_postgresql_sup:start_link().


%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    lager:debug("stopping application kazoo_couchdb_and_postgresql"),
    'ok'.
