%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% @author Ben Partridge
%%% @end
%%%-----------------------------------------------------------------------------
-module(mate_app).

-behaviour(application).

-include("mate.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    _ = kz_datamgr:revise_doc_from_file(?KZ_ACCOUNTS_DB, 'mate', <<"views/mate.json">>),
    mate_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    cowboy:stop_listener('mate_proxy'),
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kapi_self:declare_exchanges(),
    kapi_navi:declare_exchanges().
