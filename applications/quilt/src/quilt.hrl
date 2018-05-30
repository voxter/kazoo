%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc Asterisk queue_log translator for Kazoo
%%%
%%% @author Lucas Bussey
%%% @end
%%%-----------------------------------------------------------------------------
-ifndef(QUILT_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"quilt">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(VERSION_SUPPORTED, [<<"v1">>]).

-define(QUILT_HRL, 'true').

-record(state, {account_id :: kz_term:ne_binary()
               ,agent_id :: kz_term:ne_binary()
               ,queues=[] :: kz_term:ne_binaries()
               ,member_call_id='undefined'
               }).

-endif.
