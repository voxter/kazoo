%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-ifndef(QUILT_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
                                                %-include_lib("kazoo_apps/include/kz_hooks.hrl").

-define(APP_NAME, <<"quilt">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(VERSION_SUPPORTED, [<<"v1">>]).

-define(QUILT_HRL, 'true').

-record(state, {account_id :: ne_binary()
			   ,agent_id :: ne_binary()
			   ,queues=[] :: ne_binaries()
			   ,member_call_id='undefined'
			   }).

-endif.
