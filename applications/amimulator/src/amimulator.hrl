-ifndef(AMIMULATOR_HRL).

%% Typical includes needed
-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_apps/include/kz_hooks.hrl").

-define(APP_NAME, <<"amimulator">>).
-define(APP_VERSION, <<"1.0.0">>).
-define(AMIMULATOR_CONFIG_CAT, <<"amimulator">>).

-define(DEFAULT_MODULES, ['bh_token_auth']).

-define(VERSION_SUPPORTED, [<<"v1">>]).

-define(AMIMULATOR_HRL, 'true').

-endif.
