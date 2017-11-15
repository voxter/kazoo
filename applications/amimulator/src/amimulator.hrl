-ifndef(AMIMULATOR_HRL).

%% Typical includes needed
-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_apps/include/kz_hooks.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(APP_NAME, <<"amimulator">>).
-define(APP_VERSION, <<"1.0.0">>).
-define(AMIMULATOR_CONFIG_CAT, <<"amimulator">>).
-define(AMIMULATOR_CACHE, 'amimulator_cache').

-define(AMIMULATOR_HRL, 'true').

-endif.
