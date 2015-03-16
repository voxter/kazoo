-ifndef(QUILT_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-define(APP_NAME, <<"quilt">>).
-define(APP_VERSION, <<"1.0.0">>).
-define(QUILT_CONFIG_CAT, <<"quilt">>).

-define(DEFAULT_MODULES, ['bh_token_auth']).

-define(VERSION_SUPPORTED, [<<"v1">>]).

-define(QUILT_HRL, 'true').

-endif.
