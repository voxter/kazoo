%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @author Max Lay (with code from Ben Partridge)
%%% @end
%%%-----------------------------------------------------------------------------
-module(nv_apns_server_sup).
-behaviour(supervisor).

-include("navi.hrl").

%% API
-export([start_link/2
        ,push/5
        ]).

%% Callbacks
-export([init/1]).

-define(APNS, <<"apns">>).

%%=========================================================
%%                 API
%%=========================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_json:object(), atom()) -> kz_types:startlink_ret().
start_link(ServerConfig, MyName) ->
    supervisor:start_link({local, MyName}, ?MODULE, [ServerConfig, MyName]).

-spec push(supervisor:sup_ref(), atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
push(_Super, ServerName, RegistrationId, Msg, ExtraParameters) ->
    PoolName = pool_name(ServerName),
    lager:debug("pushing to pool ~s", [PoolName]),
    poolboy:transaction(PoolName, fun(Worker) ->
                                          gen_server:cast(Worker, {'push', {RegistrationId, Msg, ExtraParameters}})
                                  end).

%%=========================================================
%%                 Callbacks
%%=========================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([ServerConfig, MyName]) ->
    lager:debug("starting ~s nv_apns_server_sup", [MyName]),
    DefaultTopic = kz_json:get_value(<<"default_topic">>, ServerConfig),
    IsPushkit = kz_json:get_value(<<"pushkit">>, ServerConfig, 'false'),
    PoolName = pool_name(MyName),

    %% Poolboy enforces the args are a proplist. Bleh.
    WorkerArgs = [{'pool_name', PoolName}
                 ,{'connection', build_connection(ServerConfig)}
                 ,{'default_topic', DefaultTopic}
                 ,{'pushkit', IsPushkit}
                 ],

    PoolArgs = [{'name', {'local', PoolName}}
               ,{'worker_module', 'nv_apns_pool_worker'}
               ,{'size', kapps_config:get_pos_integer(?CONFIG_CAT, [?APNS, <<"pool_size">>], 20)}
               ,{'max_overflow', kapps_config:get_pos_integer(?CONFIG_CAT, [?APNS, <<"max_overflow_pool_size">>], 40)}
               ],
    Workers = [poolboy:child_spec(pool_name(MyName), PoolArgs, WorkerArgs)],
    {'ok', {{'one_for_one', 10, 60}, Workers}}.

%%=========================================================
%%                 Private
%%=========================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pool_name(atom()) -> atom().
pool_name(ServerName) ->
    kz_term:to_atom(<<(kz_term:to_binary(ServerName))/binary, "_pool">>, 'true').

-spec build_connection(kz_json:object()) -> apns:connection().
build_connection(ServerConfig) ->
    PemDecodedCert = public_key:pem_decode(kz_json:get_binary_value(<<"certificate">>, ServerConfig)),
    [{'Certificate', Certificate, _}|_] = PemDecodedCert,
    KeyPem = kz_json:get_value(<<"key">>, ServerConfig),
    PemDecodedKey = public_key:pem_decode(KeyPem),
    [{'RSAPrivateKey', Key, _}|_] = PemDecodedKey,

    #{'name'       => 'undefined'
     ,'certdata'   => Certificate
     ,'keydata'    => {'RSAPrivateKey', Key}
     ,'type'       => 'certdata'
     ,'timeout'    => get_timeout()
     ,'apple_host' => get_host(ServerConfig)
     ,'apple_port' => get_port(ServerConfig)
     }.

-spec get_timeout() -> pos_integer().
get_timeout() ->
    %% Timeout immediately seeing we don't ever get a confirmation from Apple
    kapps_config:get_pos_integer(?CONFIG_CAT, [?APNS, <<"timeout_ms">>], 1).

-spec get_host(kz_json:object() | kz_term:ne_binary()) -> string().
get_host(ServerConfig) when is_tuple(ServerConfig) ->
    get_host(kz_json:get_binary_value(<<"environment">>, ServerConfig));
get_host(<<"dev">>) ->
    %% Even though we want a string, the default should still be a binary. It will be converted to a string for us
    kapps_config:get_string(?CONFIG_CAT, [?APNS, <<"dev_host">>], <<"api.development.push.apple.com">>);
get_host(<<"prod">>) ->
    %% Even though we want a string, the default should still be a binary. It will be converted to a string for us
    kapps_config:get_string(?CONFIG_CAT, [?APNS, <<"prod_host">>], <<"api.push.apple.com">>);
get_host(_Env) ->
    get_host(<<"prod">>).

-spec get_port(kz_json:object() | kz_term:ne_binary()) -> pos_integer().
get_port(ServerConfig) when is_tuple(ServerConfig) ->
    get_port(kz_json:get_binary_value(<<"environment">>, ServerConfig));
get_port(<<"dev">>) ->
    kapps_config:get_pos_integer(?CONFIG_CAT, [?APNS, <<"dev_port">>], 443);
get_port(<<"prod">>) ->
    kapps_config:get_pos_integer(?CONFIG_CAT, [?APNS, <<"prod_port">>], 443);
get_port(_Env) ->
    get_port(<<"prod">>).
