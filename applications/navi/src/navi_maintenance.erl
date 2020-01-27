%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% @author Ben Partridge
%%% @end
%%%-----------------------------------------------------------------------------
-module(navi_maintenance).

-include("navi.hrl").

-export([add_apns_app/5, add_fcm_app/3]).
-export([start_server/1]).

-spec add_apns_app(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {{'error', any()}, kz_term:ne_binary()}.
add_apns_app(Name, Topic, Environment, CertificateFile, KeyFile) ->
    case {file:read_file(CertificateFile), file:read_file(KeyFile)} of
        {{'ok', CertificateBin}, {'ok', KeyBin}} ->
            NewApp = kz_json:from_list([{<<"app_name">>, Name}
                                       ,{<<"certificate">>, CertificateBin}
                                       ,{<<"default_topic">>, Topic}
                                       ,{<<"environment">>, Environment}
                                       ,{<<"key">>, KeyBin}
                                       ,{<<"notification_type">>, <<"apns">>}
                                       ]),
            validate_and_set_app(NewApp);
        {{'error', _} = Error, _} -> {Error, <<"Error reading certificate file">>};
        {_, {'error', _} = Error} -> {Error, <<"Error reading key file">>}
    end.

-spec add_fcm_app(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | {{'error', any()}, kz_term:ne_binary()}.
add_fcm_app(Name, Platform, ApiKey) ->
    NewApp = kz_json:from_list([{<<"app_name">>, Name}
                               ,{<<"api_key">>, ApiKey}
                               ,{<<"notification_type">>, <<"fcm">>}
                               ,{<<"platform">>, Platform}
                               ]),
    validate_and_set_app(NewApp).

-spec validate_and_set_app(kz_json:object()) -> 'ok' | {{'error', any()}, kz_term:ne_binary()}.
validate_and_set_app(NewApp) ->
    {'ok', Config} = kapps_config:get_category(?CONFIG_CAT),
    Apps = kz_json:get_value(<<"notification_servers">>, kz_json:get_value(<<"default">>, Config)),
    NewConfig = kz_json:set_value(<<"notification_servers">>, [NewApp|Apps], Config),
    case kz_json_schema:validate(<<"system_config.navi">>, NewConfig) of
        {'ok', _} ->
            lager:error("Did validate"),
            ConfigSetResult = kapps_config:set(?CONFIG_CAT, <<"notification_servers">>, [NewApp|Apps]),
            io:format("~p~n", [ConfigSetResult]),
            start_server(NewApp);
        Error ->
            lager:error("Did not validate"),
            {{'error', Error}, <<"Error validating app">>}
    end.

%%------------------------------------------------------------------------------
%% @doc Starts a new push server process for the given notification server
%% configuration.
%% @end
%%------------------------------------------------------------------------------
-spec start_server(kz_json:object()) -> 'ok' | {{'error', any()}, kz_term:ne_binary()}.
start_server(ServerConfig) ->
    case navi_module_sup:start_server(ServerConfig) of
        {'ok', Pid} -> start_server_success(Pid);
        {'ok', Pid, _} -> start_server_success(Pid);
        {'error', _}=E -> {E, <<"Unable to start push server process">>}
    end.

-spec start_server_success(pid()) -> 'ok'.
start_server_success(Pid) ->
    io:format("Push server started under PID ~s~n", [pid_to_list(Pid)]).
