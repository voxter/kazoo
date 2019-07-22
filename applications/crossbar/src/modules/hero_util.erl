%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communications
%%% @doc Hero Utilities
%%% @end
%%% @author Dustin Brett
%%%-----------------------------------------------------------------------------
-module(hero_util).

-include("crossbar.hrl").

-export([get_apps/1
        ,check_hero_apps/2
        ]).

-define(APPS, <<"apps">>).
-define(MOD_CONFIG_CAT, <<"hero">>).
-define(DEFAULT_APPS, kapps_config:get(?MOD_CONFIG_CAT, ?APPS, [])).

-spec get_apps(cb_context:context()) -> kz_json:object().
get_apps(Context) ->
    AccountId = cb_context:account_id(Context),
    kapps_account_config:get(AccountId, ?MOD_CONFIG_CAT, ?APPS, ?DEFAULT_APPS).

-spec check_hero_apps(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_hero_apps(_UserId, Context) ->
    case cb_context:req_value(Context, <<"hero_apps">>) of
        'undefined' -> Context;
        HeroApps -> check_available_apps(HeroApps, Context)
    end.

-spec check_available_apps(kz_term:ne_binaries(), cb_context:context()) -> cb_context:context().
check_available_apps(HeroApps, Context) ->
    AvailableApps = get_available_apps(Context),
    case lists:filter(fun(App) -> not lists:member(App, AvailableApps) end, HeroApps) of
        [] -> Context;
        [App|_] ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"App is unavailable or doesn't exist">>}
                    ,{<<"cause">>, App}
                    ]),
            cb_context:add_validation_error(<<"hero_apps">>, <<"invalid">>, Msg, Context)
    end.

-spec get_available_apps(cb_context:context()) -> kz_term:ne_binaries().
get_available_apps(Context) ->
    lists:filtermap(fun(App) ->
                            case kz_json:get_value(<<"available">>, App) of
                                'true' -> {'true', kz_json:get_value(<<"id">>, App)};
                                _ -> 'false'
                            end
                    end
                   ,get_apps(Context)
                   ).
