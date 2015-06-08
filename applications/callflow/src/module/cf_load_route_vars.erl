-module(cf_load_route_vars).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Call2 = load_route_vars(Data, Call),
    cf_exe:set_call(Call2),
    cf_exe:continue(Call2).

-spec load_route_vars(wh_json:object(), whapps_call:call()) -> whapps_call:call().
load_route_vars(Data, Call) ->
    load_route_vars_fold(wh_json:get_keys(Data), Data, Call).

-spec load_route_vars_fold(list(), wh_json:object(), whapps_call:call()) -> whapps_call:call().
load_route_vars_fold([], _, Call) ->
    Call;
load_route_vars_fold([Key|Keys], Data, Call) ->
    Value = wh_json:get_value([Key, <<"value">>], Data),
    lager:debug("setting route var ~p to ~p", [Key, Value]),
    Call2 = whapps_call:set_custom_channel_var(Key, Value, Call),
    load_route_vars_fold(Keys, Data, Call2).
