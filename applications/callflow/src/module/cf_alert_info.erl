%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications
%%% @doc
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(cf_alert_info).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"alert_info">>, Data) of
        'undefined' ->
            lager:debug("empty alert info"),
            cf_exe:continue(Call);
        AlertInfo ->
            lager:debug("setting alert info to ~s", [AlertInfo]),
            Call1 = kapps_call:set_custom_sip_header(<<"Alert-Info">>, AlertInfo, Call),
            cf_exe:set_call(Call1),
            cf_exe:continue(Call1)
    end.
