%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Stat util functions
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_stats_util).

-export([wait_time/2
         ,pause_time/2
         ,caller_id_name/2
         ,caller_id_number/2

         ,get_query_limit/1
         ,db_name/1
         ,prev_modb/1

         ,cleanup_old_stats/0
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-spec wait_time(ne_binary(), wh_json:object()) -> api_integer().
wait_time(<<"paused">>, _) -> 'undefined';
wait_time(_, JObj) -> wh_json:get_integer_value(<<"Wait-Time">>, JObj).

-spec pause_time(ne_binary(), wh_json:object()) -> api_integer().
pause_time(<<"paused">>, JObj) ->
    case wh_json:get_integer_value(<<"Pause-Time">>, JObj) of
        'undefined' -> wh_json:get_integer_value(<<"Wait-Time">>, JObj);
        PT -> PT
    end;
pause_time(_, _JObj) -> 'undefined'.

caller_id_name(_, JObj) ->
    wh_json:get_value(<<"Caller-ID-Name">>, JObj).
caller_id_number(_, JObj) ->
    wh_json:get_value(<<"Caller-ID-Number">>, JObj).

-spec get_query_limit(wh_json:object()) -> pos_integer().
get_query_limit(JObj) ->
    Max = ?MAX_RESULT_SET,
    case wh_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> Max;
        N when N > Max -> Max;
        N when N < 1 -> 1;
        N -> N
    end.

-spec db_name(ne_binary()) -> ne_binary().
db_name(Account) ->
    wh_util:format_account_mod_id(Account).

-spec prev_modb(ne_binary()) -> ne_binary().
prev_modb(Account) ->
	{{Year, Month, _}, _} = calendar:now_to_universal_time(os:timestamp()),
	prev_modb(Account, Year, Month-1).

-spec prev_modb(ne_binary(), calendar:year(), integer()) -> ne_binary().
prev_modb(Account, Year, 0) ->
	prev_modb(Account, Year-1, 12);
prev_modb(Account, Year, Month) ->
	wh_util:format_account_id(Account, Year, Month).

-spec cleanup_old_stats() -> 'ok'.
-spec cleanup_old_stats(pos_integer()) -> 'ok'.
cleanup_old_stats() ->
    cleanup_old_stats(1200).
cleanup_old_stats(Window) ->
    acdc_stats:manual_cleanup(Window).
