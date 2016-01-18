%%%-------------------------------------------------------------------
%%% @copyright (C) 2015 Voxter Communications
%%% @doc
%%% "data":{
%%%   "text":"This is what should be said"
%%%   // optional
%%%   ,"language":"en"
%%%   ,"type"
%%%   ,"method"
%%% }
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(cf_say).

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
    whapps_call_command:answer(Call),

    NoopId = whapps_call_command:noop(Call),
    whapps_call_command:say(
       wh_json:get_value(<<"text">>, Data)
       ,wh_json:get_value(<<"type">>, Data, <<"name_spelled">>)
       ,wh_json:get_value(<<"method">>, Data, <<"pronounced">>)
       ,wh_json:get_value(<<"language">>, Data, whapps_call:language(Call))
       ,Call
      ),

    case cf_util:wait_for_noop(Call, NoopId) of
        {'error', 'channel_hungup'} -> cf_exe:stop(Call);
        {'ok', Call1} ->
            %% Give control back to cf_exe process
            cf_exe:set_call(Call1),
            cf_exe:continue(Call1)
    end.
