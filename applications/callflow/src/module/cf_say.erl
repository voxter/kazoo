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
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    kapps_call_command:answer(Call),

    NoopId = kapps_call_command:noop(Call),
    kapps_call_command:say(
      kz_json:get_value(<<"text">>, Data)
                          ,kz_json:get_value(<<"type">>, Data, <<"name_spelled">>)
                          ,kz_json:get_value(<<"method">>, Data, <<"pronounced">>)
                          ,kz_json:get_value(<<"language">>, Data, kapps_call:language(Call))
                          ,Call
     ),

    case cf_util:wait_for_noop(Call, NoopId) of
        {'error', 'channel_hungup'} -> cf_exe:stop(Call);
        {'ok', Call1} ->
            %% Give control back to cf_exe process
            cf_exe:set_call(Call1),
            cf_exe:continue(Call1)
    end.
