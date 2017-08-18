%%%-------------------------------------------------------------------
%%% @copyright (C) 2017 Voxter Communications
%%% @doc
%%% Flush DTMF collection on a call
%%% "data":{
%%%   "collection_name":"your_name_here" // the collection to flush
%%% }
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_flush_dtmf).

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
    Collection = collection_name(Data),
    cf_exe:continue(whapps_call:set_dtmf_collection('undefined', Collection, Call)).

-spec collection_name(wh_json:object()) -> ne_binary().
collection_name(Data) ->
    wh_json:get_ne_binary_value(<<"collection_name">>, Data, <<"default">>).
