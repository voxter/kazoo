%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_log).

-export([handle_event/2]).

-include("quilt.hrl").

handle_event(JObj, Props) ->
	lager:debug("QUILT: unhandled event: ~p", JObj),
	lager:debug("QUILT: unhandled event props: ~p", Props).