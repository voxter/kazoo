%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Voxter Communication Inc.
%%% @doc
%%%
%%% KNM module for Vocus Communications
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(knm_vocus).

-export([find_numbers/3
         ,acquire_number/1
         ,disconnect_number/1
         ,should_lookup_cnam/0
         ,is_number_billable/1
        ]).

-include("../knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), kz_proplist()) ->
                          {'error', 'non_available'}.
find_numbers(_Prefix, _Quantity, _Options) ->
    {'error', 'non_available'}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().
acquire_number(#number{dry_run='true'}=Number) -> Number;
acquire_number(#number{}=Number) -> Number.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(#number{}=Number) ->
    Number#number{state = ?NUMBER_STATE_RELEASED
                  ,hard_delete='true'
                 }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.

-spec is_number_billable(wnm_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

