%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Voxter Communication Inc.
%%% @doc
%%%
%%% KNM module for iTel
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(knm_itel).

-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("../knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Is this carrier handling numbers local to the system?
%% Note: a non-local (foreign) carrier module makes HTTP requests.
%% @end
%%--------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), knm_carriers:options()) ->
                          {'error', 'not_available'}.
find_numbers(_Prefix, _Quantity, _Options) ->
    {'error', 'not_available'}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    case knm_phone_number:dry_run(PhoneNumber) of
        'true' -> Number;
        'false' -> Number
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) -> knm_number:knm_number().
disconnect_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.

-spec is_number_billable(knm_phone_number:knm_phone_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.
