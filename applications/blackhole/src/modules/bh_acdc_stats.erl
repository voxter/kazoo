%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz Inc
%%% @doc
%%%
%%% ACDc stats bindings
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%% Lucas Bussey
%%%-------------------------------------------------------------------
-module(bh_acdc_stats).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-define(LISTEN_TO, [<<"waiting">>, <<"exited-position">>, <<"handled">>, <<"processed">>, <<"abandoned">>, <<"missed">>]).


-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.acdc_stats">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.acdc_stats">>, ?MODULE, 'bindings').


-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"call_stat">>, <<"*">>]
                   }) ->
    Context;
validate(Context, #{keys := [<<"call_stat">>, Event]
                   }) ->
    case lists:member(Event, ?LISTEN_TO) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Event/binary, " not supported">>)
    end;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for acdc subscription : ", (kz_util:join_binary(Keys))/binary>>).


-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"call_stat">>, <<"*">>]
                    }=Map) ->
    Requested = <<"acdc_stats.call_stat.*">>,
    Subscribed = [<<"acdc_stats.call.", Event/binary, ".", AccountId/binary, ".*">>
                      || Event <- ?LISTEN_TO],
    Listeners = [{'amqp', 'acdc_stats', [{'restrict_to', ['call_stat']}]}], %%, Event} || Event <- ?LISTEN_TO],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"call_stat">>, Event]
                    }=Map) ->
    Requested = <<"acdc_stats.call_stat.", Event/binary>>,
    Subscribed = [<<"acdc_stats.call.", Event/binary, ".", AccountId/binary, ".*">>],
    Listeners = [{'amqp', 'acdc_stats', [{'restrict_to', ['call_stat']}]}], %%, Event}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.
