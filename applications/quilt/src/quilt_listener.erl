%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_listener).
-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,handle_event/2
    ,terminate/2
    ,code_change/3
    ]).

-include("quilt.hrl").

-record(state, {}).

-define(BINDINGS, [
    {'self', []}
    ,{acdc_stats, [{restrict_to, [call_stat, status_stat]}]}
]).

-define(RESPONDERS, [{
    {?MODULE, 'handle_event'}, [
        {<<"acdc_call_stat">>, <<"*">>}
        ,{<<"acdc_status_stat">>, <<"*">>}
    ]
}]).

start_link() ->
    gen_listener:start_link(?MODULE, [
        {'bindings', ?BINDINGS},
        {'responders', ?RESPONDERS}
    ], []).

init([]) -> 
    {'ok', #state{}}.

handle_call(Request, _From, State) ->
    lager:debug("unhandled call: ~p", [Request]),
    {reply, {error, not_implemented}, State}.

handle_cast(Msg, State) ->
    lager:debug("unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, shutdown} ->
            lager:debug("~p shutting down...", [Pid]),
            exit(Pid, "shutting down...");
        _ ->
            lager:debug("unhandled info: ~p", [Info])
    end,
    {noreply, State}.

handle_event(JObj, State) ->
    % lager:debug("unhandled event: ~p, state: ~p", [JObj, State]),
    quilt_log:handle_event(JObj, State),
    {noreply, []}.

terminate(Reason, _State) ->
    lager:debug("quilt_listener listener on pid ~p terminating: ~p", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.