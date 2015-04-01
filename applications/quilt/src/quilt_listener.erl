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
    {'self', []},
    {acdc_stats, []},
    % {acdc_queue, []}, %% Added dynamically
    {acdc_agent, []}
]).

-define(RESPONDERS, []).

start_link() ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                     ], []).

init([]) -> 
    lager:debug("QUILT: adding bindings to acdc"),
    quilt_store:start_link(),
    Queues = acdc_queues_sup:queues_running(),
    QueueSups = [X || {X,_} <- Queues],
    lists:foreach(fun(QueueSup) -> init_queue_responders(QueueSup) end, QueueSups),
    QueueSups = [X || {X,_} <- Queues],
    lists:foreach(fun(QueueSup) -> init_queue_bindings(QueueSup) end, QueueSups),
    {'ok', #state{}}.

init_queue_bindings(QueueSup) ->
    % lager:debug("QUILT: adding bindings to acdc queue sup: ~p", [QueueSup]),
    Manager = acdc_queue_sup:manager(QueueSup),
    {AccountId,_} = acdc_queue_manager:config(Manager),
    % Check to see if we are monitoring this account's queues already
    case quilt_store:get(AccountId) of
        undefined ->
            lager:debug("QUILT: adding bindings to acdc queues for account id: ~p", [AccountId]),
            quilt_store:put(AccountId, true),
            gen_listener:add_binding(
                Manager, {acdc_queue, [{restrict_to, [<<"*">>]}, {account_id, AccountId}]}
            );
        _ ->
            lager:debug("QUILT: already bound to events for account id: ~p", [AccountId])
    end.

init_queue_responders(QueueSup) ->
    lager:debug("QUILT: adding responders to acdc queue sup: ~p", [QueueSup]),
    Manager = acdc_queue_sup:manager(QueueSup),
    gen_listener:add_responder(
        Manager, {?MODULE, 'handle_event'}, [{<<"*">>, <<"*">>}]
    ).

handle_call(Request, _From, State) ->
    lager:debug("QUILT: unhandled call: ~p", [Request]),
    {reply, {error, not_implemented}, State}.

handle_cast(Msg, State) ->
    lager:debug("QUILT: unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, shutdown} ->
            lager:debug("QUILT: ~p shutting down...", [Pid]),
            exit(Pid, "QUILT: shutting down...");
        _ ->
            lager:debug("QUILT: unhandled info: ~p", [Info])
    end,
    {noreply, State}.

handle_event(JObj, State) ->
    % lager:debug("QUILT: unhandled event: ~p, state: ~p", [JObj, State]),
    quilt_log:handle_event(JObj, State),
    {reply, []}.

terminate(Reason, _State) ->
    lager:debug("QUILT: quilt_listener listener on pid ~p terminating: ~p", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.