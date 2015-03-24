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
    % gen_listener:add_responder(
    %     acdc_agent_manager, {'quilt_log', handle_event}, [{<<"*">>, <<"*">>}]
    % ),
    Queues = acdc_queues_sup:queues_running(),
    QueueSups = [X || {X,_} <- Queues],
    lists:foreach(fun(QueueSup) -> init_queue_responders(QueueSup) end, QueueSups),
    QueueSups = [X || {X,_} <- Queues],
    lists:foreach(fun(QueueSup) -> init_queue_bindings(QueueSup) end, QueueSups),
    {'ok', #state{}}.

init_queue_bindings(QueueSup) ->
    lager:debug("QUILT: adding bindings to acdc queue sup: ~p", [QueueSup]),
    Manager = acdc_queue_sup:manager(QueueSup),
    lager:debug("QUILT: acdc queue manager: ~p", [Manager]),
    {AccountId, _} = acdc_queue_manager:config(Manager),
    lager:debug("QUILT: acdc queue config: ~p", [AccountId]),
    gen_listener:add_binding(
        Manager, {acdc_queue, [{restrict_to, [<<"*">>]}, {account_id, AccountId}]}
    ).

init_queue_responders(QueueSup) ->
    lager:debug("QUILT: adding responders to acdc queue sup: ~p", [QueueSup]),
    Manager = acdc_queue_sup:manager(QueueSup),
    gen_listener:add_responder(
        Manager, {'quilt_log', 'handle_event'}, [{<<"*">>, <<"*">>}]
    ).

%     % queue_worker_bindings(acdc_queue_workers_sup:workers(acdc_queue_sup:workers_sup(QueueSup)))] ++ Bindings
%     %     end, [], QueueSups).

%     % queue_worker_bindings(QueueWorkerSups) ->
%     % lists:foldl(fun(QueueWorkerSup, Acc) ->
%     %     [{acdc_queue_worker_sup:listener(QueueWorkerSup), {amimulator_acdc, handle_event},
%     %         {<<"member">>, <<"connect_accepted">>}
%     %     }] ++ Acc end, [], QueueWorkerSups
%     %).

handle_call(Request, _From, State) ->
    lager:debug("QUILT: unhandled call: ~p", [Request]),
    {reply, {error, not_implemented}, State}.

handle_cast(Msg, State) ->
    lager:debug("QUILT: unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:debug("QUILT: unhandled info: ~p", [Info]),
    {noreply, State}.

handle_event(JObj, State) ->
    lager:debug("QUILT: unhandled event: ~p, state: ~p", [JObj, State]),
    {reply, []}.

terminate(Reason, _State) ->
    lager:debug("QUILT: quilt_listener listener on pid ~p terminating: ~p", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.