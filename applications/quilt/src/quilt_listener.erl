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

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, []).

start_link() ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                     ], []).

init([]) -> 
    lager:debug("QUILT: adding bindings to acdc queues"),
    Queues = acdc_queues_sup:queues_running(),
    QueueSups = [X || {X,_} <- Queues],
    lists:foreach(fun(QueueSup) -> init_queue_bindings(QueueSup) end, QueueSups),

    lager:debug("QUILT: adding bindings to acdc agents"),
    init_agent_bindings(),
    {'ok', #state{}}.

init_queue_bindings(QueueSup) ->
    lager:debug("QUILT: adding responder to acdc queue sup: ~p", [QueueSup]),
    Manager = acdc_queue_sup:manager(QueueSup),
    gen_listener:add_responder(
        Manager,
        {'quilt_log', 'handle_event'},
        [
            % {<<"member">>, <<"call">>},
            % {<<"member">>, <<"call_cancel">>},
            % {<<"queue">>, <<"agent_change">>},
            {<<"call_event">>, <<"*">>},
            {<<"member">>, <<"*">>},
            {<<"queue">>, <<"*">>}
        ]
    % ),

    % queue_worker_bindings(acdc_queue_workers_sup:workers(acdc_queue_sup:workers_sup(QueueSup)))] ++ Bindings
    %     end, [], QueueSups).

    % queue_worker_bindings(QueueWorkerSups) ->
    % lists:foldl(fun(QueueWorkerSup, Acc) ->
    %     [{acdc_queue_worker_sup:listener(QueueWorkerSup), {amimulator_acdc, handle_event},
    %         {<<"member">>, <<"connect_accepted">>}
    %     }] ++ Acc end, [], QueueWorkerSups
    ).

init_agent_bindings() ->
    gen_listener:add_responder(
        acdc_agent_manager,
        {'quilt_log', handle_event},
        [
            {<<"call_event">>, <<"*">>},
            {<<"agent">>, <<"*">>}
            % {<<"agent">>, <<"login">>},
            % {<<"agent">>, <<"logout">>},
            % {<<"agent">>, <<"queue_login">>},
            % {<<"agent">>, <<"queue_logout">>},
            % {<<"agent">>, <<"pause">>},
            % {<<"agent">>, <<"resume">>}
        ]
    ).

handle_call(_Request, _From, State) ->
    lager:debug("QUILT: unhandled call"),
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