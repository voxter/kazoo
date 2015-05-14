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
    ,{acdc_agent, []}
    ,{acdc_stats, []}
    %% Added dynamically
    %,{acdc_queue, []}
]).

-define(RESPONDERS, []).

start_link() ->
    gen_listener:start_link(?MODULE, [
        {'bindings', ?BINDINGS},
        {'responders', ?RESPONDERS}
    ], []).

init([]) -> 
    lager:debug("adding bindings for acdc queues"),
    quilt_store:start_link(),
    Queues = acdc_queues_sup:queues_running(),
    QueueSups = [X || {X,_} <- Queues],
    lists:foreach(fun(QueueSup) -> init_queue(QueueSup) end, QueueSups),
    {'ok', #state{}}.

init_queue(QueueSup) ->
    lager:debug("adding bindings/responders for queue sup: ~p", [QueueSup]),
    Manager = acdc_queue_sup:manager(QueueSup),
    {AccountId,_} = acdc_queue_manager:config(Manager),
    % Check to see if we have configuration for this account already
    case quilt_store:get(AccountId) of
        undefined ->
            lager:debug("adding queue bindings for account id: ~p", [AccountId]),
            gen_listener:add_binding(self(), {acdc_queue, [{account_id, AccountId}]}),
            lager:debug("adding queue responders for account id: ~p", [AccountId]),
            gen_listener:add_responder(self(), {?MODULE, 'handle_event'}, [
                % {<<"*">>, <<"*">>}
                {<<"acdc_call_stat">>, <<"waiting">>},
                {<<"acdc_call_stat">>, <<"abandoned">>},
                {<<"acdc_call_stat">>, <<"missed">>},
                {<<"acdc_call_stat">>, <<"handled">>},
                {<<"acdc_call_stat">>, <<"processed">>},
                {<<"agent">>, <<"pause">>},
                {<<"agent">>, <<"resume">>},
                {<<"acdc_status_stat">>, <<"logged_in">>},
                {<<"acdc_status_stat">>, <<"logged_out">>}
            ]),
            quilt_store:put(AccountId, true);
        _ ->
            lager:debug("already have configuration for account id: ~p", [AccountId])
    end.

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