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
	lager:debug("QUILT: adding responders to acdc"),
	Queues = acdc_queues_sup:queues_running(),
	QueueSups = [X || {X,_} <- Queues],
	lists:foreach(fun(QueueSup) -> init_queue_bindings(QueueSup) end, QueueSups),
	{'ok', #state{}}.

init_queue_bindings(QueueSup) ->
	lager:debug("QUILT: adding responder to acdc queue sup"),
    Manager = acdc_queue_sup:manager(QueueSup),
    gen_listener:add_responder(
       Manager,
       {'quilt_log', 'handle_event'},
       [
           {<<"member">>, <<"call">>},
           {<<"member">>, <<"call_cancel">>}
       ]
    ).

% init_agent_bindings() ->
%     gen_listener:add_responder(
%        acdc_agent_manager,
%        {?MODULE, handle_event},
%        [
%            {<<"agent">>, <<"login">>},
%            {<<"agent">>, <<"logout">>},
%            {<<"agent">>, <<"queue_login">>},
%            {<<"agent">>, <<"queue_logout">>},
%            {<<"agent">>, <<"pause">>},
%            {<<"agent">>, <<"resume">>}
%        ]
%     ).

handle_call(_Request, _From, State) ->
    lager:debug("QUILT: unhandled call"),
    {reply, {error, not_implemented}, State}.

handle_cast(Msg, State) ->
    lager:debug("QUILT: unhandled cast: ~p", Msg),
    {noreply, State}.

handle_info(Info, State) ->
    lager:debug("QUILT: unhandled info: ~p", Info),
    {noreply, State}.

handle_event(JObj, State) ->
	lager:debug("QUILT: unhandled event: ~p, state: ~p", [JObj, State]),
    {reply, []}.

terminate(Reason, _State) ->
    lager:debug("QUILT: quilt_listener listener on pid ~p terminating: ~p", [self(), Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.