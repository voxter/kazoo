-module(quilt_log).
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

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%-record(state, {}).

start_link() ->
	gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ], []).

init([]) -> 
	Queues = acdc_queues_sup:queues_running(),
	QueueSups = [X || {X,_} <- Queues],
	lists:foreach(fun(QueueSup) -> init_queue_bindings(QueueSup) end, QueueSups).

init_queue_bindings(QueueSup) ->
	lager:debug("QUILT: adding responder to acdc queue sup"),
    Manager = acdc_queue_sup:manager(QueueSup),
    gen_listener:add_responder(
       Manager,
       {?MODULE, handle_event},
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
    {ok, State}.

handle_info(Info, State) ->
    lager:debug("QUILT: unhandled info: ~p", Info),
    {ok, State}.

handle_event(JObj, State) ->
	lager:debug("QUILT: unhandled event: ~p, state: ~p", [JObj, State]),
    {reply, State}.

terminate(Reason, State) ->
    lager:debug("QUILT: quilt_log listener on pid ~p terminating: ~p", [self(), Reason]),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.