%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(konami_call).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([call_id/1]).
-export([update/3]).
-export([numbers/2, patterns/2]).

-include("konami.hrl").

-record(state, {call_id :: api_binary()
                ,code_fsm_pid :: api_pid()
                ,endpoint_numbers = [] :: wh_proplist()
                ,endpoint_patterns = [] :: wh_proplist()
               }).

start_link(JObj, Props) ->
    gen_server:start_link(?MODULE, [JObj, Props], []).

init([JObj, Props]) ->
    process_flag('trap_exit', 'true'),
    EndpointId = wh_json:get_value(<<"Endpoint-ID">>, JObj),
    CallId = wh_json:get_value([<<"Call">>, <<"Call-ID">>], JObj),
    lager:debug("started new konami call for endpoint ~s on call ~s", [EndpointId, CallId]),
    init_state(JObj, Props).

call_id(Srv) ->
    gen_server:call(Srv, 'get_call_id').

update(Srv, JObj, Props) ->
    gen_server:cast(Srv, {'update', JObj, Props}).

numbers(Srv, EndpointId) ->
    gen_server:call(Srv, {'get_numbers', EndpointId}).

patterns(Srv, EndpointId) ->
    gen_server:call(Srv, {'get_patterns', EndpointId}).
    
handle_call('get_call_id', _, #state{call_id=CallId}=State) ->
    {'reply', CallId, State};
handle_call({'get_numbers', EndpointId}, _, #state{endpoint_numbers=Ns}=State) ->
    {'reply', props:get_value(EndpointId, Ns), State};
handle_call({'get_patterns', EndpointId}, _, #state{endpoint_patterns=Ps}=State) ->
    {'reply', props:get_value(EndpointId, Ps), State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.
    
handle_cast({'update', JObj, _Props}, #state{endpoint_numbers=Ns
                                            ,endpoint_patterns=Ps
                                           }=State) ->
    EndpointId = wh_json:get_value(<<"Endpoint-ID">>, JObj),
    CallId = wh_json:get_value([<<"Call">>, <<"Call-ID">>], JObj),
    lager:debug("adding endpoint ~s to konami call ~s", [EndpointId, CallId]),
    {'noreply', State#state{endpoint_numbers = [{EndpointId, wh_json:get_value(<<"Numbers">>, JObj)} | Ns]
                            ,endpoint_patterns = [{EndpointId, wh_json:get_value(<<"Patterns">>, JObj)} | Ps]
                           }};
handle_cast(_Request, State) ->
    {'noreply', State}.

handle_info({'EXIT', FSM, Reason}, #state{code_fsm_pid=FSM}=State) ->
    lager:debug("going to die alongside my baby"),
    {'stop', Reason, State};
handle_info(_Info, State) ->
    {'noreply', State}.
    
terminate('normal', _) ->
    'ok';
terminate(Reason, _State) ->
    lager:debug("terminating (~p)", [Reason]),
    'ok'.
    
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

init_state(JObj, Props) ->
    EndpointId = wh_json:get_value(<<"Endpoint-ID">>, JObj),
    {'ok', #state{call_id = wh_json:get_value([<<"Call">>, <<"Call-ID">>], JObj)
                  ,code_fsm_pid = start_fsm(JObj, Props)
                  ,endpoint_numbers = [{EndpointId, wh_json:get_value(<<"Numbers">>, JObj)}]
                  ,endpoint_patterns = [{EndpointId, wh_json:get_value(<<"Patterns">>, JObj)}]
                 }}.

start_fsm(JObj, Props) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    proc_lib:spawn_link('konami_code_fsm', 'start_fsm', [whapps_call:kvs_store('consumer_pid', props:get_value('server', Props), Call)
                                                         ,JObj
                                                         ,self()
                                                        ]).
