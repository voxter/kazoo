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
                ,endpoint_numbers = [] :: kz_proplist()
                ,endpoint_patterns = [] :: kz_proplist()
               }).
-type state() :: #state{}.

-spec start_link(kz_json:object(), kz_proplist()) -> startlink_ret().
start_link(JObj, Props) ->
    gen_server:start_link(?MODULE, [JObj, Props], []).

-spec init(list()) -> {'ok', state()}.
init([JObj, Props]) ->
    process_flag('trap_exit', 'true'),
    EndpointId = kz_json:get_value(<<"Endpoint-ID">>, JObj),
    CallId = kz_json:get_value([<<"Call">>, <<"Call-ID">>], JObj),
    lager:debug("started new konami call for endpoint ~s on call ~s", [EndpointId, CallId]),
    init_state(JObj, Props).

-spec call_id(pid()) -> term().
call_id(Srv) ->
    gen_server:call(Srv, 'get_call_id').

-spec update(pid(), kz_json:object(), kz_proplist()) -> 'ok'.
update(Srv, JObj, Props) ->
    gen_server:cast(Srv, {'update', JObj, Props}).

-spec numbers(pid(), ne_binary()) -> term().
numbers(Srv, EndpointId) ->
    gen_server:call(Srv, {'get_numbers', EndpointId}).

-spec patterns(pid(), ne_binary()) -> term().
patterns(Srv, EndpointId) ->
    gen_server:call(Srv, {'get_patterns', EndpointId}).

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call('get_call_id', _, #state{call_id=CallId}=State) ->
    {'reply', CallId, State};
handle_call({'get_numbers', EndpointId}, _, #state{endpoint_numbers=Ns}=State) ->
    {'reply', props:get_value(EndpointId, Ns), State};
handle_call({'get_patterns', EndpointId}, _, #state{endpoint_patterns=Ps}=State) ->
    {'reply', props:get_value(EndpointId, Ps), State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).  
handle_cast({'update', JObj, _Props}, #state{code_fsm_pid=FSM
                                             ,endpoint_numbers=Ns
                                             ,endpoint_patterns=Ps
                                            }=State) ->
    EndpointId = kz_json:get_value(<<"Endpoint-ID">>, JObj),
    CallId = kz_json:get_value([<<"Call">>, <<"Call-ID">>], JObj),
    lager:debug("adding endpoint ~s to konami call ~s", [EndpointId, CallId]),
    konami_code_fsm:add_endpoint(FSM, EndpointId),
    {'noreply', State#state{endpoint_numbers = [{EndpointId, kz_json:get_value(<<"Numbers">>, JObj)} | Ns]
                            ,endpoint_patterns = [{EndpointId, kz_json:get_value(<<"Patterns">>, JObj)} | Ps]
                           }};
handle_cast(_Request, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'EXIT', FSM, Reason}, #state{code_fsm_pid=FSM}=State) ->
    lager:debug("going to die alongside my baby"),
    {'stop', Reason, State};
handle_info(_Info, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate('normal', _) ->
    'ok';
terminate(Reason, _State) ->
    lager:debug("terminating (~p)", [Reason]),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

init_state(JObj, Props) ->
    AuthorizingId = kz_json:get_value([<<"Call">>, <<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj),
    EndpointId = kz_json:get_value(<<"Endpoint-ID">>, JObj),
    {'ok', #state{call_id = kz_json:get_value([<<"Call">>, <<"Call-ID">>], JObj)
                  ,code_fsm_pid = start_fsm(JObj, Props)
                  ,endpoint_numbers = [{AuthorizingId, kz_json:get_value(<<"Numbers">>, JObj)}
                                       ,{EndpointId, kz_json:get_value(<<"Numbers">>, JObj)}
                                      ]
                  ,endpoint_patterns = [{AuthorizingId, kz_json:get_value(<<"Patterns">>, JObj)}
                                        ,{EndpointId, kz_json:get_value(<<"Patterns">>, JObj)}
                                       ]
                 }}.

start_fsm(JObj, Props) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    proc_lib:spawn_link('konami_code_fsm', 'start_fsm', [kapps_call:kvs_store('consumer_pid', props:get_value('server', Props), Call)
                                                         ,JObj
                                                         ,self()
                                                        ]).
