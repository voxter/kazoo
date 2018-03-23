%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%% Manages queue processes:
%%%   starting when a queue is created
%%%   stopping when a queue is deleted
%%%   collecting stats from queues
%%%   and more!!!
%%% @end
%%% @contributors
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(acdc_queue_manager).
-behaviour(gen_listener).

%% API
-export([start_link/2, start_link/3
        ,handle_member_call/2
        ,handle_member_call_success/2
        ,handle_member_call_cancel/2
        ,handle_agent_change/2
        ,handle_agents_available_req/2
        ,handle_queue_member_add/2
        ,handle_queue_member_remove/2
        ,handle_member_callback_reg/2
        ,are_agents_available/1
        ,handle_config_change/2
        ,should_ignore_member_call/3, should_ignore_member_call/4
        ,up_next/2
        ,config/1
        ,status/1
        ,current_agents/1
        ,refresh/2
        ,callback_details/2
        ]).

%% FSM helpers
-export([pick_winner/2]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-ifdef(TEST).
-export([ss_size/2
        ,update_strategy_with_agent/7
        ]).
-endif.

-include("acdc.hrl").
-include("acdc_queue_manager.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS(A, Q), [{'conf', [{'type', <<"queue">>}
                                  ,{'db', kz_util:format_account_id(A, 'encoded')}
                                  ,{'id', Q}
                                  ,'federate'
                                  ]}
                        ,{'acdc_queue', [{'restrict_to', ['stats_req', 'agent_change', 'agents_availability'
                                                         ,'member_addremove', 'member_call_result', 'member_callback_reg'
                                                         ]}
                                        ,{'account_id', A}
                                        ,{'queue_id', Q}
                                        ]}
                        ,{'presence', [{'restrict_to', ['probe']}]}
                        ,{'acdc_stats', [{'restrict_to', ['status_stat']}
                                        ,{'account_id', A}
                                        ]}
                        ]).
-define(AGENT_BINDINGS(AccountId, AgentId), [
                                            ]).

-define(RESPONDERS, [{{'acdc_queue_handler', 'handle_config_change'}
                     ,[{<<"configuration">>, <<"*">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_stats_req'}
                     ,[{<<"queue">>, <<"stats_req">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_presence_probe'}
                     ,[{<<"presence">>, <<"probe">>}]
                     }
                    ,{{?MODULE, 'handle_member_call'}
                     ,[{<<"member">>, <<"call">>}]
                     }
                    ,{{?MODULE, 'handle_member_call_success'}
                     ,[{<<"member">>, <<"call_success">>}]
                     }
                    ,{{?MODULE, 'handle_member_call_cancel'}
                     ,[{<<"member">>, <<"call_cancel">>}]
                     }
                    ,{{?MODULE, 'handle_agent_change'}
                     ,[{<<"queue">>, <<"agent_change">>}]
                     }
                    ,{{?MODULE, 'handle_agents_available_req'}
                     ,[{<<"queue">>, <<"agents_available_req">>}]
                     }
                    ,{{?MODULE, 'handle_queue_member_add'}
                     ,[{<<"queue">>, <<"member_add">>}]
                     }
                    ,{{?MODULE, 'handle_queue_member_remove'}
                     ,[{<<"queue">>, <<"member_remove">>}]
                     }
                    ,{{?MODULE, 'handle_member_callback_reg'}
                     ,[{<<"member">>, <<"callback_reg">>}]
                     }
                    ]).

-define(SECONDARY_BINDINGS(AccountId, QueueId)
       ,[{'acdc_queue', [{'restrict_to', ['member_call']}
                        ,{'account_id', AccountId}
                        ,{'queue_id', QueueId}
                        ]}
        ]).
-define(SECONDARY_QUEUE_NAME(QueueId), <<"acdc.queue.manager.", QueueId/binary>>).
-define(SECONDARY_QUEUE_OPTIONS(MaxPriority), [{'exclusive', 'false'}
                                              ,{'arguments',[{<<"x-max-priority">>, MaxPriority}]}
                                              ]).
-define(SECONDARY_CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(pid(), kz_json:object()) -> startlink_ret().
start_link(Super, QueueJObj) ->
    AccountId = kz_doc:account_id(QueueJObj),
    QueueId = kz_doc:id(QueueJObj),

    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(AccountId, QueueId)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Super, QueueJObj]
                           ).

-spec start_link(pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(Super, AccountId, QueueId) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(AccountId, QueueId)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Super, AccountId, QueueId]
                           ).

-spec handle_member_call(kz_json:object(), kz_proplist()) -> 'ok'.
handle_member_call(JObj, Props) ->
    'true' = kapi_acdc_queue:member_call_v(JObj),
    _ = kz_util:put_callid(JObj),

    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),

    case are_agents_available(props:get_value('server', Props)
                             ,props:get_value('enter_when_empty', Props)
                             )
    of
        'false' ->
            lager:info("no agents are available to take the call, cancel queueing"),
            gen_listener:cast(props:get_value('server', Props)
                             ,{'reject_member_call', Call, JObj}
                             );
        'true' ->
            start_queue_call(JObj, Props, Call, kz_json:is_true(<<"Enter-As-Callback">>, JObj))
    end.

-spec are_agents_available(server_ref()) -> boolean().
are_agents_available(Srv) ->
    are_agents_available(Srv, gen_listener:call(Srv, 'enter_when_empty')).

are_agents_available(Srv, EnterWhenEmpty) ->
    agents_available(Srv) > 0
        orelse EnterWhenEmpty.

start_queue_call(JObj, Props, Call, 'false') ->
    _ = kapps_call:put_callid(Call),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),

    lager:info("member call for queue ~s recv", [QueueId]),
    lager:debug("answering call"),
    kapps_call_command:answer_now(Call),

    case kz_media_util:media_path(props:get_value('moh', Props)
                                 ,kapps_call:account_id(Call)
                                 )
    of
        'undefined' ->
            lager:debug("using default moh"),
            kapps_call_command:hold(Call);
        MOH ->
            lager:debug("using MOH ~s (~p)", [MOH, Props]),
            kapps_call_command:hold(MOH, Call)
    end,

    JObj2 = kz_json:set_value([<<"Call">>, <<"Custom-Channel-Vars">>, <<"Queue-ID">>], QueueId, JObj),

    _ = kapps_call_command:set('undefined'
                              ,kz_json:from_list([{<<"Eavesdrop-Group-ID">>, QueueId}
                                                 ,{<<"Queue-ID">>, QueueId}
                                                 ])
                              ,Call
                              ),

    %% Add member to queue for tracking position
    gen_listener:cast(props:get_value('server', Props), {'add_queue_member', JObj2});
start_queue_call(JObj, Props, Call, 'true') ->
    _ = kapps_call:put_callid(Call),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),

    lager:info("member callback for queue ~s recv", [QueueId]),

    JObj2 = kz_json:set_value([<<"Call">>, <<"Custom-Channel-Vars">>, <<"Queue-ID">>], QueueId, JObj),

    %% Add member to queue for tracking position
    gen_listener:cast(props:get_value('server', Props), {'add_queue_member', JObj2}).

-spec handle_member_call_success(kz_json:object(), kz_proplist()) -> 'ok'.
handle_member_call_success(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_remove', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec handle_member_call_cancel(kz_json:object(), kz_proplist()) -> 'ok'.
handle_member_call_cancel(JObj, Props) ->
    kz_util:put_callid(JObj),
    lager:debug("cancel call ~p", [JObj]),
    'true' = kapi_acdc_queue:member_call_cancel_v(JObj),
    K = make_ignore_key(kz_json:get_value(<<"Account-ID">>, JObj)
                       ,kz_json:get_value(<<"Queue-ID">>, JObj)
                       ,kz_json:get_value(<<"Call-ID">>, JObj)
                       ),
    gen_listener:cast(props:get_value('server', Props), {'member_call_cancel', K, JObj}).

-spec handle_agent_change(kz_json:object(), kz_proplist()) -> 'ok'.
handle_agent_change(JObj, Prop) ->
    'true' = kapi_acdc_queue:agent_change_v(JObj),
    Server = props:get_value('server', Prop),
    case kz_json:get_value(<<"Change">>, JObj) of
        <<"available">> ->
            gen_listener:cast(Server, {'agent_available', JObj});
        <<"ringing">> ->
            gen_listener:cast(Server, {'agent_ringing', JObj});
        <<"busy">> ->
            gen_listener:cast(Server, {'agent_busy', JObj});
        <<"unavailable">> ->
            gen_listener:cast(Server, {'agent_unavailable', JObj})
    end.

-spec handle_agents_available_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_agents_available_req(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'agents_available_req', JObj}).

-spec handle_queue_member_add(kz_json:object(), kz_proplist()) -> 'ok'.
handle_queue_member_add(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_add', JObj}).

-spec handle_queue_member_remove(kz_json:object(), kz_proplist()) -> 'ok'.
handle_queue_member_remove(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_remove', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec handle_member_callback_reg(kz_json:object(), kz_proplist()) -> 'ok'.
handle_member_callback_reg(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_member_callback_reg', JObj}).

-spec handle_config_change(server_ref(), kz_json:object()) -> 'ok'.
handle_config_change(Srv, JObj) ->
    gen_listener:cast(Srv, {'update_queue_config', JObj}).

-spec should_ignore_member_call(server_ref(), kapps_call:call(), kz_json:object()) -> boolean().
-spec should_ignore_member_call(server_ref(), kapps_call:call(), ne_binary(), ne_binary()) -> boolean().
should_ignore_member_call(Srv, Call, CallJObj) ->
    should_ignore_member_call(Srv
                             ,Call
                             ,kz_json:get_value(<<"Account-ID">>, CallJObj)
                             ,kz_json:get_value(<<"Queue-ID">>, CallJObj)
                             ).
should_ignore_member_call(Srv, Call, AccountId, QueueId) ->
    K = make_ignore_key(AccountId, QueueId, kapps_call:call_id(Call)),
    gen_listener:call(Srv, {'should_ignore_member_call', K}).

-spec up_next(pid(), ne_binary()) -> boolean().
up_next(Srv, CallId) ->
    gen_listener:call(Srv, {'up_next', CallId}).

-spec config(pid()) -> {ne_binary(), ne_binary()}.
config(Srv) -> gen_listener:call(Srv, 'config').

-spec current_agents(server_ref()) -> ne_binaries().
current_agents(Srv) -> gen_listener:call(Srv, 'current_agents').

-spec status(pid()) -> ne_binaries().
status(Srv) -> gen_listener:call(Srv, 'status').

-spec refresh(pid(), kz_json:object()) -> 'ok'.
refresh(Mgr, QueueJObj) -> gen_listener:cast(Mgr, {'refresh', QueueJObj}).

strategy(Srv) -> gen_listener:call(Srv, 'strategy').
next_winner(Srv) -> gen_listener:call(Srv, 'next_winner').

agents_available(Srv) -> gen_listener:call(Srv, 'agents_available').

-spec pick_winner(pid(), kz_json:objects()) ->
                         'undefined' |
                         {kz_json:objects(), kz_json:objects()}.
pick_winner(Srv, Resps) -> pick_winner(Resps, strategy(Srv), next_winner(Srv)).

-spec callback_details(pid(), ne_binary()) -> api_binary().
callback_details(Srv, CallId) ->
    gen_listener:call(Srv, {'callback_details', CallId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%--------------------------------------------------------------------
-spec init([pid() | kz_json:object() | ne_binary()]) -> {'ok', mgr_state()}.
init([Super, QueueJObj]) ->
    AccountId = kz_doc:account_id(QueueJObj),
    QueueId = kz_doc:id(QueueJObj),

    kz_util:put_callid(<<"mgr_", QueueId/binary>>),

    init(Super, AccountId, QueueId, QueueJObj);

init([Super, AccountId, QueueId]) ->
    kz_util:put_callid(<<"mgr_", QueueId/binary>>),

    AcctDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', QueueJObj} = kz_datamgr:open_cache_doc(AcctDb, QueueId),

    init(Super, AccountId, QueueId, QueueJObj).

init(Super, AccountId, QueueId, QueueJObj) ->
    process_flag('trap_exit', 'false'),

    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    kz_datamgr:add_to_doc_cache(AccountDb, QueueId, QueueJObj),

    _ = start_secondary_queue(AccountId, QueueId),

    gen_listener:cast(self(), {'start_workers'}),
    Strategy = get_strategy(kz_json:get_value(<<"strategy">>, QueueJObj)),
    StrategyState = create_strategy_state(Strategy, AccountDb, QueueId),

    _ = update_strategy_state(self(), Strategy, StrategyState),

    lager:debug("queue mgr started for ~s", [QueueId]),
    {'ok', update_properties(QueueJObj, #state{account_id=AccountId
                                              ,queue_id=QueueId
                                              ,supervisor=Super
                                              ,strategy=Strategy
                                              ,strategy_state=StrategyState
                                              })}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {'reply', Reply, State} |
%%                                   {'reply', Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), mgr_state()) -> handle_call_ret_state(mgr_state()).
handle_call({'should_ignore_member_call', {AccountId, QueueId, CallId}=K}, _, #state{ignored_member_calls=Dict
                                                                                    ,account_id=AccountId
                                                                                    ,queue_id=QueueId
                                                                                    }=State) ->
    case catch dict:fetch(K, Dict) of
        {'EXIT', _} -> {'reply', 'false', State};
        _Res ->
            publish_queue_member_remove(AccountId, QueueId, CallId),
            {'reply', 'true', State#state{ignored_member_calls=dict:erase(K, Dict)}}
    end;

handle_call({'up_next', CallId}, _, #state{strategy_state=SS
                                          ,current_member_calls=CurrentCalls
                                          }=State) ->
    FreeAgents = ss_size(SS, 'free'),
    Position = call_position(CallId, lists:reverse(CurrentCalls)),
    {'reply', FreeAgents >= Position, State};

handle_call('config', _, #state{account_id=AccountId
                               ,queue_id=QueueId
                               }=State) ->
    {'reply', {AccountId, QueueId}, State};

handle_call('status', _, #state{strategy_state=#strategy_state{details=Details}}=State) ->
    Known = [A || {A, {N, _}} <- dict:to_list(Details), N > 0],
    {'reply', Known, State};

handle_call('strategy', _, #state{strategy=Strategy}=State) ->
    {'reply', Strategy, State, 'hibernate'};

handle_call('agents_available', _, #state{strategy_state=SS}=State) ->
    {'reply', ss_size(SS, 'logged_in'), State};

handle_call('enter_when_empty', _, #state{enter_when_empty=EnterWhenEmpty}=State) ->
    {'reply', EnterWhenEmpty, State};

handle_call('next_winner', _, #state{strategy='mi'}=State) ->
    {'reply', 'undefined', State};
handle_call('next_winner', _, #state{strategy='rr'
                                    ,strategy_state=#strategy_state{agents=Agents}=SS
                                    }=State) ->
    case pqueue4:pout(Agents) of
        {{'value', Winner, Priority}, Agents1} ->
            {'reply', Winner, State#state{strategy_state=SS#strategy_state{agents=pqueue4:in(Winner, Priority, Agents1)}}, 'hibernate'};
        {'empty', _} ->
            {'reply', 'undefined', State}
    end;

handle_call('current_agents', _, #state{strategy='rr'
                                       ,strategy_state=#strategy_state{agents=Q
                                                                      ,ringing_agents=RingingAgents
                                                                      ,busy_agents=BusyAgents
                                                                      }
                                       }=State) ->
    {'reply', pqueue4:to_list(Q) ++ RingingAgents ++ BusyAgents, State};
handle_call('current_agents', _, #state{strategy='mi'
                                       ,strategy_state=#strategy_state{agents=L}
                                       }=State) ->
    {'reply', L, State};

handle_call({'queue_position', CallId}, _, #state{current_member_calls=CurrentCalls}=State) ->
    Position = call_position(CallId, lists:reverse(CurrentCalls)),
    {'reply', Position, State};

handle_call({'callback_details', CallId}, _, #state{registered_callbacks=Callbacks}=State) ->
    {'reply', props:get_value(CallId, Callbacks), State};

handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), mgr_state()) -> handle_cast_ret_state(mgr_state()).
handle_cast({'update_strategy', StrategyState}, State) ->
    {'noreply', State#state{strategy_state=StrategyState}, 'hibernate'};

handle_cast({'update_queue_config', JObj}, #state{enter_when_empty=_EnterWhenEmpty}=State) ->
    EWE = kz_json:is_true([<<"Doc">>, <<"enter_when_empty">>], JObj, 'true'),
    lager:debug("maybe changing ewe from ~s to ~s", [_EnterWhenEmpty, EWE]),
    {'noreply', State#state{enter_when_empty=EWE}, 'hibernate'};

handle_cast({'member_call_cancel', K, JObj}, #state{ignored_member_calls=Dict}=State) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    Reason = kz_json:get_value(<<"Reason">>, JObj),

    acdc_stats:call_abandoned(AccountId, QueueId, CallId, Reason),
    case Reason of
        %% Don't add to ignored_member_calls because an FSM has already dealt with this call
        <<"No agents left in queue">> ->
            {'noreply', State};
        _ ->
            {'noreply', State#state{ignored_member_calls=dict:store(K, 'true', Dict)}}
    end;

handle_cast({'monitor_call', Call}, State) ->
    CallId = kapps_call:call_id(Call),
    gen_listener:add_binding(self(), 'call', [{'callid', CallId}
                                             ,{'restrict_to', [<<"CHANNEL_DESTROY">>]}
                                             ]),
    lager:debug("bound for call events for ~s", [CallId]),
    {'noreply', State};
handle_cast({'start_workers'}, #state{account_id=AccountId
                                     ,queue_id=QueueId
                                     ,supervisor=QueueSup
                                     }=State) ->
    WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
    case kz_datamgr:get_results(kz_util:format_account_id(AccountId, 'encoded')
                               ,<<"queues/agents_listing">>
                               ,[{'key', QueueId}
                                ,'include_docs'
                                ])
    of
        {'ok', []} ->
            lager:debug("no agents yet, but create a worker anyway"),
            acdc_queue_workers_sup:new_worker(WorkersSup, AccountId, QueueId);
        {'ok', Agents} ->
            _ = [start_agent_and_worker(WorkersSup, AccountId, QueueId
                                       ,kz_json:get_value(<<"doc">>, A)
                                       )
                 || A <- Agents
                ],
            'ok';
        {'error', _E} ->
            lager:debug("failed to find agent count: ~p", [_E]),
            QWC = kapps_config:get_integer(?CONFIG_CAT, <<"queue_worker_count">>, 5),
            acdc_queue_workers_sup:new_workers(WorkersSup, AccountId, QueueId, QWC)
    end,
    {'noreply', State};

handle_cast({'start_worker'}, State) ->
    handle_cast({'start_worker', 1}, State);
handle_cast({'start_worker', N}, #state{account_id=AccountId
                                       ,queue_id=QueueId
                                       ,supervisor=QueueSup
                                       }=State) ->
    WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
    acdc_queue_workers_sup:new_workers(WorkersSup, AccountId, QueueId, N),
    {'noreply', State};

handle_cast({'agent_available', AgentId, Priority, Skills}, #state{strategy=Strategy
                                                                  ,strategy_state=StrategyState
                                                                  ,supervisor=QueueSup
                                                                  }=State) when is_binary(AgentId) ->
    StrategyState1 = update_strategy_with_agent(Strategy, StrategyState, AgentId, Priority, Skills, 'add', 'undefined'),
    maybe_start_queue_workers(QueueSup, ss_size(StrategyState1, 'logged_in')),
    {'noreply', State#state{strategy_state=StrategyState1}
    ,'hibernate'};
handle_cast({'agent_available', JObj}, State) ->
    handle_cast({'agent_available'
                ,kz_json:get_ne_binary_value(<<"Agent-ID">>, JObj)
                ,kz_json:get_integer_value(<<"Priority">>, JObj, 0)
                ,kz_json:get_list_value(<<"Skills">>, JObj, [])
                }, State);

handle_cast({'agent_ringing', AgentId, Priority}, #state{strategy=Strategy
                                                        ,strategy_state=StrategyState
                                                        }=State) when is_binary(AgentId) ->
    lager:info("agent ~s ringing, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = update_strategy_with_agent(Strategy, StrategyState, AgentId, Priority, [], 'remove', 'ringing'),
    {'noreply', State#state{strategy_state=StrategyState1}
    ,'hibernate'};
handle_cast({'agent_ringing', JObj}, State) ->
    handle_cast({'agent_ringing'
                ,kz_json:get_ne_binary_value(<<"Agent-ID">>, JObj)
                ,kz_json:get_integer_value(<<"Priority">>, JObj, 0)
                }, State);

handle_cast({'agent_busy', AgentId, Priority}, #state{strategy=Strategy
                                                     ,strategy_state=StrategyState
                                                     }=State) when is_binary(AgentId) ->
    lager:info("agent ~s busy, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = update_strategy_with_agent(Strategy, StrategyState, AgentId, Priority, [], 'remove', 'busy'),
    {'noreply', State#state{strategy_state=StrategyState1}
    ,'hibernate'};
handle_cast({'agent_busy', JObj}, State) ->
    handle_cast({'agent_busy'
                ,kz_json:get_ne_binary_value(<<"Agent-ID">>, JObj)
                ,kz_json:get_integer_value(<<"Priority">>, JObj, 0)
                }, State);

handle_cast({'agent_unavailable', AgentId, Priority}, #state{strategy=Strategy
                                                            ,strategy_state=StrategyState
                                                            }=State) when is_binary(AgentId) ->
    lager:info("agent ~s unavailable, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = update_strategy_with_agent(Strategy, StrategyState, AgentId, Priority, [], 'remove', 'undefined'),
    {'noreply', State#state{strategy_state=StrategyState1}
    ,'hibernate'};
handle_cast({'agent_unavailable', JObj}, State) ->
    handle_cast({'agent_unavailable'
                ,kz_json:get_ne_binary_value(<<"Agent-ID">>, JObj)
                ,kz_json:get_integer_value(<<"Priority">>, JObj, 0)
                }, State);

handle_cast({'agents_available_req', JObj}, #state{account_id=AccountId
                                                  ,queue_id=QueueId
                                                  ,strategy_state=StrategyState
                                                  }=State) ->
    Resp = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Agent-Count">>, ss_size(StrategyState, 'logged_in')}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    Q = kz_json:get_value(<<"Server-ID">>, JObj),
    kapi_acdc_queue:publish_agents_available_resp(Q, Resp),
    {'noreply', State};

handle_cast({'reject_member_call', Call, JObj}, #state{account_id=AccountId
                                                      ,queue_id=QueueId
                                                      }=State) ->
    Prop = [{<<"Call-ID">>, kapps_call:call_id(Call)}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Failure-Reason">>, <<"no agents">>}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    Q = kz_json:get_value(<<"Server-ID">>, JObj),
    catch kapi_acdc_queue:publish_member_call_failure(Q, Prop),
    {'noreply', State};

handle_cast({'sync_with_agent', A}, #state{account_id=AccountId}=State) ->
    case acdc_agent_util:most_recent_status(AccountId, A) of
        {'ok', <<"logged_out">>} -> gen_listener:cast(self(), {'agent_unavailable', A, 0});
        _ -> 'ok'
    end,
    {'noreply', State};

handle_cast({'gen_listener', {'created_queue', _}}, State) ->
    {'noreply', State};

handle_cast({'refresh', QueueJObj}, State) ->
    lager:debug("refreshing queue configs"),
    {'noreply', update_properties(QueueJObj, State), 'hibernate'};

handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};

handle_cast({'add_queue_member', JObj}, #state{account_id=AccountId
                                              ,queue_id=QueueId
                                              ,current_member_calls=CurrentCalls
                                              }=State) ->
    Position = length(CurrentCalls)+1,
    Call = kapps_call:set_custom_channel_var(<<"Queue-Position">>
                                            ,Position
                                            ,kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj))),

    {CIDNumber, CIDName} = acdc_util:caller_id(Call),
    acdc_stats:call_waiting(AccountId, QueueId, Position
                           ,kapps_call:call_id(Call)
                           ,CIDName
                           ,CIDNumber
                           ,kz_json:get_integer_value(<<"Member-Priority">>, JObj)
                           ),

    publish_queue_member_add(AccountId, QueueId, Call
                            ,kz_json:is_true(<<"Enter-As-Callback">>, JObj)
                            ,kz_json:get_binary_value(<<"Callback-Number">>, JObj)
                            ),

    %% Add call to shared queue
    kapi_acdc_queue:publish_shared_member_call(AccountId, QueueId, JObj),
    lager:debug("put call into shared messaging queue"),

    gen_listener:cast(self(), {'monitor_call', Call}),

    acdc_util:presence_update(AccountId, QueueId, ?PRESENCE_RED_FLASH),

    State1 = State#state{current_member_calls=[Call | CurrentCalls]},
    State2 = lists:foldl(fun(Updater, StateAcc) -> Updater(JObj, Call, StateAcc) end
                        ,State1
                        ,[fun maybe_schedule_position_announcements/3
                         ,fun maybe_add_queue_member_as_callback/3
                         ]),
    {'noreply', State2};

handle_cast({'handle_queue_member_add', JObj}, #state{current_member_calls=CurrentCalls}=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    CallId = kapps_call:call_id(Call),
    lager:debug("received notification of new queue member ~s", [CallId]),

    State1 = State#state{current_member_calls=[Call | lists:keydelete(CallId, 2, CurrentCalls)]},
    State2 = maybe_add_queue_member_as_callback(JObj, Call, State1),
    {'noreply', State2};

handle_cast({'handle_queue_member_remove', CallId}, State) ->
    State1 = remove_queue_member(CallId, State),
    State2 = maybe_remove_callback_reg(CallId, State1),
    {'noreply', State2};

handle_cast({'handle_member_callback_reg', JObj}, #state{account_id=AccountId
                                                        ,queue_id=QueueId
                                                        ,current_member_calls=CurrentCalls
                                                        ,announcements_pids=Pids
                                                        ,registered_callbacks=RegCallbacks}=State) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case lists:keyfind(CallId, 2, CurrentCalls) of
        'false' ->
            lager:debug("not accepting callback reg for ~s (call not in my list of calls)", [CallId]),
            {'noreply', State};
        Call ->
            lager:debug("call ~s marked as callback", [CallId]),
            Number = kz_json:get_value(<<"Number">>, JObj),
            Call1 = callback_flag(AccountId, QueueId, Call),
            CIDPrepend = kapps_call:kvs_fetch('prepend_cid_name', Call1),
            {'noreply', State#state{current_member_calls=lists:keyreplace(CallId, 2, CurrentCalls, Call1)
                                   ,announcements_pids=cancel_position_announcements(Call, Pids)
                                   ,registered_callbacks=[{CallId, {Number, CIDPrepend}} | RegCallbacks]
                                   }}
    end;

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), mgr_state()) -> handle_info_ret_state(mgr_state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), mgr_state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{enter_when_empty=EnterWhenEmpty
                          ,moh=MOH
                          }) ->
    {'reply', [{'enter_when_empty', EnterWhenEmpty}
              ,{'moh', MOH}
              ]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), mgr_state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("queue manager terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), mgr_state(), any()) -> {'ok', mgr_state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_secondary_queue(AccountId, QueueId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Priority = lookup_priority_levels(AccountDb, QueueId),
    kz_util:spawn(fun gen_listener:add_queue/4
                 ,[self()
                  ,?SECONDARY_QUEUE_NAME(QueueId)
                  ,[{'queue_options', ?SECONDARY_QUEUE_OPTIONS(Priority)}
                   ,{'consume_options', ?SECONDARY_CONSUME_OPTIONS}
                   ]
                  ,?SECONDARY_BINDINGS(AccountId, QueueId)
                  ]).

-spec lookup_priority_levels(ne_binary(), ne_binary()) -> api_integer().
lookup_priority_levels(AccountDB, QueueId) ->
    case kz_datamgr:open_cache_doc(AccountDB, QueueId) of
        {'ok', JObj} -> kz_json:get_value(<<"max_priority">>, JObj);
        _ -> 'undefined'
    end.

make_ignore_key(AccountId, QueueId, CallId) ->
    {AccountId, QueueId, CallId}.

-spec publish_queue_member_add(ne_binary(), ne_binary(), kapps_call:call(), boolean(), api_binary()) -> 'ok'.
publish_queue_member_add(AccountId, QueueId, Call, EnterAsCallback, CallbackNumber) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call">>, kapps_call:to_json(Call)}
             ,{<<"Enter-As-Callback">>, EnterAsCallback}
             ,{<<"Callback-Number">>, CallbackNumber}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapi_acdc_queue:publish_queue_member_add(Prop).

-spec publish_queue_member_remove(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
publish_queue_member_remove(AccountId, QueueId, CallId) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Call-ID">>, CallId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_queue_member_remove(Prop).

-spec start_agent_and_worker(pid(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
start_agent_and_worker(WorkersSup, AccountId, QueueId, AgentJObj) ->
    acdc_queue_workers_sup:new_worker(WorkersSup, AccountId, QueueId),
    AgentId = kz_doc:id(AgentJObj),
    case acdc_agent_util:most_recent_status(AccountId, AgentId) of
        {'ok', <<"logout">>} -> 'ok';
        {'ok', <<"logged_out">>} -> 'ok';
        {'ok', _Status} ->
            lager:debug("maybe starting agent ~s(~s) for queue ~s", [AgentId, _Status, QueueId]),

            case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
                'undefined' -> acdc_agents_sup:new(AgentJObj);
                P when is_pid(P) -> 'ok'
            end
    end.

%% Really sophisticated selection algorithm
-spec pick_winner(kz_json:objects(), queue_strategy(), api_binary()) ->
                         {kz_json:objects(), kz_json:objects()}.
pick_winner(CRs, 'rr', AgentId) ->
    case split_agents(AgentId, CRs) of
        {[], _O} ->
            lager:debug("oops, agent ~s appears to have not responded; try again", [AgentId]),
            {[], []};
        {Winners, OtherAgents} ->
            lager:debug("found winning responders for agent: ~s", [AgentId]),
            {Winners, OtherAgents}
    end;
pick_winner(CRs, 'mi', _) ->
    [MostIdle | Rest] = lists:usort(fun sort_agent/2, CRs),
    AgentId = kz_json:get_value(<<"Agent-ID">>, MostIdle),
    {Same, Other} = split_agents(AgentId, Rest),

    {[MostIdle|Same], Other}.

-spec update_strategy_with_agent(queue_strategy(), strategy_state(), ne_binary(), agent_priority(), ne_binaries(), 'add' | 'remove', 'ringing' | 'busy' | 'undefined') ->
                                        strategy_state().
update_strategy_with_agent('rr', SS, AgentId, Priority, _, Action, Flag) ->
    update_rr_strategy_with_agent(SS, AgentId, Priority, Action, Flag);
update_strategy_with_agent('mi', SS, AgentId, _, _, Action, Flag) ->
    update_mi_strategy_with_agent(SS, AgentId, Action, Flag).

-spec update_rr_strategy_with_agent(strategy_state(), ne_binary(), agent_priority(), 'add' | 'remove', 'ringing' | 'busy' | 'undefined') ->
                                           strategy_state().
update_rr_strategy_with_agent(#strategy_state{agents=AgentQueue
                                             ,details=Details
                                             }=SS
                             ,AgentId, Priority, 'add', Flag
                             ) ->
    SS1 = case pqueue4:remove_unique(fun(AgentId1) when AgentId =:= AgentId1 ->
                                             'true';
                                        (_) ->
                                             'false'
                                     end, AgentQueue) of
              {'true', AgentQueue1} ->
                  lager:info("re-adding agent ~s (prio ~b) to strategy rr", [AgentId, -1 * Priority]),
                  SS#strategy_state{agents=pqueue4:in(AgentId, Priority, AgentQueue1)};
              {'false', AgentQueue1} ->
                  lager:info("adding agent ~s (prio ~b) to strategy rr", [AgentId, -1 * Priority]),
                  SS#strategy_state{agents=pqueue4:in(AgentId, Priority, AgentQueue1)
                                   ,details=incr_agent(AgentId, Details)
                                   }
          end,
    set_flag(AgentId, Flag, SS1);
update_rr_strategy_with_agent(#strategy_state{agents=AgentQueue}=SS, AgentId, _Priority, 'remove', Flag) ->
    SS1 = case lists:member(AgentId, pqueue4:to_list(AgentQueue)) of
              'false' -> SS;
              'true' ->
                  lager:info("removing agent ~s from strategy rr", [AgentId]),
                  remove_agent('rr', AgentId, SS)
          end,
    set_flag(AgentId, Flag, SS1).

-spec update_mi_strategy_with_agent(strategy_state(), ne_binary(), 'add' | 'remove', 'ringing' | 'busy' | 'undefined') ->
                                           strategy_state().
update_mi_strategy_with_agent(#strategy_state{agents=AgentL
                                             ,details=Details
                                             }=SS
                             ,AgentId, 'add', Flag
                             ) ->
    SS1 = case lists:member(AgentId, AgentL) of
              'true' -> SS;
              'false' ->
                  lager:info("adding agent ~s to strategy mi", [AgentId]),
                  SS#strategy_state{agents=[AgentId | AgentL]
                                   ,details=incr_agent(AgentId, Details)
                                   }
          end,
    set_flag(AgentId, Flag, SS1);
update_mi_strategy_with_agent(#strategy_state{agents=AgentL}=SS, AgentId, 'remove', Flag) ->
    SS1 = case lists:member(AgentId, AgentL) of
              'false' -> SS;
              'true' ->
                  lager:info("removing agent ~s from strategy mi", [AgentId]),
                  remove_agent('mi', AgentId, SS)
          end,
    set_flag(AgentId, Flag, SS1).

-spec remove_agent(queue_strategy(), ne_binary(), strategy_state()) -> strategy_state().
remove_agent('rr', AgentId, #strategy_state{agents=AgentQueue
                                           ,details=Details
                                           }=SS) ->
    case dict:find(AgentId, Details) of
        {'ok', {Count, _}} when Count > 1 ->
            SS#strategy_state{details=decr_agent(AgentId, Details)};
        _ ->
            {_, AgentQueue1} = pqueue4:remove_unique(fun(AgentId1) when AgentId1 =:= AgentId -> 'true';
                                                        (_) -> 'false'
                                                     end
                                                    ,AgentQueue
                                                    ),
            SS#strategy_state{agents=AgentQueue1
                             ,details=decr_agent(AgentId, Details)
                             }
    end;
remove_agent('mi', AgentId, #strategy_state{agents=AgentL
                                           ,details=Details
                                           }=SS) ->
    case dict:find(AgentId, Details) of
        {'ok', {Count, _}} when Count > 1 ->
            SS#strategy_state{details=decr_agent(AgentId, Details)};
        _ ->
            SS#strategy_state{agents=[A || A <- AgentL, A =/= AgentId]
                             ,details=decr_agent(AgentId, Details)
                             }
    end.

-spec incr_agent(ne_binary(), dict:dict()) -> dict:dict().
incr_agent(AgentId, Details) ->
    dict:update(AgentId, fun({Count, Flag}) -> {Count + 1, Flag} end, {1, 'undefined'}, Details).

-spec decr_agent(ne_binary(), dict:dict()) -> dict:dict().
decr_agent(AgentId, Details) ->
    dict:update(AgentId, fun({Count, Flag}) when Count > 1 -> {Count - 1, Flag};
                            ({_, Flag}) -> {0, Flag} end
               ,{0, 'undefined'}, Details).

-spec set_flag(ne_binary(), 'ringing' | 'busy' | 'undefined', strategy_state()) -> strategy_state().
set_flag(AgentId, Flag, #strategy_state{details=Details
                                       ,ringing_agents=RingingAgents
                                       ,busy_agents=BusyAgents
                                       }=SS) ->
    RingingAgents1 = case Flag of
                         'ringing' -> [AgentId | lists:delete(AgentId, RingingAgents)];
                         _ -> lists:delete(AgentId, RingingAgents)
                     end,
    BusyAgents1 = case Flag of
                      'busy' -> [AgentId | lists:delete(AgentId, BusyAgents)];
                      _ -> lists:delete(AgentId, BusyAgents)
                  end,
    SS#strategy_state{details=dict:update(AgentId, fun({Count, _}) -> {Count, Flag} end, {0, Flag}, Details)
                     ,ringing_agents=RingingAgents1
                     ,busy_agents=BusyAgents1
                     }.

%% If A's idle time is greater, it should come before B
-spec sort_agent(kz_json:object(), kz_json:object()) -> boolean().
sort_agent(A, B) ->
    sort_agent2(kz_json:get_integer_value(<<"Idle-Time">>, A)
               ,kz_json:get_integer_value(<<"Idle-Time">>, B)).

-spec sort_agent2(api_integer(), api_integer()) -> boolean().
sort_agent2('undefined', _) -> 'true';
sort_agent2(_, 'undefined') -> 'false';
sort_agent2(A, B) -> A > B.

-spec split_agents(ne_binary(), kz_json:objects()) ->
                          {kz_json:objects(), kz_json:objects()}.
split_agents(AgentId, Rest) ->
    lists:partition(fun(R) ->
                            AgentId =:= kz_json:get_value(<<"Agent-ID">>, R)
                    end, Rest).

-spec get_strategy(api_binary()) -> queue_strategy().
get_strategy(<<"round_robin">>) -> 'rr';
get_strategy(<<"most_idle">>) -> 'mi';
get_strategy(_) -> 'rr'.

-spec create_strategy_state(queue_strategy()
                           ,strategy_state()
                           ,ne_binary(), ne_binary()
                           ) -> strategy_state().
create_strategy_state(Strategy, AcctDb, QueueId) ->
    create_strategy_state(Strategy, #strategy_state{}, AcctDb, QueueId).

create_strategy_state('rr', #strategy_state{agents='undefined'}=SS, AcctDb, QueueId) ->
    create_strategy_state('rr', SS#strategy_state{agents=pqueue4:new()}, AcctDb, QueueId);
create_strategy_state('rr', #strategy_state{agents=AgentQ}=SS, AcctDb, QueueId) ->
    case kz_datamgr:get_results(AcctDb, <<"queues/agents_listing">>, [{'key', QueueId}]) of
        {'ok', []} -> lager:debug("no agents around"), SS;
        {'ok', JObjs} ->
            AgentMap = lists:map(fun(JObj) ->
                                         {kz_doc:id(JObj)
                                         ,-1 * kz_json:get_integer_value([<<"value">>, <<"agent_priority">>], JObj, 0)
                                         }
                                 end, JObjs),
            Q1 = lists:foldl(fun({AgentId, Priority}, Q) ->
                                     lager:info("adding agent ~s (prio ~b) to strategy rr", [AgentId, -1 * Priority]),
                                     pqueue4:in(AgentId, Priority, Q)
                             end
                            ,AgentQ
                            ,lists:usort(AgentMap)
                            ),
            Details = lists:foldl(fun(JObj, Acc) ->
                                          dict:store(kz_doc:id(JObj), {1, 'undefined'}, Acc)
                                  end, dict:new(), JObjs),
            SS#strategy_state{agents=Q1
                             ,details=Details
                             };
        {'error', _E} -> lager:debug("error creating strategy rr: ~p", [_E]), SS
    end;
create_strategy_state('mi', #strategy_state{agents='undefined'}=SS, AcctDb, QueueId) ->
    create_strategy_state('mi', SS#strategy_state{agents=[]}, AcctDb, QueueId);
create_strategy_state('mi', #strategy_state{agents=AgentL}=SS, AcctDb, QueueId) ->
    case kz_datamgr:get_results(AcctDb, <<"queues/agents_listing">>, [{key, QueueId}]) of
        {'ok', []} -> lager:debug("no agents around"), SS;
        {'ok', JObjs} ->
            AgentL1 = lists:foldl(fun(JObj, Acc) ->
                                          AgentId = kz_doc:id(JObj),
                                          case lists:member(AgentId, Acc) of
                                              'true' -> Acc;
                                              'false' ->
                                                  lager:info("adding agent ~s to strategy mi", [AgentId]),
                                                  [AgentId | Acc]
                                          end
                                  end, AgentL, JObjs),
            Details = lists:foldl(fun(JObj, Acc) ->
                                          dict:store(kz_doc:id(JObj), {1, 'undefined'}, Acc)
                                  end, dict:new(), JObjs),
            SS#strategy_state{agents=AgentL1
                             ,details=Details
                             };
        {'error', _E} -> lager:debug("error creating strategy mi: ~p", [_E]), SS
    end.

update_strategy_state(Srv, 'rr', #strategy_state{agents=AgentQueue}) ->
    L = pqueue4:to_list(AgentQueue),
    update_strategy_state(Srv, L);
update_strategy_state(Srv, 'mi', #strategy_state{agents=AgentL}) ->
    update_strategy_state(Srv, AgentL).
update_strategy_state(Srv, L) ->
    [gen_listener:cast(Srv, {'sync_with_agent', A}) || A <- L].

-spec call_position(ne_binary(), [kapps_call:call()]) -> api_integer().
-spec call_position(ne_binary(), [kapps_call:call()], pos_integer()) -> pos_integer().
call_position(CallId, Calls) ->
    call_position(CallId, Calls, 1).

call_position(_, [], _) ->
    'undefined';
call_position(CallId, [Call|Calls], Position) ->
    case kapps_call:call_id(Call) of
        CallId -> Position;
        _ -> call_position(CallId, Calls, Position + 1)
    end.

-spec ss_size(strategy_state(), 'free' | 'logged_in') -> integer().
ss_size(#strategy_state{agents=Agents
                       ,ringing_agents=RingingAgents
                       ,busy_agents=BusyAgents
                       }, 'logged_in') ->
    case pqueue4:is_queue(Agents) of
        'true' -> pqueue4:len(Agents);
        'false' -> length(Agents)
    end + length(RingingAgents) + length(BusyAgents);
ss_size(#strategy_state{agents=Agents
                       ,details=Details
                       ,ringing_agents=RingingAgents
                       }, 'free') when is_list(Agents) ->
    lists:foldl(fun(AgentId, Count) ->
                        case dict:find(AgentId, Details) of
                            {'ok', {ProcCount, 'undefined'}} when ProcCount > 0 -> Count + 1;
                            _ -> Count
                        end
                end, 0, Agents) + length(RingingAgents);
ss_size(#strategy_state{agents=Agents}=SS, 'free') ->
    ss_size(SS#strategy_state{agents=pqueue4:to_list(Agents)}, 'free').

maybe_start_queue_workers(QueueSup, AgentCount) ->
    WSup = acdc_queue_sup:workers_sup(QueueSup),
    case acdc_queue_workers_sup:worker_count(WSup) of
        N when N >= AgentCount -> 'ok';
        N when N < AgentCount -> gen_listener:cast(self(), {'start_worker', AgentCount-N})
    end.

-spec update_properties(kz_json:object(), mgr_state()) -> mgr_state().
update_properties(QueueJObj, State) ->
    State#state{
      enter_when_empty=kz_json:is_true(<<"enter_when_empty">>, QueueJObj, 'true')
     ,moh=kz_json:get_ne_value(<<"moh">>, QueueJObj)
     ,announcements_config=announcements_config(QueueJObj)
     }.

-spec announcements_config(kz_json:object()) -> kz_proplist().
announcements_config(Config) ->
    kz_json:recursive_to_proplist(
      kz_json:get_json_value(<<"announcements">>, Config, kz_json:new())).

-spec maybe_schedule_position_announcements(kz_json:object(), kapps_call:call(), mgr_state()) -> mgr_state().
maybe_schedule_position_announcements(JObj, Call, #state{announcements_config=AnnouncementsConfig
                                                        ,announcements_pids=AnnouncementsPids
                                                        }=State) ->
    EnterAsCallback = kz_json:is_true(<<"Enter-As-Callback">>, JObj),
    case not EnterAsCallback
        andalso acdc_announcements_sup:maybe_start_announcements(self(), Call, AnnouncementsConfig)
    of
        'false' -> State;
        {'ok', Pid} ->
            CallId = kapps_call:call_id(Call),
            State#state{announcements_pids=AnnouncementsPids#{CallId => Pid}}
    end.

-spec cancel_position_announcements(kapps_call:call() | 'false', map()) ->
                                           map().
cancel_position_announcements('false', Pids) -> Pids;
cancel_position_announcements(Call, Pids) ->
    CallId = kapps_call:call_id(Call),
    case catch maps:get(CallId, Pids) of
        {'badkey', _} ->
            lager:debug("did not have the announcements for call ~s", [CallId]),
            Pids;
        Pid ->
            lager:debug("cancelling announcements for ~s", [CallId]),
            Pids1 = maps:remove(CallId, Pids),
            acdc_announcements_sup:stop_announcements(Pid),

            %% Attempt to skip remaining announcement media, but don't flush hangups
            NoopId = kz_datamgr:get_uuid(),
            Command = [{<<"Application-Name">>, <<"noop">>}
                      ,{<<"Msg-ID">>, NoopId}
                      ,{<<"Insert-At">>, <<"now">>}
                      ,{<<"Filter-Applications">>, [<<"play">>, <<"say">>, <<"play">>]}
                      ],
            kapps_call_command:send_command(Command, Call),
            Pids1
    end.

-spec remove_queue_member(api_binary(), mgr_state()) -> mgr_state().
remove_queue_member(CallId, #state{current_member_calls=CurrentCalls
                                  ,announcements_pids=AnnouncementsPids
                                  }=State) ->
    lager:debug("removing call id ~s", [CallId]),

    publish_call_exited_position(CallId, State),
    AnnouncementsPids1 = cancel_position_announcements(lists:keyfind(CallId, 2, CurrentCalls), AnnouncementsPids),

    State#state{current_member_calls=lists:keydelete(CallId, 2, CurrentCalls)
               ,announcements_pids=AnnouncementsPids1
               }.

-spec publish_call_exited_position(api_binary(), mgr_state()) -> 'ok'.
publish_call_exited_position(CallId, #state{current_member_calls=CurrentCalls}=State) ->
    {Map, _} = lists:mapfoldr(fun(Call, Index) ->
                                      {{kapps_call:call_id(Call), Index}, Index+1}
                              end, 1, CurrentCalls),
    try_publish_call_exited_position(lists:keyfind(CallId, 1, Map), State).

-spec try_publish_call_exited_position({ne_binary(), pos_integer()} | 'false', mgr_state()) -> 'ok'.
try_publish_call_exited_position({CallId, Index}, #state{account_id=AccountId
                                                        ,queue_id=QueueId
                                                        }) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Exited-Position">>, Index}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_stats:publish_call_exited_position(Prop);
try_publish_call_exited_position('false', _) ->
    lager:error("call id not found in list of calls").

-spec maybe_add_queue_member_as_callback(kz_json:object(), kapps_call:call(), mgr_state()) -> mgr_state().
maybe_add_queue_member_as_callback(JObj, Call, #state{account_id=AccountId
                                                     ,queue_id=QueueId
                                                     ,current_member_calls=CurrentCalls
                                                     ,registered_callbacks=RegCallbacks
                                                     }=State) ->
    EnterAsCallback = kz_json:is_true(<<"Enter-As-Callback">>, JObj),
    case EnterAsCallback of
        'false' -> State;
        'true' ->
            CallId = kapps_call:call_id(Call),
            lager:debug("call ~s marked as callback", [CallId]),
            Number = kz_json:get_ne_binary_value(<<"Callback-Number">>, JObj),
            Call1 = callback_flag(AccountId, QueueId, Call),
            CIDPrepend = kapps_call:kvs_fetch('prepend_cid_name', Call1),
            State#state{current_member_calls=lists:keyreplace(CallId, 2, CurrentCalls, Call1)
                       ,registered_callbacks=props:set_value(CallId, {Number, CIDPrepend}, RegCallbacks)
                       }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepend CB: onto CID of callback calls and flag call ID as callback
%% in acdc_stats
%%
%% @end
%%--------------------------------------------------------------------
-spec callback_flag(ne_binary(), ne_binary(), kapps_call:call()) ->
                           kapps_call:call().
callback_flag(AccountId, QueueId, Call) ->
    Call1 = prepend_cid_name(<<"CB:">>, Call),
    {_, CIDName} = acdc_util:caller_id(Call1),
    acdc_stats:call_marked_callback(AccountId
                                   ,QueueId
                                   ,kapps_call:call_id(Call)
                                   ,CIDName
                                   ),
    Call1.

-spec prepend_cid_name(ne_binary(), kapps_call:call()) -> kapps_call:call().
prepend_cid_name(Prefix, Call) ->
    Prefix1 = case kapps_call:kvs_fetch('prepend_cid_name', Call) of
                  'undefined' -> Prefix;
                  Prepend -> <<Prefix/binary, Prepend/binary>>
              end,
    kapps_call:kvs_store('prepend_cid_name', Prefix1, Call).

-spec maybe_remove_callback_reg(ne_binary(), mgr_state()) -> mgr_state().
maybe_remove_callback_reg(CallId, #state{registered_callbacks=RegCallbacks}=State) ->
    State#state{registered_callbacks=lists:keydelete(CallId, 1, RegCallbacks)}.
