%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Voxter Communications Inc
%%% @doc
%%% Asterisk queue_log translator for Kazoo
%%% @end
%%% @contributors
%%%   Lucas Bussey
%%%-------------------------------------------------------------------
-module(quilt_agent_fsm).

-behaviour(gen_fsm).

-include("quilt.hrl").

-export([start_link/1]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-type state() :: #state{}.

%%
%% Public functions
%%

-spec start_link(state()) -> startlink_ret().
start_link(Agent) ->
    gen_fsm:start_link(?MODULE, Agent, []).

%%
%% gen_fsm callbacks
%%

-spec init(state()) -> {'ok', 'started', state()}.
init(State) ->
    process_flag('trap_exit', 'true'),
    {'ok', 'started', State}.

-spec handle_event(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_event(Event, StateName, State) ->
    lager:debug("unhandled event ~p from current state of ~s with state record: ~p", [Event, StateName, State]),
    {'next_state', StateName, State}.

-spec handle_sync_event(any(), {pid(),any()}, atom(), state()) -> handle_sync_event_ret(state()).
handle_sync_event({'queuelogin', JObj}, _From, StateName, #state{queues=Queues}=State)
  when StateName =:= 'started'
       orelse StateName =:= 'loggedout' ->
    {'reply', 'ok', 'ready', State#state{queues=queuelogin(JObj, Queues)}};
handle_sync_event({'queuelogin', JObj}, _From, StateName, #state{queues=Queues}=State) ->
    {'reply', 'ok', StateName, State#state{queues=queuelogin(JObj, Queues)}};

handle_sync_event({'queuelogoff', JObj}, _From, StateName, #state{queues=Queues}=State)
  when StateName =:= 'ready'
       orelse StateName =:= 'paused' ->
    State1 = State#state{queues=queuelogoff(JObj, Queues)},
    NextState = logoff_transition(StateName, State1),
    {'reply', 'ok', NextState, State1};
handle_sync_event({'queuelogoff', JObj}, _From, StateName, #state{queues=Queues}=State) ->
    {'reply', 'ok', StateName, State#state{queues=queuelogoff(JObj, Queues)}};

handle_sync_event({'agentlogin', JObj}, _From, StateName, #state{queues=Queues}=State)
  when StateName =:= 'started'
       orelse StateName =:= 'loggedout' ->
    case agentlogin(JObj, Queues) of
        [] -> {'reply', 'ok', StateName, State#state{queues=[]}};
        Queues1 -> {'reply', 'ok', 'ready', State#state{queues=Queues1}}
    end;
handle_sync_event({'agentlogin', JObj}, _From, StateName, #state{queues=Queues}=State) ->
    {'reply', 'ok', StateName, State#state{queues=agentlogin(JObj, Queues)}};

handle_sync_event({'agentlogoff', JObj}, _From, StateName, #state{queues=Queues}=State)
  when StateName =:= 'ready'
       orelse StateName =:= 'paused' ->
    {'reply', 'ok', 'loggedout', State#state{queues=agentlogoff(JObj, Queues)}};
handle_sync_event({'agentlogoff', JObj}, _From, StateName, #state{queues=Queues}=State) ->
    {'reply', 'ok', StateName, State#state{queues=agentlogoff(JObj, Queues)}};

handle_sync_event({'unpauseall', JObj}, _From, 'started', State) ->
    _ = kz_util:spawn(fun quilt_log:handle_event/1, [JObj]),
    {'reply', 'ok', 'ready', State#state{member_call_id='undefined'}};
handle_sync_event({'unpauseall', JObj}, _From, 'paused', State) ->
    _ = kz_util:spawn(fun quilt_log:handle_event/1, [JObj]),
    {'reply', 'ok', 'ready', State#state{member_call_id='undefined'}};
handle_sync_event({'pauseall', JObj}, _From, 'started', State) ->
    _ = kz_util:spawn(fun quilt_log:handle_event/1, [JObj]),
    {'reply', 'ok', 'paused', State#state{member_call_id='undefined'}};
handle_sync_event({'pauseall', JObj}, _From, 'ready', State) ->
    _ = kz_util:spawn(fun quilt_log:handle_event/1, [JObj]),
    {'reply', 'ok', 'paused', State#state{member_call_id='undefined'}};
handle_sync_event({'answer', JObj}, _From, 'started', State) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("agent answered call with ID: ~p", [CallId]),
    State1 = State#state{member_call_id=CallId},
    {'reply', 'ok', 'incall', State1};
handle_sync_event({'answer', JObj}, _From, 'ready', State) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("agent answered call with ID: ~p", [CallId]),
    State1 = State#state{member_call_id=CallId},
    {'reply', 'ok', 'incall', State1};
handle_sync_event({'transfer', JObj}, _From, 'incall', State) ->
    CallId = State#state.member_call_id,
    lager:debug("agent transferred call with ID: ~p to callee ID number: ~p", [CallId, kz_json:get_value(<<"Callee-ID-Number">>, JObj)]),
    J1 = kz_json:set_value(<<"Event-Category">>, <<"call_event">>, JObj),
    J2 = kz_json:set_value(<<"Event-Name">>, <<"transfer">>, J1),
    JObj1 = kz_json:set_value(<<"Call-ID">>, CallId, J2),

    %% If the transfer tries to read Talk-Time before stat processed, queue_log
    %% proc will crash. But we must block so that the hangup event does not
    %% occur before this one or we get COMPLETEAGENT instead of TRANSFER
    sleep_until_call_stat_processed(CallId),

    _ = kz_util:spawn(fun quilt_log:handle_event/1, [JObj1]),
    {'reply', 'ok', logoff_transition('ready', State), State#state{member_call_id='undefined'}};
handle_sync_event({'hangup', JObj}, _From, 'incall', State) ->
    _ = kz_util:spawn(fun quilt_log:handle_event/1, [JObj]),
    {'reply', 'ok', logoff_transition('ready', State), State#state{member_call_id='undefined'}};
handle_sync_event(Event, _From, StateName, State) ->
    lager:debug("unhandled sync event ~p from current state of ~s with state record: ~p", [Event, StateName, State]),
    {'reply', 'ok', StateName, State}.

-spec handle_info(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_info({'$gen_cast', _}, StateName, State) ->
    {'next_state', StateName, State};
handle_info({'EXIT', _Pid, _Reason}, StateName, State) ->
    {'next_state', StateName, State};
handle_info(Info, StateName, State) ->
    lager:debug("unhandled info in state ~s: ~p", [StateName, Info]),
    {'next_state', StateName, State}.

-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(Reason, StateName, _) ->
    lager:debug("terminating in state ~s (~s)", [StateName, Reason]).

-spec code_change(any(), atom(), state(), any()) -> {'ok', atom(), state()}.
code_change(_, StateName, State, _) ->
    {'ok', StateName, State}.

%%
%% gen_fsm states
%%

%%
%% Private functions
%%
-spec agent_queues(ne_binary(), ne_binary()) -> ne_binaries().
agent_queues(AccountId, AgentId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', User} = kz_datamgr:open_cache_doc(AccountDb, AgentId),
    kz_json:get_list_value(<<"queues">>, User, []).

-spec queuelogin(kz_json:object(), ne_binaries()) -> ne_binaries().
queuelogin(JObj, Queues) ->
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),
    case lists:member(QueueId, Queues) of
        'true' -> Queues;
        'false' ->
            lager:debug("joined queue ~s", [QueueId]),
            _ = kz_util:spawn(fun quilt_log:handle_event/1, [JObj]),
            [QueueId | lists:delete(QueueId, Queues)]
    end.

-spec queuelogoff(kz_json:object(), ne_binaries()) -> ne_binaries().
queuelogoff(JObj, Queues) ->
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),
    case lists:member(QueueId, Queues) of
        'true' ->
            lager:debug("left queue ~s", [QueueId]),
            _ = kz_util:spawn(fun quilt_log:handle_event/1, [JObj]),
            lists:delete(QueueId, Queues);
        'false' -> Queues
    end.

-spec agentlogin(kz_json:object(), ne_binaries()) -> ne_binaries().
agentlogin(JObj, Queues) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    AgentQueues = agent_queues(AccountId, AgentId),
    lists:foldl(fun(QueueId, QueuesAcc) ->
                        TempJObj = kz_json:set_values([{<<"Queue-ID">>, QueueId}
                                                      ,{<<"Event-Category">>, <<"agent">>}
                                                      ,{<<"Event-Name">>, <<"login_queue">>}
                                                      ], JObj),
                        queuelogin(TempJObj, QueuesAcc)
                end, Queues, AgentQueues).

-spec agentlogoff(kz_json:object(), ne_binaries()) -> ne_binaries().
agentlogoff(JObj, Queues) ->
    lists:foldl(fun(QueueId, QueuesAcc) ->
                        TempJObj = kz_json:set_values([{<<"Queue-ID">>, QueueId}
                                                      ,{<<"Event-Category">>, <<"agent">>}
                                                      ,{<<"Event-Name">>, <<"logout_queue">>}
                                                      ], JObj),
                        queuelogoff(TempJObj, QueuesAcc)
                end, Queues, Queues).

-spec logoff_transition(atom(), state()) -> atom().
logoff_transition(_, #state{queues=[]}) -> 'loggedout';
logoff_transition(StateName, _) -> StateName.

-spec sleep_until_call_stat_processed(ne_binary()) -> 'ok'.
sleep_until_call_stat_processed(CallId) ->
    Stat = acdc_stats:find_call(CallId),
    case kz_json:get_value(<<"Status">>, Stat) of
        <<"processed">> -> 'ok';
        _ ->
            timer:sleep(1000),
            sleep_until_call_stat_processed(CallId)
    end.
