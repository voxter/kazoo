%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%% Collector of stats
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(acdc_stats).
-behaviour(gen_listener).

%% Public API
-export([call_waiting/6
        ,call_waiting/8
        ,call_abandoned/4
        ,call_marked_callback/4
        ,call_handled/4
        ,call_missed/5
        ,call_processed/5

        ,find_call/1
        ,call_stat_to_json/1
        ,agent_ready/2
        ,agent_logged_in/2
        ,agent_logged_out/2
        ,agent_connecting/3, agent_connecting/5
        ,agent_connected/3, agent_connected/5
        ,agent_wrapup/3
        ,agent_paused/4
        ,agent_outbound/3

        ,agent_statuses/0
        ,manual_cleanup_calls/1
        ,manual_cleanup_statuses/1
        ]).

%% ETS config
-export([call_table_id/0
        ,call_key_pos/0
        ,call_table_opts/0

        ,call_summary_table_id/0
        ,call_summary_key_pos/0
        ,call_summary_table_opts/0

        ,agent_call_table_id/0
        ,agent_call_key_pos/0
        ,agent_call_table_opts/0

        ,init_db/1
        ,archive_call_data/2
        ]).

%% AMQP Callbacks
-export([handle_call_stat/2
        ,handle_call_summary_req/2
        ,handle_call_query/2
        ,handle_agent_calls_req/2
        ,handle_average_wait_time_req/2
        ]).

%% gen_listener functions
-export([start_link/0
        ,init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-define(SERVER, ?MODULE).

%% Public API
-spec call_waiting(api_binary()
                  ,api_binary()
                  ,api_binary()
                  ,api_binary()
                  ,api_binary()
                  ,api_binary()
                  ) -> 'ok' | {'error', any()}.
-spec call_waiting(api_binary()
                  ,api_binary()
                  ,integer()
                  ,api_binary()
                  ,api_binary()
                  ,api_binary()
                  ,api_binary()
                  ,ne_binaries()
                  ) -> 'ok' | {'error', any()}.
call_waiting(AccountId, QueueId, CallId, CallerIdName, CallerIdNumber, CallerPriority) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Caller-ID-Name">>, CallerIdName}
             ,{<<"Caller-ID-Number">>, CallerIdNumber}
             ,{<<"Entered-Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Caller-Priority">>, CallerPriority}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_call_waiting/1).

call_waiting(AccountId, QueueId, Position, CallId, CallerIdName, CallerIdNumber, CallerPriority, RequiredSkills) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Caller-ID-Name">>, CallerIdName}
             ,{<<"Caller-ID-Number">>, CallerIdNumber}
             ,{<<"Entered-Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Entered-Position">>, Position}
             ,{<<"Caller-Priority">>, CallerPriority}
             ,{<<"Required-Skills">>, RequiredSkills}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_call_waiting/1).

-spec call_abandoned(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
call_abandoned(AccountId, QueueId, CallId, Reason) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Abandon-Reason">>, Reason}
             ,{<<"Abandon-Timestamp">>, kz_time:current_tstamp()}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_call_abandoned/1).

-spec call_marked_callback(ne_binary()
                          ,ne_binary()
                          ,ne_binary()
                          ,ne_binary()
                          ) -> 'ok' | {'error', any()}.
call_marked_callback(AccountId, QueueId, CallId, CallerIdName) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Caller-ID-Name">>, CallerIdName}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(
      Prop
     ,fun kapi_acdc_stats:publish_call_marked_callback/1
     ).

-spec call_handled(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
call_handled(AccountId, QueueId, CallId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Handled-Timestamp">>, kz_time:current_tstamp()}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_call_handled/1).

-spec call_missed(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
call_missed(AccountId, QueueId, AgentId, CallId, ErrReason) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Miss-Reason">>, ErrReason}
             ,{<<"Miss-Timestamp">>, kz_time:current_tstamp()}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_call_missed/1).

-spec call_processed(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
call_processed(AccountId, QueueId, AgentId, CallId, Initiator) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Processed-Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Hung-Up-By">>, Initiator}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_call_processed/1).

-spec agent_ready(ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
agent_ready(AcctId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Status">>, <<"ready">>}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_status_ready/1).

-spec agent_logged_in(ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
agent_logged_in(AcctId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Status">>, <<"logged_in">>}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_status_logged_in/1).

-spec agent_logged_out(ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
agent_logged_out(AcctId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Status">>, <<"logged_out">>}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_status_logged_out/1).

-spec agent_connecting(ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
-spec agent_connecting(ne_binary(), ne_binary(), ne_binary(), api_ne_binary(), api_ne_binary()) -> 'ok' | {'error', any()}.
agent_connecting(AcctId, AgentId, CallId) ->
    agent_connecting(AcctId, AgentId, CallId, 'undefined', 'undefined').
agent_connecting(AcctId, AgentId, CallId, CallerIDName, CallerIDNumber) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Status">>, <<"connecting">>}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Caller-ID-Name">>, CallerIDName}
             ,{<<"Caller-ID-Number">>, CallerIDNumber}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_status_connecting/1).

-spec agent_connected(ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
-spec agent_connected(ne_binary(), ne_binary(), ne_binary(), api_ne_binary(), api_ne_binary()) -> 'ok' | {'error', any()}.
agent_connected(AcctId, AgentId, CallId) ->
    agent_connected(AcctId, AgentId, CallId, 'undefined', 'undefined').
agent_connected(AcctId, AgentId, CallId, CallerIDName, CallerIDNumber) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Status">>, <<"connected">>}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Caller-ID-Name">>, CallerIDName}
             ,{<<"Caller-ID-Number">>, CallerIDNumber}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_status_connected/1).

-spec agent_wrapup(ne_binary(), ne_binary(), pos_integer()) -> 'ok' | {'error', any()}.
agent_wrapup(AcctId, AgentId, WaitTime) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Status">>, <<"wrapup">>}
             ,{<<"Wait-Time">>, WaitTime}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_status_wrapup/1).

-spec agent_paused(ne_binary(), ne_binary(), api_pos_integer(), api_binary()) -> 'ok' | {'error', any()}.
agent_paused(AcctId, AgentId, 'undefined', _) ->
    lager:debug("undefined pause time for ~s(~s)", [AgentId, AcctId]);
agent_paused(AcctId, AgentId, PauseTime, Alias) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Status">>, <<"paused">>}
             ,{<<"Pause-Time">>, PauseTime}
             ,{<<"Pause-Alias">>, Alias}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_status_paused/1).

-spec agent_outbound(ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
agent_outbound(AcctId, AgentId, CallId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:current_tstamp()}
             ,{<<"Status">>, <<"outbound">>}
             ,{<<"Call-ID">>, CallId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapps_util:amqp_pool_send(Prop, fun kapi_acdc_stats:publish_status_outbound/1).

-spec agent_statuses() -> ne_binaries().
agent_statuses() ->
    ?STATUS_STATUSES.

-spec manual_cleanup_calls(pos_integer()) -> 'ok'.
manual_cleanup_calls(Window) ->
    {'ok', Srv} = acdc_stats_sup:stats_srv(),

    Past = kz_time:current_tstamp() - Window,
    PastConstraint = {'=<', '$1', Past},

    TypeConstraints = [{'=/=', '$2', {'const', <<"waiting">>}}
                      ,{'=/=', '$2', {'const', <<"handled">>}}
                      ],

    CallMatch = [{#call_stat{entered_timestamp='$1', status='$2', _='_'}
                 ,[PastConstraint | TypeConstraints]
                 ,['$_']
                 }],
    gen_listener:cast(Srv, {'remove_call', CallMatch}),

    case ets:select(?MODULE:call_table_id()
                   ,[{#call_stat{entered_timestamp='$1', status= <<"waiting">>, _='_'}
                     ,[PastConstraint]
                     ,['$_']
                     }
                    ,{#call_stat{entered_timestamp='$1', status= <<"handled">>, _='_'}
                     ,[PastConstraint]
                     ,['$_']
                     }
                    ])
    of
        [] -> 'ok';
        Unfinished -> cleanup_unfinished(Unfinished)
    end.

-spec manual_cleanup_statuses(pos_integer()) -> 'ok'.
manual_cleanup_statuses(Window) ->
    {'ok', Srv} = acdc_stats_sup:stats_srv(),

    Past = kz_time:current_tstamp() - Window,

    StatusMatch = [{#status_stat{timestamp='$1', _='_'}
                   ,[{'=<', '$1', Past}]
                   ,['$_']
                   }],
    gen_listener:cast(Srv, {'remove_status', StatusMatch}).

%% ETS config
-spec call_table_id() -> atom().
-spec call_key_pos() -> pos_integer().
-spec call_table_opts() -> kz_proplist().
call_table_id() -> 'acdc_stats_call'.
call_key_pos() -> #call_stat.id.
call_table_opts() ->
    ['protected', 'named_table'
    ,{'keypos', call_key_pos()}
    ].

-spec call_summary_table_id() -> atom().
-spec call_summary_key_pos() -> pos_integer().
-spec call_summary_table_opts() -> kz_proplist().
call_summary_table_id() -> 'acdc_stats_call_summary'.
call_summary_key_pos() -> #call_summary_stat.id.
call_summary_table_opts() ->
    ['protected', 'named_table'
    ,{'keypos', call_summary_key_pos()}
    ].

-spec agent_call_table_id() -> atom().
-spec agent_call_key_pos() -> pos_integer().
-spec agent_call_table_opts() -> kz_proplist().
agent_call_table_id() -> 'acdc_stats_agent_call'.
agent_call_key_pos() -> #agent_call_stat.id.
agent_call_table_opts() ->
    ['bag', 'protected', 'named_table'
    ,{'keypos', agent_call_key_pos()}
    ].

-define(BINDINGS, [{'self', []}
                  ,{?MODULE, []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_stat'}
                     ,[{<<"acdc_call_stat">>, <<"waiting">>}
                      ,{<<"acdc_call_stat">>, <<"missed">>}
                      ,{<<"acdc_call_stat">>, <<"abandoned">>}
                      ,{<<"acdc_call_stat">>, <<"marked_callback">>}
                      ,{<<"acdc_call_stat">>, <<"handled">>}
                      ,{<<"acdc_call_stat">>, <<"processed">>}
                      ,{<<"acdc_call_stat">>, <<"exited-position">>}
                      ,{<<"acdc_call_stat">>, <<"id-change">>}
                      ,{<<"acdc_call_stat">>, <<"flush">>}
                      ]
                     }
                    ,{{'acdc_agent_stats', 'handle_status_stat'}
                     ,[{<<"acdc_status_stat">>, <<"ready">>}
                      ,{<<"acdc_status_stat">>, <<"logged_in">>}
                      ,{<<"acdc_status_stat">>, <<"logged_out">>}
                      ,{<<"acdc_status_stat">>, <<"pending_logged_out">>}
                      ,{<<"acdc_status_stat">>, <<"connecting">>}
                      ,{<<"acdc_status_stat">>, <<"connected">>}
                      ,{<<"acdc_status_stat">>, <<"wrapup">>}
                      ,{<<"acdc_status_stat">>, <<"paused">>}
                      ,{<<"acdc_status_stat">>, <<"outbound">>}
                      ]
                     }
                    ,{{?MODULE, 'handle_call_query'}
                     ,[{<<"acdc_stat">>, <<"current_calls_req">>}]
                     }
                    ,{{?MODULE, 'handle_call_summary_req'}
                     ,[{<<"acdc_stat">>, <<"call_summary_req">>}]
                     }
                    ,{{?MODULE, 'handle_agent_calls_req'}
                     ,[{<<"acdc_stat">>, <<"agent_calls_req">>}]
                     }
                    ,{{?MODULE, 'handle_average_wait_time_req'}
                     ,[{<<"acdc_stat">>, <<"average_wait_time_req">>}]
                     }
                    ,{{'acdc_agent_stats', 'handle_status_query'}
                     ,[{<<"acdc_stat">>, <<"status_req">>}]
                     }
                    ,{{'acdc_agent_stats', 'handle_agent_cur_status_req'}
                     ,[{<<"acdc_stat">>, <<"agent_cur_status_req">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).

-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ]
                           ,[]
                           ).

-spec handle_call_stat(kz_json:object(), kz_proplist()) -> 'ok'.
handle_call_stat(JObj, Props) ->
    case kz_json:get_value(<<"Event-Name">>, JObj) of
        <<"waiting">> -> handle_waiting_stat(JObj, Props);
        <<"missed">> -> handle_missed_stat(JObj, Props);
        <<"abandoned">> -> handle_abandoned_stat(JObj, Props);
        <<"marked_callback">> -> handle_marked_callback_stat(JObj, Props);
        <<"handled">> -> handle_handled_stat(JObj, Props);
        <<"processed">> -> handle_processed_stat(JObj, Props);
        <<"exited-position">> -> handle_exited_stat(JObj, Props);
        <<"flush">> -> flush_call_stat(JObj, Props);
        _Name ->
            lager:debug("recv unknown call stat type ~s: ~p", [_Name, JObj])
    end.

-spec handle_call_query(kz_json:object(), kz_proplist()) -> 'ok'.
handle_call_query(JObj, _Prop) ->
    'true' = kapi_acdc_stats:current_calls_req_v(JObj),
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),

    case call_build_match_spec(JObj) of
        {'ok', Match} ->
            Limit = acdc_stats_util:get_query_limit(JObj),
            Result = query_calls(Match, Limit),
            Resp = Result ++
                kz_api:default_headers(?APP_NAME, ?APP_VERSION) ++
                [{<<"Query-Time">>, kz_time:current_tstamp()}
                ,{<<"Msg-ID">>, MsgId}
                ],
            kapi_acdc_stats:publish_current_calls_resp(RespQ, Resp);
        {'error', Errors} -> publish_call_query_errors(RespQ, MsgId, Errors)
    end.

-spec handle_call_summary_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_call_summary_req(JObj, _Prop) ->
    'true' = kapi_acdc_stats:call_summary_req_v(JObj),
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
    Limit = acdc_stats_util:get_query_limit(JObj),

    Summary = case call_summary_build_match_spec(JObj) of
                  {'ok', Match} -> query_call_summary(Match, Limit);
                  {'error', _Errors}=E -> E
              end,
    Active = case call_build_match_spec(kz_json:set_value(<<"Status">>, [<<"waiting">>, <<"handled">>], JObj)) of
                 {'ok', Match1} -> query_calls(Match1, Limit);
                 {'error', _Errors1}=E1 -> E1
             end,
    publish_summary_data(RespQ, MsgId, Summary, Active).

-spec handle_agent_calls_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_agent_calls_req(JObj, _Prop) ->
    'true' = kapi_acdc_stats:agent_calls_req_v(JObj),
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
    Limit = acdc_stats_util:get_query_limit(JObj),

    case agent_call_build_match_spec(JObj) of
        {'ok', Match} -> query_agent_calls(RespQ, MsgId, Match, Limit);
        {'error', Errors} -> publish_agent_call_query_errors(RespQ, MsgId, Errors)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Handle requests for the average wait time of a queue based on stats
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_average_wait_time_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_average_wait_time_req(JObj, _Prop) ->
    'true' = kapi_acdc_stats:average_wait_time_req_v(JObj),
    Match = average_wait_time_build_match_spec(JObj),
    query_average_wait_time(Match, JObj).

-spec find_call(ne_binary()) -> api_object().
find_call(CallId) ->
    MS = [{#call_stat{call_id=CallId
                     ,_ = '_'
                     }
          ,[]
          ,['$_']
          }],
    case ets:select(call_table_id(), MS) of
        [] -> 'undefined';
        [Stat] -> call_stat_to_json(Stat)
    end.

-record(state, {archive_ref :: reference()
               ,cleanup_ref :: reference()
               }).
-type state() :: #state{}.

-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(<<"acdc.stats">>),
    kz_datamgr:suppress_change_notice(),
    lager:debug("started new acdc stats collector"),

    {'ok', #state{archive_ref=start_archive_timer()
                 ,cleanup_ref=start_cleanup_timer()
                 }}.

-spec start_archive_timer() -> reference().
start_archive_timer() ->
    erlang:send_after(?ARCHIVE_PERIOD, self(), ?ARCHIVE_MSG).

-spec start_cleanup_timer() -> reference().
start_cleanup_timer() ->
    erlang:send_after(?CLEANUP_PERIOD, self(), ?CLEANUP_MSG).

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Req, _From, State) ->
    {'reply', 'ok', State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'create_call', JObj}, State) ->
    Id = call_stat_id(JObj),
    lager:debug("creating new call stat ~s", [Id]),
    Stat = #call_stat{id = Id
                     ,call_id = kz_json:get_value(<<"Call-ID">>, JObj)
                     ,account_id = kz_json:get_value(<<"Account-ID">>, JObj)
                     ,queue_id = kz_json:get_value(<<"Queue-ID">>, JObj)
                     ,entered_timestamp = kz_json:get_value(<<"Entered-Timestamp">>, JObj, kz_time:current_tstamp())
                     ,abandoned_timestamp = kz_json:get_value(<<"Abandon-Timestamp">>, JObj)
                     ,entered_position = kz_json:get_value(<<"Entered-Position">>, JObj)
                     ,abandoned_reason = kz_json:get_value(<<"Abandon-Reason">>, JObj)
                     ,misses = []
                     ,status = kz_json:get_value(<<"Event-Name">>, JObj)
                     ,caller_id_name = kz_json:get_value(<<"Caller-ID-Name">>, JObj)
                     ,caller_id_number = kz_json:get_value(<<"Caller-ID-Number">>, JObj)
                     ,required_skills = kz_json:get_list_value(<<"Required-Skills">>, JObj, [])
                     },
    ets:insert_new(call_table_id(), Stat),
    {'noreply', State};
handle_cast({'create_status', #status_stat{id=_Id
                                          ,agent_id=AgentId
                                          ,status=_Status
                                          }=Stat}, State) ->
    lager:debug("creating new status stat ~s: ~s", [_Id, _Status]),
    case ets:insert_new(acdc_agent_stats:status_table_id(), Stat) of
        'true' -> 'ok';
        'false' ->
            lager:debug("stat ~s already exists, updating", [_Id]),
            ets:insert(acdc_agent_stats:status_table_id(), Stat)
    end,
    %% Only set the agent's current status if the timestamp of this
    %% stat is newer than the current one
    case ets:lookup(acdc_agent_stats:agent_cur_status_table_id(), AgentId) of
        [] -> ets:insert(acdc_agent_stats:agent_cur_status_table_id(), Stat);
        [OldStat] -> maybe_insert_agent_cur_status(OldStat, Stat)
    end,
    {'noreply', State};
handle_cast({'update_call', Id, Updates}, State) ->
    lager:debug("updating call stat ~s: ~p", [Id, Updates]),
    ets:update_element(call_table_id(), Id, Updates),

    Stat = find_call_stat(Id),
    maybe_add_summary_stat(Stat),
    maybe_add_agent_call_stat(Stat),

    {'noreply', State};
handle_cast({'add_miss', JObj}, State) ->
    Id = call_stat_id(JObj),
    lager:debug("adding miss to stat ~s", [Id]),
    #call_stat{misses=Misses}=Stat = find_call_stat(Id),
    Updates = [{#call_stat.misses, [create_miss(JObj) | Misses]}],
    ets:update_element(call_table_id(), Id, Updates),

    add_agent_call_stat_miss(Stat
                            ,kz_json:get_value(<<"Agent-ID">>, JObj)
                            ,kz_json:get_value(<<"Miss-Timestamp">>, JObj)),

    {'noreply', State};
handle_cast({'flush_call', Id}, State) ->
    lager:debug("flushing call stat ~s", [Id]),

    ets:delete(call_table_id(), Id),
    ets:delete(call_summary_table_id(), Id),
    ets:delete(agent_call_table_id(), Id),

    {'noreply', State};
handle_cast({'remove_call', [{M, P, _}]=MatchSpec}, State) ->
    Stats = ets:select(call_table_id(), MatchSpec),
    lists:foreach(fun(#call_stat{id=Id}) ->
                          ets:delete(call_summary_table_id(), Id),
                          ets:delete(agent_call_table_id(), Id)
                  end, Stats),

    Match = [{M, P, ['true']}],
    N = ets:select_delete(call_table_id(), Match),
    N > 1
        andalso lager:debug("removed calls: ~p", [N]),
    {'noreply', State};

handle_cast({'update_status', Id, Updates}, State) ->
    lager:debug("updating status stat ~s: ~p", [Id, Updates]),
    ets:update_element(acdc_agent_stats:status_table_id(), Id, Updates),
    {'noreply', State};
handle_cast({'remove_status', [{M, P, _}]}, State) ->
    Match = [{M, P, ['true']}],
    N = ets:select_delete(acdc_agent_stats:status_table_id(), Match),
    N > 1
        andalso lager:debug("removed statuses: ~p", [N]),
    {'noreply', State};

handle_cast({'gen_listener',{'created_queue',_Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'ETS-TRANSFER', _TblId, _From, _Data}, State) ->
    lager:debug("ETS control for ~p transferred to me for writing", [_TblId]),
    {'noreply', State};
handle_info(?ARCHIVE_MSG, State) ->
    _ = archive_data(),
    {'noreply', State#state{archive_ref=start_archive_timer()}};
handle_info(?CLEANUP_MSG, State) ->
    _ = cleanup_data(self()),
    {'noreply', State#state{cleanup_ref=start_cleanup_timer()}};
handle_info(_Msg, State) ->
    lager:debug("unhandling message: ~p", [_Msg]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _) ->
    force_archive_data(),
    lager:debug("acdc stats terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

publish_call_query_errors(RespQ, MsgId, Errors) ->
    publish_query_errors(RespQ, MsgId, Errors, fun kapi_acdc_stats:publish_current_calls_err/2).

publish_call_summary_query_errors(RespQ, MsgId, Errors) ->
    publish_query_errors(RespQ, MsgId, Errors, fun kapi_acdc_stats:publish_call_summary_err/2).

publish_agent_call_query_errors(RespQ, MsgId, Errors) ->
    publish_query_errors(RespQ, MsgId, Errors, fun kapi_acdc_stats:publish_agent_calls_err/2).

publish_query_errors(RespQ, MsgId, Errors, PubFun) ->
    API = [{<<"Error-Reason">>, Errors}
          ,{<<"Msg-ID">>, MsgId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("responding with errors to req ~s: ~p", [MsgId, Errors]),
    PubFun(RespQ, API).

call_build_match_spec(JObj) ->
    case kz_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' ->
            {'error', kz_json:from_list([{<<"Account-ID">>, <<"missing but required">>}])};
        AccountId ->
            AccountMatch = {#call_stat{account_id='$1', _='_'}
                           ,[{'=:=', '$1', {'const', AccountId}}]
                           },
            call_build_match_spec(JObj, AccountMatch)
    end.

-spec call_build_match_spec(kz_json:object(), {call_stat(), list()}) ->
                                   {'ok', ets:match_spec()} |
                                   {'error', kz_json:object()}.
call_build_match_spec(JObj, AccountMatch) ->
    case kz_json:foldl(fun call_match_builder_fold/3, AccountMatch, JObj) of
        {'error', _Errs}=Errors -> Errors;
        {CallStat, Constraints} -> {'ok', [{CallStat, Constraints, ['$_']}]}
    end.

call_match_builder_fold(_, _, {'error', _Err}=E) -> E;
call_match_builder_fold(<<"Queue-ID">>, QueueId, {CallStat, Contstraints}) ->
    {CallStat#call_stat{queue_id='$2'}
    ,[{'=:=', '$2', {'const', QueueId}} | Contstraints]
    };
call_match_builder_fold(<<"Agent-ID">>, AgentId, {CallStat, Contstraints}) ->
    {CallStat#call_stat{agent_id='$3'}
    ,[{'=:=', '$3', {'const', AgentId}} | Contstraints]
    };
call_match_builder_fold(<<"Status">>, Statuses, {CallStat, Constraints}) when is_list(Statuses) ->
    CallStat1 = CallStat#call_stat{status='$4'},
    Constraints1 = lists:foldl(fun(_Status, {'error', _Err}=E) ->
                                       E;
                                  (Status, OrdConstraints) ->
                                       case is_valid_call_status(Status) of
                                           {'true', Normalized} ->
                                               erlang:append_element(OrdConstraints, {'=:=', '$4', {'const', Normalized}});
                                           'false' ->
                                               {'error', kz_json:from_list([{<<"Status">>, <<"unknown status supplied">>}])}
                                       end
                               end, {'orelse'}, Statuses),
    case Constraints1 of
        {'error', _Err}=E -> E;
        _ -> {CallStat1, [Constraints1 | Constraints]}
    end;
call_match_builder_fold(<<"Status">>, Status, {CallStat, Contstraints}) ->
    case is_valid_call_status(Status) of
        {'true', Normalized} ->
            {CallStat#call_stat{status='$4'}
            ,[{'=:=', '$4', {'const', Normalized}} | Contstraints]
            };
        'false' ->
            {'error', kz_json:from_list([{<<"Status">>, <<"unknown status supplied">>}])}
    end;
call_match_builder_fold(<<"Start-Range">>, Start, {CallStat, Contstraints}) ->
    Now = kz_time:current_tstamp(),
    Past = Now - (?CLEANUP_WINDOW + 1),

    try kz_term:to_integer(Start) of
        N when N < Past ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is too far in the past">>}
                                        ,{<<"Window-Size">>, ?CLEANUP_WINDOW + 1}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ,{<<"Past-Timestamp">>, Past}
                                        ])};
        N when N > Now ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is in the future">>}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {CallStat#call_stat{entered_timestamp='$5'}
            ,[{'>=', '$5', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is not an integer">>}])}
    end;
call_match_builder_fold(<<"End-Range">>, End, {CallStat, Contstraints}) ->
    Now = kz_time:current_tstamp(),
    Past = Now - (?CLEANUP_WINDOW + 1),

    try kz_term:to_integer(End) of
        N when N < Past ->
            {'error', kz_json:from_list([{<<"End-Range">>, <<"supplied value is too far in the past">>}
                                        ,{<<"Window-Size">>, ?CLEANUP_WINDOW + 1}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N when N > Now ->
            {'error', kz_json:from_list([{<<"End-Range">>, <<"supplied value is in the future">>}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {CallStat#call_stat{entered_timestamp='$5'}
            ,[{'=<', '$5', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', kz_json:from_list([{<<"End-Range">>, <<"supplied value is not an integer">>}])}
    end;
call_match_builder_fold(_, _, Acc) -> Acc.

call_summary_build_match_spec(JObj) ->
    case kz_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' ->
            {'error', kz_json:from_list([{<<"Account-ID">>, <<"missing but required">>}])};
        AccountId ->
            AccountMatch = {#call_summary_stat{account_id='$1', _='_'}
                           ,[{'=:=', '$1', {'const', AccountId}}]
                           },
            call_summary_build_match_spec(JObj, AccountMatch)
    end.

-spec call_summary_build_match_spec(kz_json:object(), {call_summary_stat(), list()}) ->
                                           {'ok', ets:match_spec()} |
                                           {'error', kz_json:object()}.
call_summary_build_match_spec(JObj, AccountMatch) ->
    case kz_json:foldl(fun call_summary_match_builder_fold/3, AccountMatch, JObj) of
        {'error', _Errs}=Errors -> Errors;
        {Stat, Constraints} -> {'ok', [{Stat, Constraints, ['$_']}]}
    end.

call_summary_match_builder_fold(_, _, {'error', _Err}=E) -> E;
call_summary_match_builder_fold(<<"Queue-ID">>, QueueId, {CallStat, Contstraints}) ->
    {CallStat#call_summary_stat{queue_id='$2'}
    ,[{'=:=', '$2', {'const', QueueId}} | Contstraints]
    };
call_summary_match_builder_fold(<<"Status">>, Status, {CallStat, Contstraints}) ->
    case is_valid_call_status(Status) of
        {'true', Normalized} ->
            {CallStat#call_summary_stat{status='$3'}
            ,[{'=:=', '$3', {'const', Normalized}} | Contstraints]
            };
        'false' ->
            {'error', kz_json:from_list([{<<"Status">>, <<"unknown status supplied">>}])}
    end;
call_summary_match_builder_fold(_, _, Acc) -> Acc.

agent_call_build_match_spec(JObj) ->
    case kz_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' ->
            {'error', kz_json:from_list([{<<"Account-ID">>, <<"missing but required">>}])};
        AccountId ->
            AccountMatch = {#agent_call_stat{account_id='$1', _='_'}
                           ,[{'=:=', '$1', {'const', AccountId}}]
                           },
            agent_call_build_match_spec(JObj, AccountMatch)
    end.

-spec agent_call_build_match_spec(kz_json:object(), {agent_call_stat(), list()}) ->
                                         {'ok', ets:match_spec()} |
                                         {'error', kz_json:object()}.
agent_call_build_match_spec(JObj, AccountMatch) ->
    case kz_json:foldl(fun agent_call_match_builder_fold/3, AccountMatch, JObj) of
        {'error', _Errs}=Errors -> Errors;
        {Stat, Constraints} -> {'ok', [{Stat, Constraints, ['$_']}]}
    end.

agent_call_match_builder_fold(_, _, {'error', _Err}=E) -> E;
agent_call_match_builder_fold(_, _, Acc) -> Acc.

-spec average_wait_time_build_match_spec(kz_json:object()) -> ets:match_spec().
average_wait_time_build_match_spec(JObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"Account-ID">>, JObj),
    QueueId = kz_json:get_ne_binary_value(<<"Queue-ID">>, JObj),

    Match = [{#call_stat{account_id=AccountId
                        ,queue_id=QueueId
                        ,entered_timestamp='$1'
                        ,abandoned_timestamp='$2'
                        ,handled_timestamp='$3'
                        ,status='$4'
                        ,_='_'
                        }
             ,[{'orelse'
               ,{'=:=', '$4', {'const', <<"handled">>}}
               ,{'=:=', '$4', {'const', <<"processed">>}}
               }]
             ,[['$1', '$2', '$3']]
             }],

    Window = kz_json:get_integer_value(<<"Window">>, JObj),

    average_wait_time_build_match_spec(Match, Window).

-spec average_wait_time_build_match_spec(ets:match_spec(), api_integer()) ->
                                                ets:match_spec().
average_wait_time_build_match_spec(Match, 'undefined') ->
    Match;
average_wait_time_build_match_spec([{CallStat, Conditions, Results}], Window) ->
    Start = kz_time:current_tstamp() - Window,
    [{CallStat, [{'>=', '$1', {'const', Start}} | Conditions], Results}].

is_valid_call_status(S) ->
    Status = kz_term:to_lower_binary(S),
    case lists:member(Status, ?VALID_STATUSES) of
        'true' -> {'true', Status};
        'false' -> 'false'
    end.

-spec query_calls(ets:match_spec(), pos_integer() | 'no_limit') -> kz_proplist().
query_calls(Match, _Limit) ->
    case ets:select(call_table_id(), Match) of
        [] ->
            lager:debug("no stats found, sorry"),
            [];
        Stats ->
            Dict = dict:from_list([{<<"waiting">>, []}
                                  ,{<<"handled">>, []}
                                  ,{<<"abandoned">>, []}
                                  ,{<<"processed">>, []}
                                  ,{<<"entered_position">>, []}
                                  ,{<<"exited_position">>, []}
                                  ]),

            QueryResult = lists:foldl(fun query_call_fold/2, Dict, Stats),
            [{<<"Waiting">>, dict:fetch(<<"waiting">>, QueryResult)}
            ,{<<"Handled">>, dict:fetch(<<"handled">>, QueryResult)}
            ,{<<"Abandoned">>, dict:fetch(<<"abandoned">>, QueryResult)}
            ,{<<"Processed">>, dict:fetch(<<"processed">>, QueryResult)}
            ,{<<"Entered-Position">>, dict:fetch(<<"entered_position">>, QueryResult)}
            ,{<<"Exited-Position">>, dict:fetch(<<"exited_position">>, QueryResult)}
            ]
    end.

-spec query_call_summary(ets:match_spec(), pos_integer() | 'no_limit') -> kz_proplist().
query_call_summary(Match, _Limit) ->
    case ets:select(call_summary_table_id(), Match) of
        [] ->
            lager:debug("no stats found, sorry"),
            [];
        Stats ->
            QueryResult = lists:foldl(fun query_call_summary_fold/2, [], Stats),
            JsonResult = lists:foldl(fun({QueueId, {TotalCalls, AbandonedCalls, TotalWaitTime}}, JObj) ->
                                             QueueJObj = kz_json:set_values([{<<"total_calls">>, TotalCalls}
                                                                            ,{<<"abandoned_calls">>, AbandonedCalls}
                                                                            ,{<<"total_wait_time">>, TotalWaitTime}
                                                                            ]
                                                                           ,kz_json:new()),
                                             kz_json:set_value(QueueId, QueueJObj, JObj)
                                     end
                                    ,kz_json:new()
                                    ,QueryResult),
            [{<<"Data">>, JsonResult}]
    end.

-spec query_call_summary_fold(call_summary_stat(), kz_proplist()) -> kz_proplist().
query_call_summary_fold(#call_summary_stat{queue_id=QueueId
                                          ,status=Status
                                          ,wait_time=WaitTime
                                          }, Props) ->
    {TotalCalls, AbandonedCalls, TotalWaitTime} = props:get_value(QueueId, Props, {0, 0, 0}),
    {AbandonedCalls1, TotalWaitTime1} = case Status of
                                            <<"processed">> -> {AbandonedCalls, TotalWaitTime + WaitTime};
                                            <<"abandoned">> -> {AbandonedCalls + 1, TotalWaitTime}
                                        end,
    props:set_value(QueueId, {TotalCalls+1, AbandonedCalls1, TotalWaitTime1}, Props).

-spec query_agent_calls(ne_binary(), ne_binary(), ets:match_spec(), pos_integer() | 'no_limit') -> 'ok'.
query_agent_calls(RespQ, MsgId, Match, _Limit) ->
    case ets:select(agent_call_table_id(), Match) of
        [] ->
            lager:debug("no stats found, sorry ~s", [RespQ]),
            Resp = [{<<"Query-Time">>, kz_time:current_tstamp()}
                   ,{<<"Msg-ID">>, MsgId}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_acdc_stats:publish_agent_calls_resp(RespQ, Resp);
        Stats ->
            QueryResult = lists:foldl(fun query_agent_calls_fold/2, kz_json:new(), Stats),
            Resp = kz_json:to_proplist(kz_json:set_value(<<"Data">>, QueryResult, kz_json:new())) ++
                kz_api:default_headers(?APP_NAME, ?APP_VERSION) ++
                [{<<"Query-Time">>, kz_time:current_tstamp()}
                ,{<<"Msg-ID">>, MsgId}
                ],
            kapi_acdc_stats:publish_agent_calls_resp(RespQ, Resp)
    end.

-spec query_agent_calls_fold(agent_call_stat(), kz_json:object()) -> kz_json:object().
query_agent_calls_fold(#agent_call_stat{agent_id=AgentId}=Stat, JObj) ->
    AgentJObj = kz_json:get_value(AgentId, JObj, []),
    kz_json:set_value(AgentId, increment_agent_calls(Stat, AgentJObj), JObj).

-spec increment_agent_calls(agent_call_stat(), kz_json:object()) -> kz_json:object().
increment_agent_calls(#agent_call_stat{queue_id=QueueId
                                      ,status=Status
                                      }, AgentJObj) ->
    case Status of
        <<"handled">> -> increment_agent_calls(QueueId, AgentJObj, <<"answered_calls">>);
        <<"missed">> -> increment_agent_calls(QueueId, AgentJObj, <<"missed_calls">>);
        _ -> AgentJObj
    end.

-spec increment_agent_calls(ne_binary(), kz_json:object(), ne_binary()) -> kz_json:object().
increment_agent_calls(QueueId, AgentJObj, Key) ->
    Count = kz_json:get_integer_value([QueueId, Key], AgentJObj, 0) + 1,
    kz_json:set_value([QueueId, Key], Count, AgentJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculate and reply with the average wait time on a queue
%%
%% @end
%%--------------------------------------------------------------------
-spec query_average_wait_time(ets:match_spec(), kz_json:object()) -> 'ok'.
query_average_wait_time(Match, JObj) ->
    AverageWaitTime = average_wait_time_fold(ets:select(call_table_id(), Match)),

    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    Resp = [{<<"Average-Wait-Time">>, AverageWaitTime}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_stats:publish_average_wait_time_resp(RespQ, Resp).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculate the average wait time given a list of finished call stat
%% timestamps
%%
%% @end
%%--------------------------------------------------------------------
-spec average_wait_time_fold(list()) -> non_neg_integer().
average_wait_time_fold(Stats) ->
    {CallCount, TotalWaitTime} = lists:foldl(fun average_wait_time_fold/2
                                            ,{0, 0}
                                            ,Stats
                                            ),
    case CallCount of
        0 -> 0;
        _ -> TotalWaitTime div CallCount
    end.

-spec average_wait_time_fold([non_neg_integer()], {non_neg_integer(), non_neg_integer()}) ->
                                    {non_neg_integer(), non_neg_integer()}.
average_wait_time_fold([EnteredT, AbandonedT, HandledT], {CallCount, TotalWaitTime}) ->
    WaitTime = wait_time(EnteredT, AbandonedT, HandledT),
    {CallCount + 1, TotalWaitTime + WaitTime}.

-spec archive_data() -> 'ok'.
archive_data() ->
    Self = self(),
    _ = kz_util:spawn(fun archive_call_data/2, [Self, 'false']),
    _ = kz_util:spawn(fun acdc_agent_stats:archive_status_data/2, [Self, 'false']),
    'ok'.

force_archive_data() ->
    Self = self(),
    _ = kz_util:spawn(fun archive_call_data/2, [Self, 'true']),
    _ = kz_util:spawn(fun acdc_agent_stats:archive_status_data/2, [Self, 'true']),
    'ok'.

cleanup_data(Srv) ->
    Past = kz_time:current_tstamp() - ?CLEANUP_WINDOW,
    PastConstraint = {'=<', '$1', Past},

    TypeConstraints = [{'=/=', '$2', {'const', <<"waiting">>}}
                      ,{'=/=', '$2', {'const', <<"handled">>}}
                      ],

    CallMatch = [{#call_stat{entered_timestamp='$1', status='$2', _='_'}
                 ,[PastConstraint | TypeConstraints]
                 ,['$_']
                 }],
    gen_listener:cast(Srv, {'remove_call', CallMatch}),

    StatusMatch = [{#status_stat{timestamp='$1', _='_'}
                   ,[{'=<', '$1', Past}]
                   ,['$_']
                   }],
    gen_listener:cast(Srv, {'remove_status', StatusMatch}),

    case ets:select(call_table_id()
                   ,[{#call_stat{entered_timestamp='$1', status= <<"waiting">>, _='_'}
                     ,[PastConstraint]
                     ,['$_']
                     }
                    ,{#call_stat{entered_timestamp='$1', status= <<"handled">>, _='_'}
                     ,[PastConstraint]
                     ,['$_']
                     }
                    ])
    of
        [] -> 'ok';
        Unfinished -> cleanup_unfinished(Unfinished)
    end.

cleanup_unfinished(Unfinished) ->
    lager:debug("unfinished stats: ~p", [Unfinished]).

-spec archive_call_data(pid(), boolean()) -> 'ok'.
archive_call_data(Srv, 'true') ->
    kz_util:put_callid(<<"acdc_stats.force_call_archiver">>),

    Match = [{#call_stat{status='$1'
                        ,is_archived='$2'
                        ,_='_'
                        }
             ,[{'=/=', '$1', {'const', <<"waiting">>}}
              ,{'=/=', '$1', {'const', <<"handled">>}}
              ,{'=:=', '$2', 'false'}
              ]
             ,['$_']
             }],
    maybe_archive_call_data(Srv, Match);
archive_call_data(Srv, 'false') ->
    kz_util:put_callid(<<"acdc_stats.call_archiver">>),

    Past = kz_time:current_tstamp() - ?ARCHIVE_WINDOW,
    Match = [{#call_stat{entered_timestamp='$1'
                        ,status='$2'
                        ,is_archived='$3'
                        , _='_'
                        }
             ,[{'=<', '$1', Past}
              ,{'=/=', '$2', {'const', <<"waiting">>}}
              ,{'=/=', '$2', {'const', <<"handled">>}}
              ,{'=:=', '$3', 'false'}
              ]
             ,['$_']
             }],
    maybe_archive_call_data(Srv, Match).

maybe_archive_call_data(Srv, Match) ->
    case ets:select(call_table_id(), Match) of
        [] -> 'ok';
        Stats ->
            kz_datamgr:suppress_change_notice(),
            ToSave = lists:foldl(fun archive_call_fold/2, dict:new(), Stats),
            _ = [kz_datamgr:save_docs(acdc_stats_util:db_name(Account), Docs)
                 || {Account, Docs} <- dict:to_list(ToSave)
                ],
            _ = [gen_listener:cast(Srv, {'update_call', Id, [{#call_stat.is_archived, 'true'}]})
                 || #call_stat{id=Id} <- Stats
                ],
            'ok'
    end.

-spec query_call_fold(call_stat(), dict:dict()) -> dict:dict().
query_call_fold(#call_stat{status=Status}=Stat, Acc) ->
    Doc = call_stat_to_doc(Stat),
    dict:update(Status, fun(L) -> [Doc | L] end, [Doc], Acc).

-spec archive_call_fold(call_stat(), dict:dict()) -> dict:dict().
archive_call_fold(#call_stat{account_id=AccountId}=Stat, Acc) ->
    Doc = call_stat_to_doc(Stat),
    dict:update(AccountId, fun(L) -> [Doc | L] end, [Doc], Acc).

-spec call_stat_to_doc(call_stat()) -> kz_json:object().
call_stat_to_doc(#call_stat{id=Id
                           ,call_id=CallId
                           ,account_id=AccountId
                           ,queue_id=QueueId
                           ,agent_id=AgentId
                           ,entered_timestamp=EnteredT
                           ,abandoned_timestamp=AbandonedT
                           ,handled_timestamp=HandledT
                           ,processed_timestamp=ProcessedT
                           ,hung_up_by=HungUpBy
                           ,entered_position=EnteredPos
                           ,exited_position=ExitedPos
                           ,abandoned_reason=AbandonedR
                           ,is_callback=IsCallback
                           ,misses=Misses
                           ,status=Status
                           ,caller_id_name=CallerIdName
                           ,caller_id_number=CallerIdNumber
                           ,caller_priority=CallerPriority
                           ,required_skills=RequiredSkills
                           }) ->
    kz_doc:update_pvt_parameters(kz_json:from_list(
                                   [{<<"_id">>, Id}
                                   ,{<<"call_id">>, CallId}
                                   ,{<<"queue_id">>, QueueId}
                                   ,{<<"agent_id">>, AgentId}
                                   ,{<<"entered_timestamp">>, EnteredT}
                                   ,{<<"abandoned_timestamp">>, AbandonedT}
                                   ,{<<"handled_timestamp">>, HandledT}
                                   ,{<<"processed_timestamp">>, ProcessedT}
                                   ,{<<"hung_up_by">>, HungUpBy}
                                   ,{<<"entered_position">>, EnteredPos}
                                   ,{<<"exited_position">>, ExitedPos}
                                   ,{<<"abandoned_reason">>, AbandonedR}
                                   ,{<<"is_callback">>, IsCallback}
                                   ,{<<"misses">>, misses_to_docs(Misses)}
                                   ,{<<"status">>, Status}
                                   ,{<<"caller_id_name">>, CallerIdName}
                                   ,{<<"caller_id_number">>, CallerIdNumber}
                                   ,{<<"caller_priority">>, CallerPriority}
                                   ,{<<"required_skills">>, RequiredSkills}
                                   ,{<<"wait_time">>, wait_time(EnteredT, AbandonedT, HandledT)}
                                   ,{<<"talk_time">>, talk_time(HandledT, ProcessedT)}
                                   ])
                                ,acdc_stats_util:db_name(AccountId)
                                ,[{'account_id', AccountId}
                                 ,{'type', <<"call_stat">>}
                                 ]
                                ).

-spec call_stat_to_json(call_stat()) -> kz_json:object().
call_stat_to_json(#call_stat{id=Id
                            ,call_id=CallId
                            ,account_id=AccountId
                            ,queue_id=QueueId
                            ,agent_id=AgentId
                            ,entered_timestamp=EnteredT
                            ,abandoned_timestamp=AbandonedT
                            ,handled_timestamp=HandledT
                            ,processed_timestamp=ProcessedT
                            ,hung_up_by=HungUpBy
                            ,entered_position=EnteredPos
                            ,exited_position=ExitedPos
                            ,abandoned_reason=AbandonedR
                            ,is_callback=IsCallback
                            ,misses=Misses
                            ,status=Status
                            ,caller_id_name=CallerIdName
                            ,caller_id_number=CallerIdNumber
                            }) ->
    kz_json:from_list(
      [{<<"Id">>, Id}
      ,{<<"Call-ID">>, CallId}
      ,{<<"Queue-ID">>, QueueId}
      ,{<<"Agent-ID">>, AgentId}
      ,{<<"Account-ID">>, AccountId}
      ,{<<"Entered-Timestamp">>, EnteredT}
      ,{<<"Abandoned-Timestamp">>, AbandonedT}
      ,{<<"Handled-Timestamp">>, HandledT}
      ,{<<"Processed-Timestamp">>, ProcessedT}
      ,{<<"Hung-Up-By">>, HungUpBy}
      ,{<<"Entered-Position">>, EnteredPos}
      ,{<<"Exited-Position">>, ExitedPos}
      ,{<<"Abandoned-Reason">>, AbandonedR}
      ,{<<"Is-Callback">>, IsCallback}
      ,{<<"Misses">>, misses_to_docs(Misses)}
      ,{<<"Status">>, Status}
      ,{<<"Caller-ID-Name">>, CallerIdName}
      ,{<<"Caller-ID-Number">>, CallerIdNumber}
      ,{<<"Wait-Time">>, wait_time(EnteredT, AbandonedT, HandledT)}
      ,{<<"Talk-Time">>, talk_time(HandledT, ProcessedT)}
      ]).

wait_time(E, _, H) when is_integer(E), is_integer(H) -> H - E;
wait_time(E, A, _) when is_integer(E), is_integer(A) -> A - E;
wait_time(_, _, _) -> 'undefined'.

talk_time(H, P) when is_integer(H), is_integer(P) -> P - H;
talk_time(_, _) -> 'undefined'.

-spec misses_to_docs(agent_misses()) -> kz_json:objects().
-spec miss_to_doc(agent_miss()) -> kz_json:object().
misses_to_docs(Misses) -> [miss_to_doc(Miss) || Miss <- Misses].
miss_to_doc(#agent_miss{agent_id=AgentId
                       ,miss_reason=Reason
                       ,miss_timestamp=T
                       }) ->
    kz_json:from_list([{<<"agent_id">>, AgentId}
                      ,{<<"reason">>, Reason}
                      ,{<<"timestamp">>, T}
                      ]).

-spec publish_summary_data(ne_binary()
                          ,ne_binary()
                          ,kz_proplist() | {'error', _}
                          ,kz_proplist() | {'error', _}) -> 'ok'.
publish_summary_data(RespQ, MsgId, {'error', Errors}, _) ->
    publish_call_summary_query_errors(RespQ, MsgId, Errors);
publish_summary_data(RespQ, MsgId, _, {'error', Errors}) ->
    publish_call_query_errors(RespQ, MsgId, Errors);
publish_summary_data(RespQ, MsgId, Summary, Active) ->
    Resp = Summary ++
        remove_missed(Active) ++
        kz_api:default_headers(?APP_NAME, ?APP_VERSION) ++
        [{<<"Query-Time">>, kz_time:current_tstamp()}
        ,{<<"Msg-ID">>, MsgId}
        ],
    kapi_acdc_stats:publish_call_summary_resp(RespQ, Resp).

-spec remove_missed(kz_proplist()) -> kz_proplist().
remove_missed(Active) ->
    [{<<"Waiting">>, remove_misses_fold(props:get_value(<<"Waiting">>, Active, []))}
    ,{<<"Handled">>, remove_misses_fold(props:get_value(<<"Handled">>, Active, []))}
    ].

-spec remove_misses_fold(kz_json:objects()) -> kz_json:objects().
-spec remove_misses_fold(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
remove_misses_fold(JObjs) ->
    remove_misses_fold(JObjs, []).

remove_misses_fold([], Acc) ->
    Acc;
remove_misses_fold([JObj|JObjs], Acc) ->
    remove_misses_fold(JObjs, [kz_json:delete_key(<<"misses">>, JObj) | Acc]).

-spec init_db(ne_binary()) -> 'ok'.
init_db(AccountId) ->
    DbName = acdc_stats_util:db_name(AccountId),
    maybe_created_db(DbName, kazoo_modb:maybe_create(DbName)).

-spec maybe_created_db(ne_binary(), boolean()) -> 'ok'.
maybe_created_db(DbName, 'false') ->
    case kz_datamgr:db_exists(DbName) of
        'true' ->
            lager:debug("database ~s already created, refreshing view", [DbName]),
            kz_datamgr:revise_views_from_folder(DbName, 'acdc');
        'false' ->
            lager:debug("modb ~s was not created", [DbName])
    end;
maybe_created_db(DbName, 'true') ->
    lager:debug("created db ~s, adding views", [DbName]),
    kz_datamgr:revise_views_from_folder(DbName, 'acdc').

-spec call_stat_id(kz_json:object()) -> ne_binary().
-spec call_stat_id(ne_binary(), ne_binary()) -> ne_binary().
call_stat_id(JObj) ->
    call_stat_id(kz_json:get_value(<<"Call-ID">>, JObj)
                ,kz_json:get_value(<<"Queue-ID">>, JObj)
                ).
call_stat_id(CallId, QueueId) -> <<CallId/binary, "::", QueueId/binary>>.

-spec handle_waiting_stat(kz_json:object(), kz_proplist()) -> 'ok'.
handle_waiting_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_waiting_v(JObj),

    Id = call_stat_id(JObj),
    case find_call_stat(Id) of
        'undefined' -> gen_listener:cast(props:get_value('server', Props), {'create_call', JObj});
        _Stat ->
            Updates = props:filter_undefined(
                        [{#call_stat.caller_id_name, kz_json:get_value(<<"Caller-ID-Name">>, JObj)}
                        ,{#call_stat.caller_id_number, kz_json:get_value(<<"Caller-ID-Number">>, JObj)}
                        ,{#call_stat.entered_timestamp, kz_json:get_value(<<"Entered-Timestamp">>, JObj)}
                        ,{#call_stat.entered_position, kz_json:get_value(<<"Entered-Position">>, JObj)}
                        ,{#call_stat.required_skills, kz_json:get_list_value(<<"Required-Skills">>, JObj, [])}
                        ]),
            update_call_stat(Id, Updates, Props)
    end.

-spec handle_missed_stat(kz_json:object(), kz_proplist()) -> 'ok'.
handle_missed_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_missed_v(JObj),

    Id = call_stat_id(JObj),
    case find_call_stat(Id) of
        'undefined' -> lager:debug("can't update stat ~s with missed data, missing", [Id]);
        _ -> gen_listener:cast(props:get_value('server', Props), {'add_miss', JObj})
    end.

-spec create_miss(kz_json:object()) -> agent_miss().
create_miss(JObj) ->
    #agent_miss{agent_id = kz_json:get_value(<<"Agent-ID">>, JObj)
               ,miss_reason = kz_json:get_value(<<"Miss-Reason">>, JObj)
               ,miss_timestamp = kz_json:get_value(<<"Miss-Timestamp">>, JObj)
               }.

-spec handle_abandoned_stat(kz_json:object(), kz_proplist()) -> 'ok'.
handle_abandoned_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_abandoned_v(JObj),

    Id = call_stat_id(JObj),
    %% If caller leaves quickly, the waiting entry might not have arrived yet
    case find_call_stat(Id) of
        'undefined' -> gen_listener:cast(props:get_value('server', Props), {'create_call', JObj});
        _Stat ->
            Updates = props:filter_undefined(
                        [{#call_stat.abandoned_reason, kz_json:get_value(<<"Abandon-Reason">>, JObj)}
                        ,{#call_stat.abandoned_timestamp, kz_json:get_value(<<"Abandon-Timestamp">>, JObj)}
                        ,{#call_stat.status, <<"abandoned">>}
                        ]),
            update_call_stat(Id, Updates, Props)
    end.

-spec handle_marked_callback_stat(kz_json:object(), kz_proplist()) -> 'ok'.
handle_marked_callback_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_marked_callback_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.is_callback, 'true'}
                ,{#call_stat.caller_id_name, kz_json:get_value(<<"Caller-ID-Name">>, JObj)}
                ]),
    update_call_stat(Id, Updates, Props).

-spec handle_handled_stat(kz_json:object(), kz_proplist()) -> 'ok'.
handle_handled_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_handled_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.agent_id, kz_json:get_value(<<"Agent-ID">>, JObj)}
                ,{#call_stat.handled_timestamp, kz_json:get_value(<<"Handled-Timestamp">>, JObj)}
                ,{#call_stat.status, <<"handled">>}
                ]),
    update_call_stat(Id, Updates, Props).

-spec handle_processed_stat(kz_json:object(), kz_proplist()) -> 'ok'.
handle_processed_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_processed_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.agent_id, kz_json:get_value(<<"Agent-ID">>, JObj)}
                ,{#call_stat.processed_timestamp, kz_json:get_value(<<"Processed-Timestamp">>, JObj)}
                ,{#call_stat.hung_up_by, kz_json:get_value(<<"Hung-Up-By">>, JObj)}
                ,{#call_stat.status, <<"processed">>}
                ]),
    update_call_stat(Id, Updates, Props).

-spec handle_exited_stat(kz_json:object(), kz_proplist()) -> 'ok'.
handle_exited_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_exited_position_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined([{#call_stat.exited_position, kz_json:get_value(<<"Exited-Position">>, JObj)}]),
    update_call_stat(Id, Updates, Props).

-spec flush_call_stat(kz_json:object(), kz_proplist()) -> 'ok'.
flush_call_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_flush_v(JObj),

    Id = call_stat_id(JObj),

    lager:debug("flushing ~s: ~p", [Id, JObj]),

    gen_listener:cast(props:get_value('server', Props)
                     ,{'flush_call', Id}
                     ).

-spec find_call_stat(ne_binary()) -> 'undefined' | call_stat().
find_call_stat(Id) ->
    case ets:lookup(call_table_id(), Id) of
        [] -> 'undefined';
        [Stat] -> Stat
    end.

-type updates() :: [{pos_integer(), any()}].
-spec update_call_stat(ne_binary(), updates(), kz_proplist()) -> 'ok'.
update_call_stat(Id, Updates, Props) ->
    gen_listener:cast(props:get_value('server', Props), {'update_call', Id, Updates}).

-spec maybe_add_summary_stat(call_stat()) -> boolean().
maybe_add_summary_stat(#call_stat{status=Status}=Stat)
  when Status =:= <<"processed">>
       orelse Status =:= <<"abandoned">> ->
    ets:insert(call_summary_table_id(), call_stat_to_summary_stat(Stat));
maybe_add_summary_stat(_) -> 'false'.

-spec maybe_add_agent_call_stat(call_stat()) -> boolean().
maybe_add_agent_call_stat(#call_stat{status= <<"handled">>}=Stat) ->
    ets:insert(agent_call_table_id(), call_stat_to_agent_call_stat(Stat));
maybe_add_agent_call_stat(_) -> 'false'.

-spec add_agent_call_stat_miss(call_stat(), ne_binary(), non_neg_integer()) -> 'true'.
add_agent_call_stat_miss(Stat, AgentId, Timestamp) ->
    AgentStat = call_stat_to_agent_call_stat(Stat),
    AgentStat1 = AgentStat#agent_call_stat{agent_id=AgentId
                                          ,status= <<"missed">>
                                          ,timestamp=Timestamp
                                          },
    ets:insert(agent_call_table_id(), AgentStat1).

-spec maybe_insert_agent_cur_status(status_stat(), status_stat()) -> boolean().
maybe_insert_agent_cur_status(#status_stat{status= <<"logged_out">>, timestamp=Timestamp}
                             ,#status_stat{status= <<"pending_logged_out">>, timestamp=Timestamp1}=Stat
                             ) ->
    %% Note the timestamp must be GREATER if new stat is pending (fix logout bug)
    case Timestamp1 > Timestamp of
        'true' -> ets:insert(acdc_agent_stats:agent_cur_status_table_id(), Stat);
        'false' -> 'false'
    end;
maybe_insert_agent_cur_status(#status_stat{timestamp=Timestamp}
                             ,#status_stat{timestamp=Timestamp1}=Stat
                             ) ->
    case Timestamp1 >= Timestamp of
        'true' -> ets:insert(acdc_agent_stats:agent_cur_status_table_id(), Stat);
        'false' -> 'false'
    end.

-spec call_stat_to_summary_stat(call_stat()) -> call_summary_stat().
call_stat_to_summary_stat(#call_stat{id=Id
                                    ,call_id=CallId
                                    ,account_id=AccountId
                                    ,queue_id=QueueId
                                    ,entered_timestamp=EnteredTimestamp
                                    ,abandoned_timestamp=AbandonedTimestamp
                                    ,handled_timestamp=HandledTimestamp
                                    ,status=Status
                                    }) ->
    #call_summary_stat{id=Id
                      ,account_id=AccountId
                      ,queue_id=QueueId
                      ,call_id=CallId
                      ,status=Status
                      ,wait_time=wait_time(EnteredTimestamp, AbandonedTimestamp, HandledTimestamp)
                      }.

-spec call_stat_to_agent_call_stat(call_stat()) -> agent_call_stat().
call_stat_to_agent_call_stat(#call_stat{id=Id
                                       ,call_id=CallId
                                       ,account_id=AccountId
                                       ,queue_id=QueueId
                                       ,agent_id=AgentId
                                       ,status=Status
                                       ,handled_timestamp=HandledTimestamp
                                       }) ->
    #agent_call_stat{id=Id
                    ,account_id=AccountId
                    ,queue_id=QueueId
                    ,agent_id=AgentId
                    ,call_id=CallId
                    ,status=Status
                    ,timestamp=HandledTimestamp
                    }.
