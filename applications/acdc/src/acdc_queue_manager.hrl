-ifndef(ACDC_QUEUE_MANAGER_HRL).

%% rr :: Round Robin
%% mi :: Most Idle
-type queue_strategy() :: 'rr' | 'mi'.

-record(state, {ignored_member_calls = dict:new() :: dict()
                ,account_id :: api_binary()
                ,queue_id :: api_binary()
                ,supervisor :: pid()
                ,strategy = 'rr' :: queue_strategy() % round-robin | most-idle
                ,strategy_state :: strategy_state()
                ,enter_when_empty = 'true' :: boolean() % allow caller into queue if no agents are logged in
                ,moh :: api_binary()
                ,current_member_calls = [] :: list() % ordered list of current members waiting
                ,pos_announce_enabled = 'false' :: boolean()
                ,wait_announce_enabled = 'false' :: boolean()
                ,announcements_timer = 30 :: non_neg_integer()
                ,pos_announce_pids = [] :: announce_pid_list()

                ,position_media = <<"queue-you_are_at_position">> :: binary()
                ,in_the_queue_media = <<"queue-in_the_queue">> :: binary()
                ,increase_call_volume_media = <<"queue-increase_in_call_volume">> :: binary()
                ,estimated_wait_time_media = <<"queue-the_estimated_wait_time_is">> :: binary()

                ,registered_callbacks = [] :: list()
               }).
-type mgr_state() :: #state{}.

-ifdef(ERL_VERSION_GT_5_10).
-type queue_strategy_state() :: pqueue4:pqueue() | ne_binaries().
-else.
-type queue_strategy_state() :: queue() | ne_binaries().
-endif.
-type ss_details() :: {non_neg_integer(), 'ringing' | 'busy' | 'undefined'}.
-record(strategy_state, {agents :: queue_strategy_state()
                          %% details include # of agent processes and availability
                         ,details = dict:new() :: dict()
                         ,ringing_agents = [] :: ne_binaries()
                         ,busy_agents = [] :: ne_binaries()
                        }).
-type strategy_state() :: #strategy_state{}.

-define(ACDC_QUEUE_MANAGER_HRL, 'true').
-endif.