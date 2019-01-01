%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ami_sm).
-behaviour(gen_server).

-export([start_link/0]).
-export([init_state/1, purge_state/1
        ,registration/2, add_registration/4, delete_registration/1
        ,call/1, call_by_channel/1, new_call/1, update_call/1, delete_call/1
        ,queue_call/3, queue_calls/1, queue_pos/2, fetch_queue_call_data/2, queue_leave/2
        ,conf_cache/1, cache_conf_part/2, delete_cached_conf_part/1
        ,calls/1, channel_call_ids/1, add_channel_call_id/2, call_id_in_channel/2
        ,maybe_ringing/2
        ,answer/2, answered_or_ignored/2, flag_early_answer/1
        ,db_put/4, db_del/3
        ,debug/1, debug_clear_call/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("amimulator.hrl").
-include("amimulator_call.hrl").

-record(state, {}).
-type state() :: #state{}.

%%
%% Public functions
%%

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    lager:debug("Starting state master"),
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

%% Fetches the existing calls, queue state, etc and puts in ETS
-spec init_state(kz_term:ne_binary()) -> 'ok'.
init_state(AccountId) ->
    gen_server:cast(?MODULE, {'init_state', AccountId}).

-spec purge_state(kz_term:ne_binary()) -> 'ok'.
purge_state(AccountId) ->
    gen_server:cast(?MODULE, {'purge_state', AccountId}).

-spec registration(kz_term:ne_binary(), kz_term:ne_binary()) -> term().
registration(AccountId, EndpointId) ->
    gen_server:call(?MODULE, {'get_registration', AccountId, EndpointId}).

-spec add_registration(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pos_integer()) -> 'ok'.
add_registration(AccountId, EndpointId, ContactIP, ContactPort) ->
    gen_server:cast(?MODULE, {'add_registration', AccountId, EndpointId, ContactIP, ContactPort}).

-spec delete_registration(kz_term:ne_binary()) -> 'ok'.
delete_registration(EndpointId) ->
    gen_server:cast(?MODULE, {'delete_registration', EndpointId}).

-spec call(kz_term:ne_binary()) -> term().
call(CallId) ->
    gen_server:call(?MODULE, {'get_call', CallId}, 'infinity').

-spec call_by_channel(kz_term:ne_binary()) -> term().
call_by_channel(Channel) ->
    gen_server:call(?MODULE, {'get_call_by_channel', Channel}).

-spec new_call(amimulator_call:call()) -> 'ok'.
new_call(Call) ->
    gen_server:cast(?MODULE, {'new_call', Call}).

-spec update_call(amimulator_call:call()) -> 'ok'.
update_call(Call) ->
    gen_server:cast(?MODULE, {'update_call', Call}).

-spec delete_call(kz_term:ne_binary()) -> 'ok'.
delete_call(CallId) ->
    gen_server:cast(?MODULE, {'delete_call', CallId}).

-spec queue_call(kz_term:ne_binary(), kz_term:ne_binary(), amimulator_call:call()) -> term().
queue_call(QueueId, CallId, Call) ->
    gen_server:call(?MODULE, {'queue_call', QueueId, CallId, Call}).

-spec queue_calls(kz_term:ne_binary()) -> term().
queue_calls(QueueId) ->
    gen_server:call(?MODULE, {'queue_calls', QueueId}).

-spec queue_pos(kz_term:ne_binary(), kz_term:ne_binary()) -> term().
queue_pos(QueueId, CallId) ->
    gen_server:call(?MODULE, {'queue_pos', QueueId, CallId}).

-spec fetch_queue_call_data(kz_term:ne_binary(), kz_term:ne_binary()) -> term().
fetch_queue_call_data(QueueId, CallId) ->
    gen_server:call(?MODULE, {'get_queue_call', QueueId, CallId}).

-spec queue_leave(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
queue_leave(QueueId, CallId) ->
    gen_server:cast(?MODULE, {'queue_leave', QueueId, CallId}).

-spec conf_cache(kz_term:ne_binary()) -> term().
conf_cache(CallId) ->
    gen_server:call(?MODULE, {'conf_cache', CallId}).

-spec cache_conf_part(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
cache_conf_part(CallId, Data) ->
    gen_server:cast(?MODULE, {'cache_conf_part', CallId, Data}).

-spec delete_cached_conf_part(kz_term:ne_binary()) -> 'ok'.
delete_cached_conf_part(CallId) ->
    gen_server:cast(?MODULE, {'delete_cached_conf_part', CallId}).

-spec calls(kz_term:ne_binary()) -> term().
calls(AccountId) ->
    gen_server:call(?MODULE, {'get_calls', AccountId}, 'infinity').

-spec channel_call_ids(kz_term:ne_binary()) -> term().
channel_call_ids(Channel) ->
    gen_server:call(?MODULE, {'get_call_ids_by_channel', Channel}).

-spec add_channel_call_id(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
add_channel_call_id(Channel, CallId) ->
    gen_server:cast(?MODULE, {'add_channel_call_id', Channel, CallId}).

-spec call_id_in_channel(kz_term:ne_binary(), kz_term:ne_binary()) -> term().
call_id_in_channel(CallId, Channel) ->
    gen_server:call(?MODULE, {'call_id_in_channel', CallId, Channel}).

-spec maybe_ringing(kz_term:ne_binary(), kz_term:ne_binary()) -> term().
maybe_ringing(Channel, CallId) ->
    gen_server:call(?MODULE, {'maybe_ringing', Channel, CallId}).

-spec answer(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
answer(Channel, CallId) ->
    gen_server:cast(?MODULE, {'answer', Channel, CallId}).

-spec answered_or_ignored(kz_term:ne_binary(), kz_term:ne_binary()) -> term().
answered_or_ignored(Channel, CallId) ->
    gen_server:call(?MODULE, {'answered_or_ignored', Channel, CallId}).

%% TODO hopefully we can remove this later
-spec flag_early_answer(kz_term:ne_binary()) -> 'ok'.
flag_early_answer(CallId) ->
    gen_server:cast(?MODULE, {'flag_early_answer', CallId}).

-spec db_put(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
db_put(AccountId, Family, Key, Value) ->
    gen_server:cast(?MODULE, {'db_put', AccountId, Family, Key, Value}).

-spec db_del(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
db_del(AccountId, Family, Key) ->
    gen_server:cast(?MODULE, {'db_del', AccountId, Family, Key}).

-spec debug(atom()) -> term().
debug(TableName) ->
    gen_server:call(?MODULE, {'debug', TableName}).

-spec debug_clear_call(kz_term:ne_binary()) -> 'ok'.
debug_clear_call(_CallId) ->
    io:format("Function not yet implemented~n").
                                                % gen_server:call(?MODULE, {debug_clear_call, CallId}).

%%
%% gen_server callbacks
%%

-spec init([]) -> {'ok', state()}.
init([]) ->
    process_flag('trap_exit', 'true'),
    _ = ets:new('registrations', ['named_table']),
    _ = ets:new('calls', ['named_table', {'keypos', 2}]),
    _ = ets:new('flags', ['named_table']),
    _ = ets:new('channels', ['named_table', 'bag']),
    _ = ets:new('ringing_channels', ['named_table']),
    _ = ets:new('answered_channels', ['named_table']),
    _ = ets:new('queue_calls', ['named_table', 'bag']),
    _ = ets:new('conference_cache_data', ['named_table']),
    _ = ets:new('database', ['named_table', 'bag']),
    {'ok', #state{}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'get_registration', AccountId, EndpointId}, _From, State) ->
    Reply = case ets:match('registrations', {EndpointId, AccountId, '$1', '$2'}) of
                [] ->
                    'not_registered';
                [[IP, Port]] ->
                    [{<<"IP">>, IP}, {<<"Port">>, Port}]
            end,
    {'reply', Reply, State};

handle_call({'get_call', CallId}, _From, State) ->
    Spec = new_call_match_spec(),
    Reply = case ets:select('calls', [{Spec#call{call_id=CallId}, [], ['$_']}]) of
                [] -> 'undefined';
                [Match] -> Match
            end,
    {'reply', Reply, State};

handle_call({'get_calls', AccountId}, _From, State) ->
    Spec = new_call_match_spec(),
    Reply = ets:select('calls', [{Spec#call{account_id=AccountId}, [], ['$_']}]),
    {'reply', Reply, State};

handle_call({'call_id_in_channel', CallId, Channel}, _From, State) ->
    Reply = case ets:match('channels', {Channel, '$1'}) of
                [] ->
                    'false';
                List ->
                    lists:member([CallId], List)
            end,
    {'reply', Reply, State};

handle_call({'get_call_ids_by_channel', Channel}, _From, State) ->
    Reply = lists:flatten(ets:match('channels', {Channel, '$1'})),
    {'reply', Reply, State};

handle_call({'get_call_by_channel', Channel}, From, State) ->
    Reply = case ets:match('channels', {Channel, '$1'}) of
                [] ->
                    lager:debug("Channel ~p not matched exactly, attempting short version", [Channel]),
                    case ets:match('channels', {'$1', '$2'}) of
                        [] ->
                            'undefined';
                        List ->
                            lists:foldl(fun([Channel2, CallId], Result) ->
                                                case binary:match(Channel2, Channel) of
                                                    'nomatch' ->
                                                        Result;
                                                    _ ->
                                                        {_, Result2, _} = handle_call({'get_call', CallId}, From, State),
                                                        Result2
                                                end
                                        end, 'undefined', List)
                    end;
                List ->
                    {_, Result, _} = handle_call({'get_call', hd(hd(List))}, From, State),
                    Result
            end,
    {'reply', Reply, State};

handle_call({'queue_call', QueueId, CallId, Call}, _From, State) ->
    Size = length(ets:match('queue_calls', {QueueId, '_', '_'})),
    ets:insert('queue_calls', {QueueId, CallId, Call}),
    _ = handle_cast({'update_call', Call}, State),
    {'reply', Size+1, State};

handle_call({'queue_calls', QueueId}, _From, State) ->
    Reply = ets:select('queue_calls', [{{QueueId, '$1', '_'}, [], ['$1']}]),
    {'reply', Reply, State};

handle_call({'queue_pos', QueueId, CallId}, _From, State) ->
    Reply = case ets:match('queue_calls', {QueueId, '$1', '_'}) of
                [] ->
                    'undefined';
                List ->
                    amimulator_util:index_of([CallId], List)
            end,
    {'reply', Reply, State};

handle_call({'get_queue_call', QueueId, CallId}, _From, State) ->
    Reply = case ets:match('queue_calls', {QueueId, CallId, '$1'}) of
                [] ->
                    'undefined';
                [[Match]] ->
                    Match
            end,
    {'reply', Reply, State};

handle_call({'conf_cache', CallId}, _From, State) ->
    Reply = case ets:match('conference_cache_data', {CallId, '$1'}) of
                [] ->
                    'undefined';
                [[Match]] ->
                    Match
            end,
    {'reply', Reply, State};

handle_call({'maybe_ringing', Channel, CallId}, _From, State) ->
    Reply = case ets:match('ringing_channels', {Channel, '$1'}) of
                [] ->
                    ets:insert('ringing_channels', {Channel, CallId}),
                    'true';
                [[CallId]] ->
                    'true';
                _ ->
                    'false'
            end,
    {'reply', Reply, State};

handle_call({'answered_or_ignored', Channel, CallId}, _From, State) ->
    Reply = case ets:match('answered_channels', {Channel, '$1'}) of
                [] ->
                    'true';
                [[CallId]] ->
                    'true';
                _ ->
                    'false'
            end,
    {'reply', Reply, State};

handle_call({'debug', TableName}, _From, State) ->
    {'reply', ets:tab2list(TableName), State};

handle_call(_Request, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'init_state', AccountId}, State) ->
    lager:debug("initializing account ~s state", [AccountId]),
    pvt_init_state(AccountId),
    {'noreply', State};

handle_cast({'purge_state', AccountId}, State) ->
    pvt_purge_state(AccountId),
    {'noreply', State};

handle_cast({'add_registration', AccountId, EndpointId, ContactIP, ContactPort}, State) ->
    ets:insert('registrations', {EndpointId, AccountId, ContactIP, ContactPort}),
    {'noreply', State};

handle_cast({'delete_registration', EndpointId}, State) ->
    ets:delete('registrations', EndpointId),
    {'noreply', State};

handle_cast({'new_call', Call}, State) ->
    CallId = amimulator_call:call_id(Call),
    case ets:match('flags', {CallId, 'early_answer'}) of
        [] ->
            'ok';
        [[]] ->
            lager:debug("The early-answered call (~p) was registered somewhere", [CallId])
    end,

    ets:insert('calls', Call),
    ets:insert('channels', {amimulator_call:channel(Call), CallId}),
    {'noreply', State};

handle_cast({'update_call', Call}, State) ->
    CallId = amimulator_call:call_id(Call),
    Spec = new_call_match_spec(),
    case ets:select('calls', [{Spec#call{call_id=CallId}, [], ['$_']}]) of
        [] -> lager:debug("Trying to update a call (~p) that is not in the calls table", [CallId]);
        [_] -> ets:insert('calls', Call)
    end,
    {'noreply', State};

handle_cast({'delete_call', CallId}, State) ->
    Spec = new_call_match_spec(),
    case ets:select('calls', [{Spec#call{call_id=CallId}, [], ['$_']}]) of
        [] -> lager:debug("Trying to delete a call (~p) that is not in the calls table", [CallId]);
        [Match] ->
            Channel = amimulator_call:channel(Match),
            ets:delete('calls', CallId),
            ets:delete_object('channels', {Channel, CallId}),
            ets:delete_object('ringing_channels', {Channel, CallId}),
            ets:delete_object('answered_channels', {Channel, CallId})
    end,
    {'noreply', State};

handle_cast({'add_channel_call_id', Channel, CallId}, State) ->
    ets:insert('channels', {Channel, CallId}),
    {'noreply', State};

handle_cast({'queue_leave', QueueId, CallId}, State) ->
    case ets:match('queue_calls', {QueueId, CallId, '$1'}) of
        [] ->
            lager:debug("Trying to clean up a queue call (~p) that is not in the queue_calls table", [CallId]);
        [[Call]] ->
            ets:delete_object('queue_calls', {QueueId, CallId, Call})
    end,
    {'noreply', State};

handle_cast({'cache_conf_part', CallId, Data}, State) ->
    ets:insert('conference_cache_data', {CallId, Data}),
    {'noreply', State};

handle_cast({'delete_cached_conf_part', CallId}, State) ->
    ets:delete('conference_cache_data', CallId),
    {'noreply', State};

handle_cast({'answer', Channel, CallId}, State) ->
    case ets:match('answered_channels', {Channel, '_'}) of
        [] ->
            ets:insert('answered_channels', {Channel, CallId});
        _ ->
            'ok'
    end,
    {'noreply', State};

handle_cast({'flag_early_answer', CallId}, State) ->
    ets:insert('flags', {CallId, 'early_answer'}),
    {'noreply', State};

handle_cast({'db_put', AccountId, Family, Key, Value}, State) ->
    ets:insert('database', {{AccountId, Family, Key}, Value}),
    {'noreply', State};

handle_cast({'db_del', AccountId, Family, Key}, State) ->
    ets:delete('database', {AccountId, Family, Key}),
    {'noreply', State};

                                                % handle_cast({debug_clear_call, CallId}, State) ->
                                                %       case ets:match(calls, {CallId, '$1', '$2'}) of
                                                %               [] ->
                                                %                       io:format("The call ~p does not exist", [CallId]);
                                                %               [[AccountId, Call]] ->
                                                %                       ets:delete_object(calls, {CallId, AccountId, Call}),
                                                %                       ets:delete_object(channels, {props:get_value(<<"aleg_ami_channel">>, Call), })


                                                %       lists:foreach(fun([CallId, Call]) ->
                                                %               ets:delete_object('calls', {CallId, AccountId, Call}),
                                                %               ets:delete_object('channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
                                                %               ets:delete_object('ringing_channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
                                                %               ets:delete_object('answered_channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId})
                                                %               %% queue_calls
                                                %               %% conference_participants
                                                %       end, ets:match('calls', {'$1', AccountId, '$2'})).




                                                %        ets:new('registrations', ['named_table']),
                                                %     ets:new('calls', ['named_table']),
                                                %     ets:new('flags', ['named_table']),
                                                %     ets:new('channels', ['named_table', 'bag']),
                                                %     ets:new('ringing_channels', ['named_table']),
                                                %     ets:new('answered_channels', ['named_table']),
                                                %     ets:new('queue_calls', ['named_table', 'bag']),
                                                %     ets:new('conference_cache_data', ['named_table']),

handle_cast(_Request, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate('shutdown', _State) ->
    lager:debug("Received shutdown request"),
    'ok';
terminate(Reason, _State) ->
    lager:debug("Unexpected terminate (~p)", [Reason]),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%
%% Private functions
%%

-spec new_call_match_spec() -> amimulator_call().
new_call_match_spec() ->
    #call{call_id='_'
         ,other_leg_call_id='_'
         ,channel='_'
         ,other_channel='_'
         ,account_id='_'
         ,authorizing_id='_'
         ,authorizing_type='_'
         ,custom_channel_vars='_'
         ,control_q='_'
         ,acdc_queue_id='_'
         ,agent_id='_'
         ,conference_id='_'
         ,username='_'
         ,to='_'
         ,from='_'
         ,direction='_'
         ,answered='_'
         ,timestamp='_'
         ,caller_id_name='_'
         ,caller_id_number='_'
         ,callee_id_name='_'
         ,callee_id_number='_'
         }.

%% By doing the state initialization here, we ensure that it is atomic, and no
%% functions can read from the data store before it is ready
pvt_init_state(AccountId) ->
    Calls = amimulator_util:initial_calls(AccountId),

    lists:foreach(fun(Call) ->
                          ets:insert('calls', Call),
                          ets:insert('channels', {amimulator_call:channel(Call), amimulator_call:call_id(Call)}),
                          ringing_on_init(amimulator_call:answered(Call), Call),
                          answered_on_init(amimulator_call:answered(Call), Call),
                          queue_on_init(amimulator_call:acdc_queue_id(Call), amimulator_call:other_channel(Call), Call),
                          %% TODO implement
                          conf_on_init('undefined', Call),

                          amimulator_call_sup:initial_call(Call)
                  end, Calls),

    initial_registrations(AccountId).

pvt_purge_state(AccountId) ->
    Spec = new_call_match_spec(),
    lists:foreach(fun(Call) ->
                          CallId = amimulator_call:call_id(Call),
                          Channel = amimulator_call:channel(Call),
                          ets:delete('calls', CallId),
                          ets:delete_object('channels', {Channel, CallId}),
                          ets:delete_object('ringing_channels', {Channel, CallId}),
                          ets:delete_object('answered_channels', {Channel, CallId}),
                          maybe_purge_queue_call(amimulator_call:acdc_queue_id(Call), Call)
                          %% TODO conference_participants
                  end, ets:select('calls', [{Spec#call{account_id=AccountId}, [], ['$_']}])).

-spec ringing_on_init(kz_term:api_boolean(), amimulator_call()) -> boolean().
ringing_on_init('true', _) ->
    'false';
ringing_on_init('false', Call) ->
    ets:insert('ringing_channels', {amimulator_call:channel(Call), amimulator_call:call_id(Call)}).

-spec answered_on_init(kz_term:api_boolean(), amimulator_call()) -> boolean().
answered_on_init('true', Call) ->
    ets:insert('answered_channels', {amimulator_call:channel(Call), amimulator_call:call_id(Call)});
answered_on_init('false', _) ->
    'false'.

queue_on_init('undefined', _, _) ->
    'false';
queue_on_init(QueueId, 'undefined', Call) ->
    ets:insert('queue_calls', {QueueId, amimulator_call:call_id(Call), Call});
queue_on_init(_, _, _) ->
    false.

%% TODO implement
conf_on_init(_, _) ->
    'false'.

-spec maybe_purge_queue_call(kz_term:api_binary(), amimulator_call:call()) -> boolean().
maybe_purge_queue_call('undefined', _) ->
    'false';
maybe_purge_queue_call(QueueId, Call) ->
    CallId = amimulator_call:call_id(Call),
    case ets:match('queue_calls', {QueueId, CallId, '$1'}) of
        [] -> 'false';
        [[Call2]] -> ets:delete_object('queue_calls', {QueueId, CallId, Call2})
    end.

initial_registrations(AccountId) ->
    {'ok', AccountDoc} = kz_datamgr:open_doc(<<"accounts">>, AccountId),
    AccountRealm = kz_json:get_value(<<"realm">>, AccountDoc),

    Req = [
           {<<"Realm">>, AccountRealm}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    ReqResp = kapps_util:amqp_pool_collect(
                Req,
                fun kapi_registration:publish_query_req/1,
                {'ecallmgr', 'true'}
               ),
    case ReqResp of
        {'error', E} ->
            lager:debug("Initial registrations failed (~p)", [E]);
        {_, JObjs} ->
            lager:debug("~p responses for list of registrations", [length(JObjs)]),
            lists:foreach(fun(JObj) ->
                                  lists:foreach(fun(RegJObj) ->
                                                % lager:debug("Going to add registration for username ~p", [kz_json:get_value(<<"Username">>, RegJObj)]),
                                                        EndpointId = case kz_json:get_value(<<"Owner-ID">>, RegJObj) of
                                                                         <<"undefined">> ->
                                                                             kz_json:get_value(<<"Authorizing-ID">>, RegJObj);
                                                                         OwnerId ->
                                                                             OwnerId
                                                                     end,
                                                        NormalizedReg = cb_registrations:normalize_registration(RegJObj),
                                                        ets:insert('registrations', {EndpointId, AccountId, kz_json:get_value(<<"contact_ip">>, NormalizedReg), kz_json:get_value(<<"contact_port">>, NormalizedReg)})
                                                end, kz_json:get_value(<<"Fields">>, JObj))
                          end, JObjs)
    end.
