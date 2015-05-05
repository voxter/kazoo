-module(ami_sm).
-behaviour(gen_server).

-export([start_link/0, register/0, unregister/0, ev_going_down/2, ev_staying_up/1, is_ev_down/2,
    account_id/0, set_account_id/1,
    init_state/1, purge_state/1, events/0, account_consumers/1, registration/2, call/1, call_by_channel/1, new_call/2, 
    new_call/3, update_call/2, delete_call/1, queue_call/3, queue_pos/2, fetch_queue_call_data/2, queue_leave/2, conf_parts/1,
    update_conf_parts/2, conf_cache/1, cache_conf_part/2,
    calls/1, channel_call_ids/1, add_channel_call_id/2, call_id_in_channel/2, maybe_ringing/2,
    answer/2, answered_or_ignored/2, debug/1]).%, debug_clear_call/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([initial_registrations/1]).

-export([flag_early_answer/1]).

-include("amimulator.hrl").

-record(state, {
}).

%%
%% Public functions
%%

start_link() ->
    lager:debug("Starting state master"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register an AMI handler and initialize its state
register() ->
    gen_server:cast(?MODULE, {register, self()}).

unregister() ->
    gen_server:cast(?MODULE, {unregister, self()}).

ev_going_down(AccountId, Timestamp) ->
    gen_server:cast(?MODULE, {event_listener_kill_req, AccountId, Timestamp}).

ev_staying_up(AccountId) ->
    gen_server:cast(?MODULE, {event_listener_kill_cancel_req, AccountId}).

is_ev_down(AccountId, Timestamp) ->
    gen_server:call(?MODULE, {is_ev_down, AccountId, Timestamp}).

account_id() ->
    gen_server:call(?MODULE, 'get_account_id', 60000).

set_account_id(AccountId) ->
    gen_server:cast(?MODULE, {reg_account_id, AccountId, self()}).

%% Fetches the existing calls, queue state, etc and puts in ETS
init_state(AccountId) ->
    gen_server:cast(?MODULE, {init_state, AccountId}).

purge_state(AccountId) ->
	gen_server:cast(?MODULE, {purge_state, AccountId}).

%% Whether events should be published to this client
events() ->
    gen_server:call(?MODULE, 'get_event_mask').

account_consumers(AccountId) ->
    gen_server:call(?MODULE, {get_account_consumers, AccountId}).

registration(AccountId, EndpointId) ->
	gen_server:call(?MODULE, {'get_registration', AccountId, EndpointId}).

call(CallId) ->
    gen_server:call(?MODULE, {get_call, CallId}).

call_by_channel(Channel) ->
    gen_server:call(?MODULE, {get_call_by_channel, Channel}).

new_call(CallId, Data) ->
    AccountId = whapps_call:account_id(props:get_value(<<"call">>, Data)),
    gen_server:cast(?MODULE, {new_call, CallId, AccountId, Data}).

new_call(CallId, AccountId, Data) ->
    gen_server:cast(?MODULE, {new_call, CallId, AccountId, Data}).

update_call(CallId, Data) ->
    gen_server:cast(?MODULE, {update_call, CallId, Data}).

delete_call(CallId) ->
    gen_server:cast(?MODULE, {delete_call, CallId}).

queue_call(QueueId, CallId, Call) ->
    gen_server:call(?MODULE, {queue_call, QueueId, CallId, Call}).

queue_pos(QueueId, CallId) ->
    gen_server:call(?MODULE, {queue_pos, QueueId, CallId}).

fetch_queue_call_data(QueueId, CallId) ->
	gen_server:call(?MODULE, {get_queue_call, QueueId, CallId}).

queue_leave(QueueId, CallId) ->
	gen_server:cast(?MODULE, {queue_leave, QueueId, CallId}).

conf_parts(ConfId) ->
    gen_server:call(?MODULE, {conf_parts, ConfId}).

update_conf_parts(ConfId, Data) ->
    gen_server:cast(?MODULE, {update_conf_parts, ConfId, Data}).

conf_cache(CallId) ->
    gen_server:call(?MODULE, {conf_cache, CallId}).

cache_conf_part(CallId, Data) ->
    gen_server:cast(?MODULE, {cache_conf_part, CallId, Data}).

calls(AccountId) ->
    gen_server:call(?MODULE, {get_calls, AccountId}).

channel_call_ids(Channel) ->
    gen_server:call(?MODULE, {get_call_ids_by_channel, Channel}).

add_channel_call_id(Channel, CallId) ->
    gen_server:cast(?MODULE, {'add_channel_call_id', Channel, CallId}).

call_id_in_channel(CallId, Channel) ->
	gen_server:call(?MODULE, {call_id_in_channel, CallId, Channel}).

maybe_ringing(Channel, CallId) ->
    gen_server:call(?MODULE, {maybe_ringing, Channel, CallId}).

answer(Channel, CallId) ->
	gen_server:cast(?MODULE, {answer, Channel, CallId}).

answered_or_ignored(Channel, CallId) ->
	gen_server:call(?MODULE, {'answered_or_ignored', Channel, CallId}).

%% TODO hopefully we can remove this later
flag_early_answer(CallId) ->
	gen_server:cast(?MODULE, {'flag_early_answer', CallId}).

debug(TableName) ->
    gen_server:call(?MODULE, {'debug', TableName}).

debug_clear_call(CallId) ->
	gen_server:call(?MODULE, {debug_clear_call, CallId}).

%%
%% gen_server callbacks
%%

init([]) ->
    process_flag('trap_exit', 'true'),
    ets:new('registrations', ['named_table']),
    ets:new('calls', ['named_table']),
    ets:new('flags', ['named_table']),
    ets:new('channels', ['named_table', 'bag']),
    ets:new('ringing_channels', ['named_table']),
    ets:new('answered_channels', ['named_table']),
    ets:new('queue_calls', ['named_table', 'bag']),
    ets:new('conference_participants', ['named_table', 'bag']),
    ets:new('conference_cache_data', ['named_table']),
    ets:new('event_listener_kill_reqs', ['named_table', 'bag']),
    ets:new('account_consumers', ['named_table', 'bag']),
    {'ok', #state{}}.

handle_call({'is_ev_down', AccountId, Timestamp}, _From, State) ->
	Reply = case ets:match('event_listener_kill_reqs', {AccountId, Timestamp}) of
		[] ->
			false;
        [[]] ->
            true
    end,
    {reply, Reply, State};

handle_call('get_account_id', {Pid, _Tag}, State) ->
	Reply = case ets:match('account_consumers', {'$1', Pid, '_'}) of
		[] ->
			'undefined';
		[[AccountId]] ->
			AccountId
	end,
	{'reply', Reply, State};

handle_call('get_event_mask', {Pid, _Tag}, State) ->
	Reply = case ets:match('account_consumers', {'_', Pid, '$1'}) of
		[] ->
			lager:debug("No event mask setting found for ~p", [Pid]),
			'undefined';
		[[EventMask]] ->
			EventMask
	end,
	{'reply', Reply, State};

handle_call({get_account_consumers, AccountId}, _From, State) ->
	Reply = lists:flatten(ets:match('account_consumers', {AccountId, '$1', '_'})),
    {reply, Reply, State};

handle_call({'get_registration', AccountId, EndpointId}, _From, State) ->
	Reply = case ets:match('registrations', {EndpointId, AccountId, '$1', '$2'}) of
		[] ->
			'not_registered';
		[[IP, Port]] ->
			[{<<"IP">>, IP}, {<<"Port">>, Port}]
	end,
	{'reply', Reply, State};
   
handle_call({get_call, CallId}, _From, State) ->
	Reply = case ets:match('calls', {CallId, '_', '$1'}) of
		[] ->
			undefined;
		[[Match]] ->
			Match
	end,
    {reply, Reply, State};

handle_call({get_calls, AccountId}, _From, State) ->
	Reply = case ets:match('calls', {'_', AccountId, '$1'}) of
		[] ->
			[];
		Results ->
			lists:foldl(fun(Result, Acc) ->
				Result ++ Acc
			end, [], Results)
	end,
    {reply, Reply, State};

handle_call({call_id_in_channel, CallId, Channel}, _From, State) ->
	Reply = case ets:match('channels', {Channel, '$1'}) of
		[] ->
			false;
		List ->
			lists:member([CallId], List)
	end,
	{reply, Reply, State};

handle_call({get_call_ids_by_channel, Channel}, _From, State) ->
	Reply = lists:flatten(ets:match('channels', {Channel, '$1'})),
    {reply, Reply, State};

handle_call({get_call_by_channel, Channel}, From, State) ->
	Reply = case ets:match('channels', {Channel, '$1'}) of
		[] ->
			undefined;
		List ->
			{_, Result, _} = handle_call({get_call, hd(hd(List))}, From, State),
			Result
	end,
    {reply, Reply, State};

handle_call({queue_call, QueueId, CallId, Call}, _From, State) ->
	Size = length(ets:match('queue_calls', {QueueId, '_', '_'})),
	ets:insert('queue_calls', {QueueId, CallId, Call}),
    {reply, Size+1, State};

handle_call({queue_pos, QueueId, CallId}, _From, State) ->
	Reply = case ets:match('queue_calls', {QueueId, '$1', '_'}) of
		[] ->
			undefined;
		List ->
			amimulator_util:index_of([CallId], List)
	end,
    {reply, Reply, State};

handle_call({get_queue_call, QueueId, CallId}, _From, State) ->
	Reply = case ets:match('queue_calls', {QueueId, CallId, '$1'}) of
		[] ->
			undefined;
		[[Match]] ->
			Match
	end,
	{reply, Reply, State};

handle_call({conf_parts, ConfId}, _From, State) ->
	Reply = lists:flatten(ets:match('conference_participants', {ConfId, '$1'})),
    {reply, Reply, State};

handle_call({conf_cache, CallId}, _From, State) ->
	Reply = case ets:match('conference_cache', {CallId, '$1'}) of
		[] ->
			'undefined';
		[[Match]] ->
			Match
	end,
    {reply, Reply, State};

handle_call({'maybe_ringing', Channel, CallId}, _From, State) ->
	Reply = case ets:match('ringing_channels', {Channel, '$1'}) of
		[] ->
			ets:insert('ringing_channels', {Channel, CallId}),
			true;
		[[CallId]] ->
			true;
		_ ->
			false
	end,
    {reply, Reply, State};

handle_call({'answered_or_ignored', Channel, CallId}, _From, State) ->
	Reply = case ets:match('answered_channels', {Channel, '$1'}) of
		[] ->
			true;
		[[CallId]] ->
			true;
		_ ->
			false
	end,
	{reply, Reply, State};

handle_call({'debug', TableName}, _From, State) ->
    {'reply', ets:tab2list(TableName), State};

handle_call(_Request, _From, State) ->
    {noreply, State}.






handle_cast({'register', Pid}, State) ->
    ets:insert('account_consumers', {'undefined', Pid, 'on'}),
    {noreply, State};

handle_cast({unregister, Pid}, State) ->
	case ets:match('account_consumers', {'$1', Pid, '$2'}) of
		[] ->
			ok;
		[[AccountId, EventMask]] ->
			amimulator_sup:pause_ev(AccountId),
			ets:delete_object('account_consumers', {AccountId, Pid, EventMask})
	end,
    {noreply, State};

handle_cast({event_listener_kill_req, AccountId, Timestamp}, State) ->
	ets:insert('event_listener_kill_reqs', {AccountId, Timestamp}),
    {noreply, State};

handle_cast({event_listener_kill_cancel_req, AccountId}, State) ->
	ets:delete('event_listener_kill_reqs', AccountId),
    {noreply, State};

handle_cast({reg_account_id, AccountId, Pid}, State) ->
	case ets:match('account_consumers', {'undefined', Pid, '$1'}) of
		[] ->
			ets:insert('account_consumers', {AccountId, Pid, 'on'});
		[[EventMask]] ->
			ets:delete_object('account_consumers', {'undefined', Pid, EventMask}),
			ets:insert('account_consumers', {AccountId, Pid, EventMask})
	end,
	{'noreply', State};

handle_cast({init_state, AccountId}, State) ->
    pvt_init_state(AccountId),
    {noreply, State};

handle_cast({purge_state, AccountId}, State) ->
	pvt_purge_state(AccountId),
	{noreply, State};

handle_cast({new_call, CallId, AccountId, Call}, State) ->
	case ets:match('flags', {CallId, 'early_answer'}) of
		[] ->
			ok;
		[[]] ->
			lager:debug("The early-answered call (~p) was registered somewhere", [CallId])
	end,

	ets:insert('calls', {CallId, AccountId, Call}),
	ets:insert('channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
    {noreply, State};

handle_cast({update_call, CallId, Call}, State) ->
	case ets:match('calls', {CallId, '$1', '_'}) of
		[] ->
			lager:debug("Trying to update a call (~p) that is not in the calls table", [CallId]);
		[[AccountId]] ->
			ets:insert('calls', {CallId, AccountId, Call})
	end,
    {noreply, State};

handle_cast({delete_call, CallId}, State) ->
	case ets:match('calls', {CallId, '_', '$1'}) of
		[] ->
			lager:debug("Trying to delete a call (~p) that is not in the calls table", [CallId]);
		[[Call]] ->
			ets:delete('calls', CallId),
			ets:delete_object('channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
			ets:delete_object('ringing_channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
			ets:delete_object('answered_channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId})
	end,
    {noreply, State};

handle_cast({'add_channel_call_id', Channel, CallId}, State) ->
	ets:insert('channels', {Channel, CallId}),
	{'noreply', State};

handle_cast({queue_leave, QueueId, CallId}, State) ->
	case ets:match('queue_calls', {QueueId, CallId, '$1'}) of
		[] ->
			lager:debug("Trying to clean up a queue call (~p) that is not in the queue_calls table", [CallId]);
		[[Call]] ->
			ets:delete_object('queue_calls', {QueueId, CallId, Call})
	end,
	{noreply, State};

handle_cast({update_conf_parts, ConfId, Data}, State) ->
	ets:insert('conference_participants', {ConfId, Data}),
    {noreply, State};

handle_cast({cache_conf_part, CallId, Data}, State) ->
	ets:insert('conference_cache', {CallId, Data}),
    {noreply, State};

handle_cast({answer, Channel, CallId}, State) ->
	ets:insert('answered_channels', {Channel, CallId}),
	{noreply, State};

handle_cast({'flag_early_answer', CallId}, State) ->
	ets:insert('flags', {CallId, 'early_answer'}),
	{'noreply', State};

% handle_cast({debug_clear_call, CallId}, State) ->
% 	case ets:match(calls, {CallId, '$1', '$2'}) of
% 		[] ->
% 			io:format("The call ~p does not exist", [CallId]);
% 		[[AccountId, Call]] ->
% 			ets:delete_object(calls, {CallId, AccountId, Call}),
% 			ets:delete_object(channels, {props:get_value(<<"aleg_ami_channel">>, Call), })


% 	lists:foreach(fun([CallId, Call]) ->
% 		ets:delete_object('calls', {CallId, AccountId, Call}),
% 		ets:delete_object('channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
% 		ets:delete_object('ringing_channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
% 		ets:delete_object('answered_channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId})
% 		%% queue_calls
% 		%% conference_participants
% 	end, ets:match('calls', {'$1', AccountId, '$2'})).




% 	 ets:new('registrations', ['named_table']),
%     ets:new('calls', ['named_table']),
%     ets:new('flags', ['named_table']),
%     ets:new('channels', ['named_table', 'bag']),
%     ets:new('ringing_channels', ['named_table']),
%     ets:new('answered_channels', ['named_table']),
%     ets:new('queue_calls', ['named_table', 'bag']),
%     ets:new('conference_participants', ['named_table', 'bag']),
%     ets:new('conference_cache_data', ['named_table']),
%     ets:new('event_listener_kill_reqs', ['named_table', 'bag']),
%     ets:new('account_consumers', ['named_table', 'bag']),

handle_cast(_Request, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate('shutdown', _State) ->
	lager:debug("Received shutdown request"),
    'ok';
terminate(Reason, _State) ->
    lager:debug("Unexpected terminate (~p)", [Reason]),
    'ok'.
    
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%
%% Private functions
%%

%% By doing the state initialization here, we ensure that it is atomic, and no commander/ami_ev
%% functions can read from the data store before it is ready
pvt_init_state(AccountId) ->
    Calls = amimulator_util:initial_calls2(AccountId),

    lists:foreach(fun(Call) ->
        WhappsCall = props:get_value(<<"call">>, Call),
        CallId = whapps_call:call_id(WhappsCall),
        Channel = props:get_value(<<"aleg_ami_channel">>, Call),

        ets:insert('calls', {CallId, AccountId, Call}),
        ets:insert('channels', {Channel, CallId})
    end, Calls),

    initial_registrations(AccountId).

pvt_purge_state(AccountId) ->
	lists:foreach(fun([CallId, Call]) ->
		ets:delete_object('calls', {CallId, AccountId, Call}),
		ets:delete_object('channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
		ets:delete_object('ringing_channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId}),
		ets:delete_object('answered_channels', {props:get_value(<<"aleg_ami_channel">>, Call), CallId})
		%% queue_calls
		%% conference_participants
	end, ets:match('calls', {'$1', AccountId, '$2'})).

initial_registrations(AccountId) ->
	{ok, AccountDoc} = couch_mgr:open_doc(<<"accounts">>, AccountId),
    AccountRealm = wh_json:get_value(<<"realm">>, AccountDoc),

    Req = [
        {<<"Realm">>, AccountRealm}
        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ],

    ReqResp = whapps_util:amqp_pool_collect(
        Req,
        fun wapi_registration:publish_query_req/1,
        {'ecallmgr', 'true'}
    ),
    case ReqResp of
        {'error', E} ->
        	lager:debug("Initial registrations failed (~p)", [E]),
        	[];
        {_, JObjs} ->
        	lager:debug("~p responses for list of registrations", [length(JObjs)]),
            lists:foreach(fun(JObj) ->
                lists:foreach(fun(RegJObj) ->
                    % lager:debug("Going to add registration for username ~p", [wh_json:get_value(<<"Username">>, RegJObj)]),
                    EndpointId = case wh_json:get_value(<<"Owner-ID">>, RegJObj) of
                    	<<"undefined">> ->
                    		wh_json:get_value(<<"Authorizing-ID">>, RegJObj);
                    	OwnerId ->
                    		OwnerId
                    end,
                    NormalizedReg = cb_registrations:normalize_registration(RegJObj),
                    ets:insert('registrations', {EndpointId, AccountId, wh_json:get_value(<<"contact_ip">>, NormalizedReg), wh_json:get_value(<<"contact_port">>, NormalizedReg)})
                end, wh_json:get_value(<<"Fields">>, JObj))
            end, JObjs)
    end.



