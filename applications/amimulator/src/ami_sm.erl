-module(ami_sm).
-behaviour(gen_server).

-export([start_link/0, register/0, unregister/0, ev_going_down/2, ev_staying_up/1, is_ev_down/2,
    account_id/0, set_account_id/1,
    init_state/1, purge_state/1, events/0, account_consumers/1, call/1, call_by_channel/1, new_call/2, 
    new_call/3, update_call/2, delete_call/1, queue_call/2, queue_pos/2, conf_parts/1,
    update_conf_parts/2, conf_cache/1, cache_conf_part/2,
    calls/1, channel_call_ids/1, add_channel_call_id/2, maybe_ringing/2, debug/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-record(state, {
    ets
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
    gen_server:cast(?MODULE, {ev_going_down, AccountId, Timestamp}).

ev_staying_up(AccountId) ->
    gen_server:cast(?MODULE, {ev_staying_up, AccountId}).

is_ev_down(AccountId, Timestamp) ->
    gen_server:call(?MODULE, {is_ev_down, AccountId, Timestamp}).

account_id() ->
    gen_server:call(?MODULE, {pid_get, "AccountId"}).

set_account_id(AccountId) ->
    gen_server:cast(?MODULE, {reg_account_id, AccountId, self()}),
    gen_server:cast(?MODULE, {pid_set, "AccountId", AccountId, self()}).

%% Fetches the existing calls, queue state, etc and puts in ETS
init_state(AccountId) ->
    gen_server:cast(?MODULE, {init_state, AccountId}).

purge_state(AccountId) ->
	gen_server:cast(?MODULE, {purge_state, AccountId}).

%% Whether events should be published to this client
events() ->
    gen_server:call(?MODULE, {pid_get, "EventMask"}).

account_consumers(AccountId) ->
    gen_server:call(?MODULE, {get_account_consumers, AccountId}).

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

queue_call(QueueId, CallId) ->
    gen_server:call(?MODULE, {queue_call, QueueId, CallId}).

queue_pos(QueueId, CallId) ->
    gen_server:call(?MODULE, {queue_pos, QueueId, CallId}).

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
    gen_server:call(?MODULE, {get_channel_call_ids, Channel}).

add_channel_call_id(Channel, CallId) ->
    gen_server:cast(?MODULE, {concat, "ChannelCallIds", Channel, CallId}).

maybe_ringing(Channel, CallId) ->
    gen_server:call(?MODULE, {maybe_ringing, Channel, CallId}).

debug() ->
    gen_server:cast(?MODULE, {debug}).

%%
%% gen_server callbacks
%%

init([]) ->
    process_flag(trap_exit, true),
    ETS = init_ets(),
    {ok, #state{ets=ETS}}.

handle_call({is_ev_down, AccountId, Timestamp}, _From, #state{ets=ETS}=State) ->
    Reply = case get("EvDown", AccountId, ETS) of
        Timestamp ->
            true;
        _ ->
            false
    end,
    {reply, Reply, State};

handle_call({get_account_consumers, AccountId}, _From, #state{ets=ETS}=State) ->
    Pids2 = case get("Pid-Lookup", AccountId, ETS) of
        undefined ->
            [];
        Pids ->
            Pids
    end,
    {reply, Pids2, State};
   
handle_call({get_call, CallId}, _From, #state{ets=ETS}=State) ->
    {reply, get("Call", CallId, ETS), State};

handle_call({get_call_by_channel, Channel}, _From, #state{ets=ETS}=State) ->
    Call = case get("ChannelCallIds", Channel, ETS) of
        undefined ->
            undefined;
        CallIds ->
            get("Call", hd(CallIds), ETS)
    end,
    {reply, Call, State};

handle_call({queue_call, QueueId, CallId}, _From, #state{ets=ETS}=State) ->
    concat("Queue", QueueId, CallId, ETS),
    Position = pos("Queue", QueueId, CallId, ETS),
    {reply, Position, State};

handle_call({queue_pos, QueueId, CallId}, _From, #state{ets=ETS}=State) ->
    {reply, pos("Queue", QueueId, CallId, ETS), State};

handle_call({conf_parts, ConfId}, _From, #state{ets=ETS}=State) ->
    ConfParts = case get("Conf", ConfId, ETS) of
        undefined ->
            [];
        List ->
            List
    end,
    {reply, ConfParts, State};

handle_call({conf_cache, CallId}, _From, #state{ets=ETS}=State) ->
    {reply, get("ConfCache", CallId, ETS), State};

handle_call({get_calls, AccountId}, _From, #state{ets=ETS}=State) ->
    Calls2 = case get("Calls", AccountId, ETS) of
        undefined ->
            undefined;
        Calls ->
            lists:foldl(fun(CallId, Calls3) ->
                [get("Call", CallId, ETS) | Calls3]
            end, [], Calls)
    end,

    {reply, Calls2, State};

handle_call({get_channel_call_ids, Channel}, _From, #state{ets=ETS}=State) ->
    CallIds = case get("ChannelCallIds", Channel, ETS) of
        undefined ->
            undefined;
        CallIds2 ->
            CallIds2
    end,

    {reply, CallIds, State};

handle_call({maybe_ringing, Channel, CallId}, _From, #state{ets=ETS}=State) ->
    CanIRing = case get("UI-Ring", Channel, ETS) of
        undefined ->
            insert("UI-Ring", Channel, CallId, ETS),
            true;
        CallId ->
            true;
        _ ->
            false
    end,
    {reply, CanIRing, State};

handle_call({pid_get, Key}, {Pid, _Tag}, #state{ets=ETS}=State) ->
    {reply, pid_get(Key, Pid, ETS), State};

handle_call(_Request, _From, State) ->
    {noreply, State}.






handle_cast({register, Pid}, #state{ets=ETS}=State) ->
    p_register(Pid, ETS),
    {noreply, State};

handle_cast({unregister, Pid}, #state{ets=ETS}=State) ->
    AccountId = pid_get("AccountId", Pid, ETS),
    remove_elem("Pid-Lookup", AccountId, Pid, ETS),

    %% Might need to request a timeout for pausing the event consumer
    case get("Pid-Lookup", AccountId, ETS) of
        undefined ->
            amimulator_sup:pause_ev(AccountId);
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({ev_going_down, AccountId, Timestamp}, #state{ets=ETS}=State) ->
    insert("EvDown", AccountId, Timestamp, ETS),
    {noreply, State};

handle_cast({ev_staying_up, AccountId}, #state{ets=ETS}=State) ->
    delete("EvDown", AccountId, ETS),
    {noreply, State};

handle_cast({reg_account_id, AccountId, Pid}, #state{ets=ETS}=State) ->
    p_reg_account_id(AccountId, Pid, ETS),
    {noreply, State};
handle_cast({init_state, AccountId}, #state{ets=ETS}=State) ->
    init_state(AccountId, ETS),
    {noreply, State};
handle_cast({purge_state, AccountId}, #state{ets=ETS}=State) ->
	purge_state(AccountId, ETS),
	{noreply, State};

handle_cast({insert, Cat, Key, Value}, #state{ets=ETS}=State) ->
    insert(Cat, Key, Value, ETS),
    {noreply, State};


handle_cast({new_call, CallId, AccountId, Data}, #state{ets=ETS}=State) ->
    insert("Call", CallId, Data, ETS),
    concat("Calls", AccountId, CallId, ETS),
    Channel = props:get_value(<<"aleg_ami_channel">>, Data),
    concat("ChannelCallIds", Channel, CallId, ETS),
    {noreply, State};

handle_cast({update_call, CallId, Data}, #state{ets=ETS}=State) ->
    insert("Call", CallId, Data, ETS),
    {noreply, State};

handle_cast({delete_call, CallId}, #state{ets=ETS}=State) ->
    Call = get("Call", CallId, ETS),
    AccountId = whapps_call:account_id(props:get_value(<<"call">>, Call)),
    Channel = props:get_value(<<"aleg_ami_channel">>, Call),

    delete("Call", CallId, ETS),
    remove_elem("Calls", AccountId, CallId, ETS),
    remove_elem("ChannelCallIds", Channel, CallId, ETS),
    case get("UI-Ring", Channel, ETS) of
        CallId ->
            delete("UI-Ring", Channel, ETS);
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({update_conf_parts, ConfId, Data}, #state{ets=ETS}=State) ->
    insert("Conf", ConfId, Data, ETS),
    {noreply, State};

handle_cast({cache_conf_part, CallId, Data}, #state{ets=ETS}=State) ->
    insert("ConfCache", CallId, Data, ETS),
    {noreply, State};

handle_cast({set, Cat, Key, Value}, #state{ets=ETS}=State) ->
    set(Cat, Key, Value, ETS),
    {noreply, State};

handle_cast({pid_set, Key, Value, Pid}, #state{ets=ETS}=State) ->
    pid_set(Key, Value, Pid, ETS),
    {noreply, State};
handle_cast({concat, Cat, Key, Value}, #state{ets=ETS}=State) ->
    concat(Cat, Key, Value, ETS),
    {noreply, State};
handle_cast({debug}, #state{ets=ETS}=State) ->
    ets:tab2file(ETS, "/tmp/ami_sm_dump"),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(shutdown, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private functions
%%

init_ets() ->
    ets:new(ami_sm_ets, []).

p_register(Pid, ETS) ->
    PidStr = pid_to_list(Pid),
    ets:insert(ETS, [
        {PidStr ++ "-EventMask", "On"},
        {PidStr ++ "-AccountId", undefined}
    ]).

p_reg_account_id(AccountId, Pid, ETS) ->
    concat("Pid-Lookup", AccountId, Pid, ETS).

get(Cat, Key, ETS) ->
    case ets:match(ETS, {Cat ++ "-" ++ wh_util:to_list(Key), '$1'}) of
        [] ->
            undefined;
        [[Match]] ->
            Match
    end.

insert(Cat, Key, Value, ETS) ->
    ets:insert(ETS, {Cat ++ "-" ++ wh_util:to_list(Key), Value}).

set(Cat, Key, Value, ETS) ->
    ets:insert(ETS, {Cat ++ "-" ++ wh_util:to_list(Key), Value}).

delete(Cat, Key, ETS) ->
    ets:delete(ETS, Cat ++ "-" ++ wh_util:to_list(Key)).

pid_get(Key, Pid, ETS) ->
    case ets:match(ETS, {pid_to_list(Pid) ++ "-" ++ Key, '$1'}) of
        [] ->
            undefined;
        [[Match]] ->
            Match
    end.

pid_set(Key, Value, Pid, ETS) ->
    ets:update_element(ETS, pid_to_list(Pid) ++ "-" ++ Key, {2, Value}).

concat(Cat, Key, Value, ETS) ->
    FullKey = Cat ++ "-" ++ wh_util:to_list(Key),
    case ets:match(ETS, {FullKey, '$1'}) of
        [] ->
            ets:insert(ETS, {FullKey, [Value]});
        [[Existing]] ->
            ets:insert(ETS, {FullKey, [Value | Existing]})
    end.

remove_elem(Cat, Key, Value, ETS) ->
    FullKey = Cat ++ "-" ++ wh_util:to_list(Key),
    case ets:match(ETS, {FullKey, '$1'}) of
        [] ->
            ok;
        [[Existing]] ->
            case lists:delete(Value, Existing) of
                [] ->
                    delete(Cat, Key, ETS);
                Remaining ->
                    ets:insert(ETS, {FullKey, Remaining})
            end
    end.

pos(Cat, Key, Value, ETS) ->
    List = get(Cat, Key, ETS),
    find_pos(Value, lists:reverse(List)).

find_pos(Value, List) ->
    find_pos(Value, List, 0).

find_pos(_Value, [], _Index) ->
    undefined;
find_pos(Value, [Value], Index) ->
    Index;
find_pos(Value, [_Wrong|Others], Index) ->
    find_pos(Value, Others, Index+1).

%% By doing the state initialization here, we ensure that it is atomic, and no commander/ami_ev
%% functions can read from the data store before it is ready
init_state(AccountId, ETS) ->
    Calls = amimulator_util:initial_calls(AccountId),

    lists:foreach(fun(Call) ->
        WhappsCall = props:get_value(<<"call">>, Call),
        CallId = whapps_call:call_id(WhappsCall),
        Channel = props:get_value(<<"aleg_ami_channel">>, Call),

        insert("Call", CallId, Call, ETS),
        concat("Calls", AccountId, CallId, ETS),
        concat("ChannelCallIds", Channel, CallId, ETS)
    end, Calls).

purge_state(AccountId, ETS) ->
	Calls = get("Calls", AccountId, ETS),
	lists:foreach(fun(CallId) ->
		delete("Call", CallId, ETS)
	end, Calls),
	delete("Calls", AccountId, ETS).
























