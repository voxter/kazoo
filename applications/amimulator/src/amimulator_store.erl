-module(amimulator_store).
-behaviour(gen_server).

-export([start_link/0, put/2, put_fun/3, get/1, delete/1, delete_all/0, enqueue/2, dequeue/1,
    reverse_lookup/2, debug/0, size/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-record(state, {
    store = [],
    queue_start = [],
    queue_count = []
}).

%%
%% Public functions
%%
start_link() ->
    lager:debug("Starting datastore"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
	gen_server:call(?MODULE, {put, Key, Value}).

put_fun(Key, Value, Fun) ->
    gen_server:call(?MODULE, {put_fun, Key, Value, Fun}).

get(Key) ->
	gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

delete_all() ->
    gen_server:call(?MODULE, {delete_all}).

enqueue(Key, Value) ->
    gen_server:call(?MODULE, {enqueue, Key, Value}).

dequeue(Key) ->
    gen_server:call(?MODULE, {dequeue, Key}).

reverse_lookup(QueueKey, Value) ->
    gen_server:call(?MODULE, {reverse_lookup, QueueKey, Value}).

debug() ->
    io:format("Store: ~p~nCount: ~p~nStart: ~p~n", gen_server:call(?MODULE, {debug})).

size() ->
    gen_server:call(?MODULE, {size}).

%%
%% gen_server callbacks
%%
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
   
handle_call({put, Key, Value}, {_Pid, _Tag}, #state{store=Store}=State) ->
	{reply, ok, State#state{store=[{Key, Value}] ++ proplists:delete(Key, Store)}};
handle_call({put_fun, Key, Value, Fun}, _From, #state{store=Store}=State) ->
    {reply, ok, State#state{store=Fun(Key, Value, Store)}};
handle_call({get, Key}, {_Pid, _Tag}, #state{store=Store}=State) ->
	{reply, proplists:get_value(Key, Store), State};
handle_call({delete, Key}, {_Pid, _Tag}, #state{store=Store}=State) ->
    {reply, ok, State#state{store=proplists:delete(Key, Store)}};
handle_call({delete_all}, _From, State) ->
    {reply, ok, State#state{store=[]}};
handle_call({enqueue, Key, Value}, _From, #state{store=Store, queue_count=Count, queue_start=Start}=State) ->
    Index = proplists:get_value(Key, Count, 0),
    St = proplists:get_value(Key, Start, 0),
    {reply, {Index-St},
        State#state{store = [{{Key, Index}, Value}] ++ Store,
        queue_count = [{Key, Index+1}] ++ proplists:delete(Key, Count)}};
handle_call({dequeue, Key}, _From, #state{store=Store, queue_count=Count, queue_start=Start}=State) ->
    Index = proplists:get_value(Key, Count, 0),
    St = proplists:get_value(Key, Start, 0),
    FullKey = {Key, St},
    case proplists:get_value(FullKey, Store) of
        undefined ->
            {reply, undefined, State};
        _ ->
            {reply, {proplists:get_value(FullKey, Store), Index-St-1},
                State#state{store=proplists:delete(FullKey, Store),
                queue_start = [{Key, St+1}] ++ proplists:delete(Key, Start)}}
    end;
handle_call({reverse_lookup, Key, Value}, _From, #state{store=Store, queue_start=Start}=State) ->
    {reply, reverse_lookup(Key, Value, Store, Start), State};
handle_call({debug}, _From, #state{store=Store, queue_count=Count, queue_start=Start}=State) ->
    {reply, [Store, Count, Start], State};
handle_call({size}, _From, #state{store=Store}=State) ->
    {reply, length(Store), State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

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

reverse_lookup(_Key, _Value, [], _Start) ->
    undefined;
reverse_lookup(Key, Value, [{{Key, Index}, Value}|_Others], Start) ->
    St = proplists:get_value(Key, Start, 0),
    Index-St;
reverse_lookup(Key, Value, [_|Others], Start) ->
    reverse_lookup(Key, Value, Others, Start).




