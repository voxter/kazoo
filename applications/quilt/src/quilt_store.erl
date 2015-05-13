-module(quilt_store).
-behaviour(gen_server).

-export([start_link/0, put/2, get/1, delete/1, delete_all/0, enqueue/2, dequeue/1, debug/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("quilt.hrl").

-record(state, {
    store = [],
    queue_start = [],
    queue_count = []
}).

%%
%% Public functions
%%
start_link() ->
    lager:debug("starting datastore"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
	gen_server:call(?MODULE, {put, Key, Value}).

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

debug() ->
    io:format("Store: ~p~nCount: ~p~nStart: ~p~n", gen_server:call(?MODULE, {debug})).

%%
%% gen_server callbacks
%%
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
   
handle_call({put, Key, Value}, {_Pid, _Tag}, #state{store=Store}=State) ->
	{reply, ok, State#state{store=[{Key, Value}] ++ Store}};
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
        State#state{store = [{<<Key/binary, "-", (wh_util:to_binary(Index))/binary>>, Value}] ++ Store,
        queue_count = [{Key, Index+1}] ++ proplists:delete(Key, Count)}};
handle_call({dequeue, Key}, _From, #state{store=Store, queue_count=Count, queue_start=Start}=State) ->
    Index = proplists:get_value(Key, Count, 0),
    St = proplists:get_value(Key, Start, 0),
    FullKey = <<Key/binary, "-", (wh_util:to_binary(St))/binary>>,
    case proplists:get_value(FullKey, Store) of
        undefined ->
            {reply, undefined, State};
        _ ->
            {reply, {proplists:get_value(FullKey, Store), Index-St-1},
                State#state{store=proplists:delete(FullKey, Store),
                queue_start = [{Key, St+1}] ++ proplists:delete(Key, Start)}}
    end;
handle_call({debug}, _From, #state{store=Store, queue_count=Count, queue_start=Start}=State) ->
    {reply, [Store, Count, Start], State};
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