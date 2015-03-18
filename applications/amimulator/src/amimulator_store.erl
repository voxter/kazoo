-module(amimulator_store).
-behaviour(gen_server).

-export([start_link/0, store/2, retrieve/1, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-record(state, {
    store = []
}).

%%
%% Public functions
%%
start_link() ->
    lager:debug("Starting datastore"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Key, Value) ->
	gen_server:call(?MODULE, {store, Key, Value}).

retrieve(Key) ->
	gen_server:call(?MODULE, {retrieve, Key}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

%%
%% gen_server callbacks
%%
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
   
handle_call({store, Key, Value}, {_Pid, _Tag}, #state{store=Store}=State) ->
	{reply, ok, State#state{store=[{Key, Value}] ++ Store}};
handle_call({retrieve, Key}, {_Pid, _Tag}, #state{store=Store}=State) ->
	{reply, proplists:get_value(Key, Store), State};
handle_call({delete, Key}, {_Pid, _Tag}, #state{store=Store}=State) ->
    {reply, ok, State#state{store=proplists:delete(Key, Store)}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(shutdown, #state{}) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
