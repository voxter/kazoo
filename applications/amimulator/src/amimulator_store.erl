-module(amimulator_store).
-behaviour(gen_server).

-export([start_link/0, put/2, get/1, delete/1, delete_all/0]).
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

put(Key, Value) ->
	gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
	gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

delete_all() ->
    gen_server:call(?MODULE, {delete_all}).

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
