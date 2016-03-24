%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Voxter Communications Inc.
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(acdc_recordings_map_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-record(state, {table_id :: integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec register(whapps_call:call(), wh_json:object()) -> pid().
register(Call, RecordingJObj) ->
    gen_server:call(?SERVER, {'register', Call, RecordingJObj}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag('trap_exit', 'true'),
    TabId = ets:new('map', []),
    {'ok', #state{table_id=TabId}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({'register', Call, RecordingJObj}, _From, #state{table_id=TabId}=State) ->
    case ets:lookup(TabId, whapps_call:call_id(Call)) of
        [] ->
            {'ok', Pid} = acdc_recordings_sup:new(Call, RecordingJObj),
            link(Pid),
            ets:insert(TabId, {whapps_call:call_id(Call), Pid}),
            Pid;
        [{_, Pid}] ->
            wh_media_recording:update_control_queue(Pid, whapps_call:control_queue(Call)),
            Pid
    end,
    {'reply', Pid, State};
handle_call(_Request, _From, State) ->
    Reply = 'ok',
    {'reply', Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, #state{table_id=TabId}=State) ->
    ets:match_delete(TabId, {'$1', Pid}),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
