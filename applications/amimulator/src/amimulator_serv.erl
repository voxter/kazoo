-module(amimulator_serv).
-behaviour(gen_server).

-export([start_link/0, close_listen_socket/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-record(state, {listen_socket
               }).
-type state() :: #state{}.

-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec close_listen_socket() -> 'ok'.
close_listen_socket() ->
    gen_server:cast(?MODULE, 'close_listen_socket').

%%
%% gen_server callbacks
%%

-spec init([]) -> {'ok', state()}.
init([]) ->
    process_flag('trap_exit', 'true'),

    AMIPort = kapps_config:get_integer(<<"amimulator">>, <<"port">>, 5038),
    {'ok', ListenSocket} = gen_tcp:listen(AMIPort, [{'reuseaddr', 'true'}, {'active', 'true'}, {'packet', 'line'}]),
    lager:debug("listening on port ~p", [AMIPort]),
    gen_server:cast(?MODULE, 'start_listeners'),
    {'ok', #state{listen_socket = ListenSocket}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('start_listeners', #state{listen_socket=ListenSocket}=State) ->
    amimulator_supersup:start_listener_sup(),
    amimulator_sup:start_listeners(ListenSocket),
    {'noreply', State};
handle_cast('close_listen_socket', #state{listen_socket=ListenSocket}=State) ->
    gen_tcp:close(ListenSocket),
    lager:debug("listen socket was closed"),
    {'noreply', State};
handle_cast(_Request, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, _State) ->
    lager:debug("terminating (~p)", [Reason]),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
