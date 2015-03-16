-module(quilt_serv).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("quilt.hrl").

-record(state, {
    listen_socket
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    Logfile = whapps_config:get_string(<<"quilt">>, <<"logfile">>, '/var/log/queue_log'),
    % {ok, ListenSocket} = gen_tcp:listen(AMIPort, [{active, true}, {packet, line}]),
    % lager:debug("listening on port ~p", [AMIPort]),
    
    % quilt_sup:start_link(ListenSocket),
    % quilt_sup:start_listeners(),
    
    % {ok, #state{listen_socket = ListenSocket}}.
    
handle_call(_Request, _From, State) ->
    {noreply, State}.
    
handle_cast(_Request, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(shutdown, #state{listen_socket=ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    lager:debug("gracefully closed listen_socket"),
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.