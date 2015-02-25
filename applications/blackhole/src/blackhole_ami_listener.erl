-module(blackhole_ami_listener).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
         
-include("blackhole.hrl").
         
-record(state, {
    socket
}).
 
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_call(Request, _From, State) ->
    lager:debug("AMI: unhandled call"),
    {stop, {unknown_call, Request}, State}.

handle_cast(accept, #state{socket=Socket}=State) ->
    case gen_tcp:accept(Socket) of
        {ok, AcceptSocket} ->
            blackhole_ami_sup:start_ami_listener(),
            lager:debug("AMI: Client accepted for socket"),
    
            % Create AMQP listener
            blackhole_ami_amqp:start_link(AcceptSocket),
    
            {noreply, State#state{socket=AcceptSocket}};
        {error, closed} ->
            {stop, normal, State};
        {_, _} ->
            lager:debug("AMI: accept exception"),
            {noreply, State}
    end;
handle_cast(_Event, State) ->
    lager:debug("AMI: unhandled cast"),
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
    lager:debug("AMI: received data: ~p", [Data]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    lager:debug("AMI: Disconnected client"),
    gen_tcp:close(State#state.socket),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    lager:debug("AMI: tcp_error"),
    {stop, normal, State};
handle_info(Info, State) ->
    io:format("AMI: unexpected info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    lager:debug("AMI: terminating"),
    gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
