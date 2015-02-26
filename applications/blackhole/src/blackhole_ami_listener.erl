-module(blackhole_ami_listener).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
         
-include("blackhole.hrl").
         
-record(state, {
    socket,
    accept_socket,
    status,
    account_id
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

%% Start the listener waiting for socket accept
handle_cast(accept, #state{socket=Socket}=State) ->
    case gen_tcp:accept(Socket) of
        {ok, AcceptSocket} ->
            %% Add another listener to the pool to keep up responsiveness
            blackhole_ami_sup:start_ami_listener(),
            lager:debug("AMI: Client accepted for socket"),
            
            %% Need to wait for login now
            {noreply, State#state{accept_socket=AcceptSocket, status=login}};
        {error, closed} ->
            {stop, normal, State};
        {_, _} ->
            lager:debug("AMI: accept exception"),
            {noreply, State}
    end;
handle_cast(_Event, State) ->
    lager:debug("AMI: unhandled cast"),
    {noreply, State}.
    
%% Need to perform a login prior to sending/receiving anything
handle_info({tcp, _Socket, Data}, #state{status=login, accept_socket=AcceptSocket}=State) ->
    AccountId = blackhole_ami_commander:login(Data),
    case AccountId of
        {ok, _} ->
            %% Start the AMQP service
            blackhole_ami_amqp:start_link(AcceptSocket),
            %% Status moves to active so that AMI commands can be received
            {noreply, State#state{account_id=AccountId, status=active}};
        _ ->
            {stop, normal, State}
    end;

handle_info({tcp, _Socket, Data}, #state{status=active, account_id=AccountId}=State) ->
    blackhole_ami_commander:handle(Data, AccountId),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    lager:debug("AMI: Disconnected client"),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    lager:debug("AMI: tcp_error"),
    {stop, normal, State};
handle_info(Info, State) ->
    lager:debug("AMI: unexpected info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{accept_socket=AcceptSocket}) ->
    % TODO: actually close these accept sockets on restart
    lager:debug("AMI: terminating"),
    case AcceptSocket of
        undefined ->
            ok;
        _ ->
            gen_tcp:close(AcceptSocket),
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
