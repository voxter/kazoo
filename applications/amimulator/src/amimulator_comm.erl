-module(amimulator_comm).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
         
-include("amimulator.hrl").
         
-record(state, {
    listen_socket,
    accept_socket,
    account_id = <<>>,
    %% A collection of data packets that represent a single command
    bundle = <<>>,
    originator_pid
}).
 
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).
    
publish_events({[Event|_]=Events, Mode}, Socket) when is_list(Event) ->
    [publish_event(Event2, Mode, Socket) || Event2 <- Events];
publish_events({Event, Mode}, Socket) ->
    publish_event(Event, Mode, Socket).
  
%% It looks like sometimes, Asterisk sends the messages broken up by newlines...  
publish_event(Props, broken, Socket) ->
    lists:foreach(fun(Part) ->
        gen_tcp:send(Socket, amimulator_util:format_prop(Part))
        end, Props),
    gen_tcp:send(Socket, <<"\r\n">>);
publish_event(Props, _, Socket) ->
    %lager:debug("AMI: publish ~p", [Props]),
    gen_tcp:send(Socket, amimulator_util:format_binary(Props)).

init(Socket) ->
    process_flag(trap_exit, true),
    %% Random seed used to change md5 challenge
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    gen_server:cast(self(), accept),
    {ok, #state{listen_socket=Socket}}.

handle_call(account_id, _, #state{account_id=AcctId}=State) ->
    {reply, AcctId, State};
handle_call(Request, _From, State) ->
    lager:debug("unhandled call"),
    {stop, {unknown_call, Request}, State}.

%% Start the listener waiting for socket accept
handle_cast(accept, #state{listen_socket=Socket}=State) ->
    case gen_tcp:accept(Socket) of
        {ok, AcceptSocket} ->
            %% Add another listener to the pool to keep up responsiveness
            amimulator_sup:start_listener(),
            lager:debug("Client accepted for socket"),
            
            %% Need to wait for login now
            {noreply, State#state{accept_socket=AcceptSocket}};
        {error, closed} ->
            {stop, normal, State};
        {_, _} ->
            lager:debug("accept exception"),
            {noreply, State}
    end;
handle_cast({login, AccountId}, State) ->
    {ok, OriginatorPid} = blackhole_ami_originator:start_link(),
    {noreply, State#state{account_id=AccountId,originator_pid=OriginatorPid}};
handle_cast({originator, Action, Props}, #state{originator_pid=OriginatorPid}=State) ->
    gen_listener:cast(OriginatorPid, {Action, Props}),
    {noreply, State};
%% Synchronously publish AMI events to socket
handle_cast({publish, Events}, #state{accept_socket=AcceptSocket}=State) ->
    publish_events(Events, AcceptSocket),
    {noreply, State};
handle_cast(_Event, State) ->
    lager:debug("unhandled cast"),
    {noreply, State}.
    
%% Need to perform a login prior to sending/receiving anything
handle_info({tcp, _Socket, Data}, #state{bundle=Bundle,
        account_id=AccountId}=State) ->
    %% Received commands are buffered until a flush (data containing only \r\n)
    case list_to_binary(Data) of
        <<"\r\n">> ->
            maybe_send_response(amimulator_commander:handle(Bundle, AccountId)),
            {noreply, State#state{bundle = <<>>}};
        NewData ->
            {noreply, State#state{bundle = <<Bundle/binary, NewData/binary>>}}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    lager:debug("Disconnected client"),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    lager:debug("tcp_error"),
    {stop, normal, State};
handle_info(Info, State) ->
    lager:debug("unexpected info: ~p~n", [Info]),
    {noreply, State}.
    
maybe_send_response(HandleResp) ->
    case HandleResp of
        {ok, Resp} ->
            gen_server:cast(self(), {publish, Resp});
        _ ->
            ok
    end.

terminate(_Reason, #state{accept_socket=AcceptSocket}) ->
    % TODO: actually close these accept sockets on restart
    lager:debug("terminating"),
    case AcceptSocket of
        undefined ->
            ok;
        _ ->
            gen_tcp:close(AcceptSocket),
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
