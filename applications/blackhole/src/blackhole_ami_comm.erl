-module(blackhole_ami_comm).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
         
-include("blackhole.hrl").
         
-record(state, {
    socket,
    accept_socket,
    account_id = <<>>,
    %% A collection of data packets that represent a single command
    bundle = <<>>
}).
 
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).
    
publish_events(Events, Socket) ->
    [publish_event(Event, Socket) || Event <- Events].
  
%% It looks like sometimes, Asterisk sends the messages broken up by newlines...  
publish_event({Props, broken}, Socket) ->
    lists:foreach(fun(Part) ->
        gen_tcp:send(Socket, format_prop(Part))
        end, Props),
    gen_tcp:send(Socket, <<"\r\n">>);
publish_event({Props, _}, Socket) ->
    gen_tcp:send(Socket, format_binary(Props)).

init(Socket) ->
    process_flag(trap_exit, true),
    %% Random seed used to change md5 challenge
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
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
            {noreply, State#state{accept_socket=AcceptSocket}};
        {error, closed} ->
            {stop, normal, State};
        {_, _} ->
            lager:debug("AMI: accept exception"),
            {noreply, State}
    end;
%% Synchronously publish AMI events to socket
handle_cast({publish, Events}, #state{accept_socket=AcceptSocket}=State) ->
    publish_events(Events, AcceptSocket),
    {noreply, State};
handle_cast(_Event, State) ->
    lager:debug("AMI: unhandled cast"),
    {noreply, State}.
    
%% Need to perform a login prior to sending/receiving anything
handle_info({tcp, _Socket, Data}, #state{bundle=Bundle,
        account_id=AccountId}=State) ->
    %% Received commands are buffered until a flush (data containing only \r\n)
    case list_to_binary(Data) of
        <<"\r\n">> ->
            lager:debug("AMI: flush command ~p", [Bundle]),
            maybe_send_response(blackhole_ami_commander:handle(Bundle, AccountId)),
            {noreply, State#state{bundle = <<>>}};
        NewData ->
            {noreply, State#state{bundle = <<Bundle/binary, NewData/binary>>}}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    lager:debug("AMI: Disconnected client"),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    lager:debug("AMI: tcp_error"),
    {stop, normal, State};
handle_info(Info, State) ->
    lager:debug("AMI: unexpected info: ~p~n", [Info]),
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
