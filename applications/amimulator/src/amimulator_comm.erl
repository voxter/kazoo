-module(amimulator_comm).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
         
-include("amimulator.hrl").
         
-record(state, {
    listen_socket,
    accept_socket,
    %% A collection of data packets that represent a single command
    bundle = <<>>,
    originator_pid
}).

%%
%% Public functions
%%

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%%
%% gen_server and gen_tcp callbacks
%%

init(Socket) ->
    process_flag(trap_exit, true),
    %% Random seed used to change md5 challenge
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),

    %% Register this handler with the state master
    ami_sm:register(),

    gen_server:cast(self(), accept),
    {ok, #state{listen_socket=Socket}}.

handle_call(Request, _From, State) ->
    lager:debug("unhandled call"),
    {stop, {unknown_call, Request}, State}.

%% Start the listener waiting for socket accept
handle_cast(accept, #state{listen_socket=Socket}=State) ->
    case gen_tcp:accept(Socket) of
        {ok, AcceptSocket} ->
            lager:debug("Client accepted for socket"),

            %% Send announcement to clients of who we are
            %% Required for queuestats application
            gen_tcp:send(AcceptSocket, <<"Asterisk Call Manager/1.1\r\n">>),
            
            %% Need to wait for login now
            {noreply, State#state{accept_socket=AcceptSocket}};
        {error, closed} ->
        	% lager:debug("Listen socket closed"),
            {noreply, State};
        {_, _} ->
            lager:debug("Exception occurred when waiting for socket accept"),
            {noreply, State}
    end;
handle_cast({login, AccountId}, State) ->
    {ok, OriginatorPid} = amimulator_originator:start_link(),
    maybe_init_state(AccountId),
    {noreply, State#state{originator_pid=OriginatorPid}};
handle_cast({logout}, #state{accept_socket=AcceptSocket, originator_pid=OriginatorPid}=State) ->
    inet:setopts(AcceptSocket, [{nodelay, true}]),
    gen_tcp:send(AcceptSocket, <<"Response: Goodbye\r\nMessage: Thanks for all the fish.\r\n\r\n">>),
    inet:setopts(AcceptSocket, [{nodelay, false}]),

    case AcceptSocket of
        undefined ->
            ok;
        _ ->
            lager:debug("Closing an accept socket"),
            gen_tcp:close(AcceptSocket),
            ok
    end,
    exit(OriginatorPid, kill),

    gen_server:cast(self(), accept),
    {noreply, State#state{accept_socket=undefined, bundle = <<>>, originator_pid=undefined}};
handle_cast({originator, Action, Props}, #state{originator_pid=OriginatorPid}=State) ->
    gen_listener:cast(OriginatorPid, {Action, Props}),
    {noreply, State};
%% Synchronously publish AMI events to socket
handle_cast({publish, Events}, #state{accept_socket=AcceptSocket}=State) ->
    publish_events(Events, AcceptSocket),
    {noreply, State};
handle_cast(Event, State) ->
    lager:debug("unhandled cast ~p", [Event]),
    {noreply, State}.
    
%% Route socket data to command processor
handle_info({tcp, _Socket, Data}, #state{bundle=Bundle}=State) ->
    AccountId = ami_sm:account_id(),

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
    lager:debug("One less consumer for the account."),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    lager:debug("tcp_error"),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(shutdown, #state{accept_socket=AcceptSocket}) ->
	% TODO: actually close these accept sockets on restart
    case AcceptSocket of
        undefined ->
            ok;
        _ ->
            lager:debug("Closing an accept socket"),
            gen_tcp:close(AcceptSocket),
            ok
    end,
    ami_sm:unregister();
terminate(Reason, State) ->
    lager:debug("Unexpected terminate (~p)", [Reason]),
    terminate(shutdown, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private functions
%%

%% Build up state for this account if it is not in the state master
maybe_init_state(AccountId) ->
    case ami_sm:calls(AccountId) of
        [] ->
            ami_sm:init_state(AccountId);
        _ ->
            ok
    end.
    
maybe_send_response(HandleResp) ->
    case HandleResp of
        {ok, Resp} ->
            %% Can disable events from AMI
            case ami_sm:events() of
                'on' ->
                    gen_server:cast(self(), {publish, Resp});
                'off' ->
                    ok;
                'undefined' ->
                    ok
            end;
        _ ->
            ok
    end.

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
publish_event(Props, raw, Socket) ->
    inet:setopts(Socket, [{nodelay, true}]),
    lists:foreach(fun(Part) ->
        gen_tcp:send(Socket, Part)
        end, Props),
    inet:setopts(Socket, [{nodelay, false}]);
publish_event(Props, _, Socket) ->
    %lager:debug("AMI: publish ~p", [Props]),
    gen_tcp:send(Socket, amimulator_util:format_binary(Props)).



