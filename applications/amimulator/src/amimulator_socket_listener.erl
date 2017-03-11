-module(amimulator_socket_listener).
-behaviour(gen_server).

-export([start_link/1]).
-export([login/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-record(state, {listen_socket
               ,accept_socket
                %% A collection of data packets that represent a single command
               ,bundle = <<>>
               ,account_id
               ,event_mask = 'on' :: list() | 'on' | 'off'
               ,challenge
               }).
-type state() :: #state{}.

%%
%% Public functions
%%

-spec start_link(gen_tcp:socket()) -> startlink_ret().
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

-spec login(ne_binary()) -> 'ok'.
login(AccountId) ->
    gen_server:cast(self(), {'login', AccountId}).

%%
%% gen_server and gen_tcp callbacks
%%

-spec init(gen_tcp:socket()) -> {'ok', state()}.
init(Socket) ->
    process_flag('trap_exit', 'true'),
    %% Random seed used to change md5 challenge
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    _ = random:seed({A,B,C}),

    gen_server:cast(self(), 'accept'),
    {'ok', #state{listen_socket=Socket}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(Request, _From, State) ->
    lager:debug("unhandled call"),
    {'stop', {'unknown_call', Request}, State}.

%% Start the listener waiting for socket accept
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('accept', #state{listen_socket=Socket}=State) ->
    case gen_tcp:accept(Socket) of
        {'ok', AcceptSocket} ->
            lager:debug("Client accepted for socket"),

            %% Send announcement to clients of who we are
            %% Required for queuestats application
            _ = gen_tcp:send(AcceptSocket, <<"Asterisk Call Manager/1.1\r\n">>),

            %% Need to wait for login now
            {'noreply', State#state{accept_socket=AcceptSocket}};
        {'error', 'closed'} ->
                                                % lager:debug("Listen socket closed"),
            {'noreply', State};
        {_, _} ->
            lager:debug("Exception occurred when waiting for socket accept"),
            {'noreply', State}
    end;
handle_cast({'login', AccountId}, State) ->
    amimulator_sup:register_event_listener(AccountId, self()),
    {'noreply', State#state{account_id=AccountId}};
handle_cast({'logout'}, #state{accept_socket=AcceptSocket
                              ,account_id=AccountId
                              }=State) ->
    _ = inet:setopts(AcceptSocket, [{'nodelay', 'true'}]),
    _ = gen_tcp:send(AcceptSocket, <<"Response: Goodbye\r\nMessage: Thanks for all the fish.\r\n\r\n">>),
    _ = inet:setopts(AcceptSocket, [{'nodelay', 'false'}]),

    lager:debug("Closing an accept socket"),
    gen_tcp:close(AcceptSocket),

    amimulator_sup:unregister_event_listener(AccountId, self()),
    gen_server:cast(self(), 'accept'),
    {'noreply', State#state{accept_socket='undefined', bundle = <<>>, account_id='undefined', event_mask='on', challenge='undefined'}};
%% Synchronously publish AMI events to socket
handle_cast({'publish', Events}, #state{accept_socket=AcceptSocket}=State) ->
    publish_events(Events, AcceptSocket),
    {'noreply', State};
handle_cast(Event, State) ->
    lager:debug("unhandled cast ~p", [Event]),
    {'noreply', State}.

%% Route socket data to command processor
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'tcp', _Socket, Data}, #state{bundle=Bundle
                                          ,account_id=AccountId
                                          ,event_mask=EventMask
                                          ,challenge=Challenge
                                          }=State) ->
    %% Received commands are buffered until a flush (data containing only \r\n)
    case list_to_binary(Data) of
        <<"\r\n">> ->
            case amimulator_commander:handle(Bundle, AccountId, Challenge) of
                [_|_]=Props ->
                    Result = props:get_value(<<"Ret">>, Props),
                    Challenge2 = props:get_value(<<"Challenge">>, Props),
                    maybe_send_response(EventMask, Result),
                    {'noreply', State#state{bundle = <<>>
                                           ,challenge=Challenge2
                                           }};
                Result ->
                    maybe_send_response(EventMask, Result),
                    {'noreply', State#state{bundle = <<>>
                                           ,challenge='undefined'
                                           }}
            end;
        NewData ->
            {'noreply', State#state{bundle = <<Bundle/binary, NewData/binary>>}}
    end;
handle_info({'tcp_closed', _Socket}, State) ->
    lager:debug("Disconnected client"),
    lager:debug("One less consumer for the account."),
    {'stop', 'normal', State};
handle_info({'tcp_error', _Socket, _}, State) ->
    lager:debug("tcp_error"),
    {'stop', 'normal', State};
handle_info(_Info, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate('shutdown', #state{accept_socket=AcceptSocket}) ->
                                                % TODO: actually close these accept sockets on restart
    case AcceptSocket of
        'undefined' ->
            'ok';
        _ ->
            lager:debug("Closing an accept socket"),
            gen_tcp:close(AcceptSocket),
            'ok'
    end;
terminate(Reason, #state{account_id=AccountId}=State) ->
    lager:debug("Unexpected terminate (~p)", [Reason]),
    amimulator_sup:unregister_event_listener(AccountId, self()),
    terminate('shutdown', State).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%
%% Private functions
%%

-spec maybe_send_response(list() | 'on' | 'off', tuple()) -> 'ok'.
maybe_send_response('off', _) ->
    'ok';
maybe_send_response('on', HandleResp) ->
    send_response(HandleResp);
maybe_send_response(_EventMask, HandleResp) ->
    %% TODO implement
    send_response(HandleResp).

-spec send_response(tuple()) -> 'ok'.
send_response(HandleResp) ->
    case HandleResp of
        {'ok', Resp} -> gen_server:cast(self(), {'publish', Resp});
        _ -> 'ok'
    end.

publish_events({[Event|_]=Events, Mode}, Socket) when is_list(Event) ->
    _ = [publish_event(Event2, Mode, Socket) || Event2 <- Events],
    'ok';
publish_events({Event, Mode}, Socket) ->
    publish_event(Event, Mode, Socket).

%% It looks like sometimes, Asterisk sends the messages broken up by newlines...
publish_event(Props, 'broken', Socket) ->
    lists:foreach(fun(Part) ->
                          gen_tcp:send(Socket, amimulator_util:format_prop(Part))
                  end, Props),
    _ = gen_tcp:send(Socket, <<"\r\n">>),
    'ok';
publish_event(Props, 'raw', Socket) ->
    _ = inet:setopts(Socket, [{'nodelay', 'true'}]),
    lists:foreach(fun(Part) ->
                          gen_tcp:send(Socket, Part)
                  end, Props),
    _ = inet:setopts(Socket, [{'nodelay', 'false'}]),
    'ok';
publish_event(Props, _, Socket) ->
                                                %lager:debug("AMI: publish ~p", [Props]),
    _ = gen_tcp:send(Socket, amimulator_util:format_binary(Props)),
    'ok'.



