%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% @author Ben Partridge
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(nv_apns_pool_worker).

-behaviour(gen_server).

-include("navi.hrl").

-export([start_link/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {default_topic  :: kz_term:ne_binary()
               ,apns_pid       :: pid()
               ,pushkit        :: boolean()
               }).
-type state() :: #state{}.

-type apns_notification_payload() :: #{'alert' => binary()
                                      ,'badge' => integer()
                                      ,'sound' => binary()
                                      ,'content-available' => 1
                                      ,'category' => binary()
                                      ,'thread-id' => binary()
                                      }.
-type apns_payload() :: #{'aps' => apns_notification_payload()
                         ,'metadata' => kz_term:proplist()
                         }.

-spec start_link(kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Args) ->
    PoolName = props:get_value('pool_name', Args),
    Connection = props:get_value('connection', Args),
    DefaultTopic = props:get_value('default_topic', Args),
    IsPushkit = props:get_value('pushkit', Args),

    lager:debug("starting apns notification worker for supervisor: ~s", [PoolName]),
    gen_server:start_link(?MODULE, [PoolName, Connection, DefaultTopic, IsPushkit], []).

-spec init(any()) -> {'ok', state()}.
init([PoolName, Connection, DefaultTopic, IsPushkit]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(<<(kz_term:to_binary(PoolName))/binary, "_worker">>),

    {'ok', Pid} = apns:connect(Connection),
    {'ok', #state{default_topic=DefaultTopic, apns_pid=Pid, pushkit=IsPushkit}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'push', {RegistrationId, Message, ExtraParams}}, #state{default_topic=Topic, apns_pid=Pid}=State) ->
    lager:debug("received request to push notification into apns worker ~p", [self()]),

    TrueTopic = get_true_topic(Topic, ExtraParams),
    lager:debug("using topic ~p", [TrueTopic]),

    Headers = #{'apns_topic' => TrueTopic},

    BaseNotification = #{'aps' => #{'alert' => Message}
                        ,'metadata' => kz_json:to_map(props:get_value(<<"metadata">>, ExtraParams, []))
                        },
    SoundNotification = set_sound_value(BaseNotification, ExtraParams),

    case apns:push_notification(Pid, RegistrationId, SoundNotification, Headers) of
        {'timeout', _StreamId} ->
            lager:debug("apns notification timed out in connection. This will happen every time, so this is no cause for worry"),
            {'noreply', State};
        {200, _, _} ->
            lager:debug("apns notification sent successfully"),
            {'noreply', State}
    end;
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, #state{apns_pid=Pid}) ->
    lager:info("nv_apns terminating with reason: ~p. Destroying apns connection", [Reason]),
    apns:close_connection(Pid),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec get_true_topic(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:ne_binary().
get_true_topic(Topic, ExtraParameters) ->
    case props:get_value(<<"topic_extension">>, ExtraParameters) of
        <<"voip">> -> kz_term:to_binary(io_lib:format(<<"~s.voip">>, [Topic]));
        <<"complication">> -> kz_term:to_binary(io_lib:format(<<"~s.complication">>, [Topic]));
        'undefined' -> Topic
    end.

%% Determines whether the notification should have a vibration/sound. ExtraParams
%% should contain the key "soundless" with the value of true if the notification should not make noise
-spec set_sound_value(apns_payload(), kz_term:proplist()) -> apns_payload().
set_sound_value(Notification, ExtraParams) ->
    case props:get_value(<<"soundless">>, ExtraParams, 'false') of
        'true' -> Notification;
        'false' ->
            NewAps = maps:put('sound', props:get_value(<<"sound">>, ExtraParams, <<"default">>), maps:get('aps', Notification)),
            maps:put('aps', NewAps, Notification)
    end.
