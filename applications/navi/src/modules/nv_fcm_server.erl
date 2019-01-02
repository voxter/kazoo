%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% @author Ben Partridge
%%% @end
%%%-----------------------------------------------------------------------------
-module(nv_fcm_server).

-behaviour(gen_server).

-include("navi.hrl").

-export([start_link/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,push/3,push/4
        ]).

-type platform() :: 'android' | 'ios'.

-record(state, {name :: atom()
               ,platform :: platform()
               }).
-type state() :: #state{}.

-spec start_link(kz_json:object()) -> kz_types:startlink_ret().
start_link(ServerConfig) ->
    gen_server:start_link(?MODULE, [ServerConfig], []).

-spec init(any()) -> {'ok', state()}.
init([ServerConfig]) ->
    kz_util:put_callid(?MODULE),
    AppName = kz_json:get_value(<<"app_name">>, ServerConfig),
    Platform = kz_json:get_atom_value(<<"platform">>, ServerConfig, 'android'),
    Name = kz_term:to_atom(kz_term:to_binary(io_lib:format("nv_fcm_~s_~s_srv", [AppName, Platform])), 'true'),
    %% Start the fcm server and store its name in state
    fcm:start_link(Name, kz_json:get_string_value(<<"api_key">>, ServerConfig)),
    lager:debug("starting fcm push notification server: ~p for platform ~p", [Name, Platform]),
    {'ok', #state{name=Name, platform=Platform}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'push', {RegistrationId, Message, Parameters}}, #state{name=Name, platform=Platform}=State) ->
    lager:debug("Received request to push notification into fcm for ~s", [Platform]),
    Notification = [{<<"body">>, Message}
                   ,{<<"title">>, props:get_ne_binary_value(<<"title">>, Parameters, <<"Notification">>)}
                   ,{<<"sound">>, <<"default">>}
                   ],
    %% Undocumented by Google, but PhonegapPluginPush says to include the notification fields under data instead
    %% of notification. This means we actually get the on('notification') event when the user clicks
    %% on the notification.
    Data = props:set_values(Notification, kz_json:to_proplist(props:get_value(<<"metadata">>, Parameters, kz_json:new()))),
    Payload = [{<<"data">>, Data}],
    case Platform of
        'android' ->
            fcm:push(Name, RegistrationId, Payload);
        'ios' ->
            %% On iOS we need the notification field to be present to treat it as a notification
            %% rather than a background notification
            fcm:push(Name, RegistrationId, props:set_value(<<"notification">>, Notification, Payload))
    end,
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Request, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{}) ->
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.


%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec push(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
push(Srv, RegistrationId, Message) ->
    push(Srv, RegistrationId, Message, []).
-spec push(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> any().
push(Srv, RegistrationId, Message, Parameters) ->
    lager:debug("fcm module casting push request to fcm server: ~p", [Srv]),
    gen_server:cast(Srv, {'push', {RegistrationId, Message, Parameters}}).
