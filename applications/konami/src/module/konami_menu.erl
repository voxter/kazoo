%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(konami_menu).

-export([handle/2]).
-export([callback_event/1]).

-include("../konami.hrl").

-define(MENU_KEY_LENGTH, 1).

-record(menu_data, {retries = 3 :: non_neg_integer()
                    ,timeout = 10000 :: pos_integer()
                    ,interdigit_timeout = whapps_call_command:default_interdigit_timeout() :: pos_integer()
                    ,prompt = <<"breakout-prompt">> :: binary()
                    ,invalid_media = <<"menu-invalid_entry">> :: binary()
                    ,called_back_at_media = <<"breakout-call_back_at">> :: binary()
                    ,callback_number_correct_media = <<"breakout-number_correct">> :: binary()
                    ,enter_callback_number_media = <<"breakout-enter_callback_number">> :: binary()
                    ,callback_registered_media = <<"breakout-callback_registered">> :: binary()
                    % ,alt_moh
                    ,cf_controller_queue :: api_binary()
                    ,custom_vars = wh_json:new() :: wh_json:object()
                   }).
-type menu() :: #menu_data{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    konami_event_listener:add_call_binding(whapps_call:call_id(Call), [<<"DTMF">>]),
    whapps_call_command:flush(Call),
    whapps_call_command:hold(<<"silence_stream://0">>, Call),
    Menu = get_menu_profile(Data),
    menu_loop(Menu, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The main auto-attendant loop, will execute for the number
%% of retries playing the greeting and collecting digits till the
%% digits are routable
%% @end
%%--------------------------------------------------------------------
-spec menu_loop(menu(), whapps_call:call()) -> 'ok'.
menu_loop(#menu_data{retries=Retries}, Call) when Retries =< 0 ->
    lager:info("maximum number of retries reached"),
    whapps_call_command:flush_dtmf(Call),
    whapps_call_command:hold(Call);
menu_loop(#menu_data{retries=Retries
                        ,timeout=Timeout
                        ,interdigit_timeout=Interdigit
                        ,prompt=Prompt
                       }=Menu, Call) ->
    NoopId = whapps_call_command:prompt(Prompt, Call),

    case whapps_call_command:collect_digits(?MENU_KEY_LENGTH, Timeout, Interdigit, NoopId, Call) of
        {'ok', <<>>} ->
            lager:info("menu entry timeout"),
            menu_loop(Menu#menu_data{retries=Retries - 1}, Call);
        {'ok', Digits} ->
            %% this try_match_digits calls hunt_for_callflow() based on the digits dialed
            %% if it finds a callflow, the main CFPid will move on to it and try_match_digits
            %% will return true, matching here, and causing menu_loop to exit; this is
            %% expected behaviour as CFPid has moved on from this invocation
            case try_match_digits(Digits, Menu, Call) of
                'return' ->
                    menu_loop(Menu#menu_data{retries=3}, Call);
                'exit' ->
                    menu_loop(Menu#menu_data{retries=0}, Call);
                'callback_registered' ->
                    lager:debug("Successful breakout menu exec"),
                    konami_event_listener:rm_call_binding(whapps_call:call_id(Call), <<"DTMF">>);
                {'error', 'invalid_entry'} ->
                    lager:info("invalid selection ~p", [Digits]),
                    whapps_call_command:prompt(<<"menu-invalid_entry">>, Call),
                    menu_loop(Menu#menu_data{retries=Retries - 1}, Call);
                {'error', E} ->
                    lager:debug("breakout menu error: ~s", [E])
            end;
        {'error', _} ->
            lager:info("caller hungup while in the menu")
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The primary sequence logic to route the collected digits
%% @end
%%--------------------------------------------------------------------
-spec try_match_digits(ne_binary(), menu(), whapps_call:call()) ->
    'return' | 'exit' | 'callback_registered' | {'error', atom()}.
try_match_digits(Digits, Menu, Call) ->
    lager:info("trying to match digits ~s", [Digits]),

    case Digits of
        <<"1">> ->
            From = whapps_call:from_user(Call),
            callback_loop(Call, Menu, From);
        % <<"2">> ->
        %     AltMoh1 = hd(Menu#cf_menu_data.alt_moh),
        %     lager:debug("Changing hold music ~p", [AltMoh1]),
        %     whapps_call_command:hold(AltMoh1, Call),
        %     'true';
        <<"2">> ->
            'exit';
        _ ->
            {'error', 'invalid_entry'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
callback_loop(Call, #menu_data{called_back_at_media=CalledBackAtMedia
                               ,callback_number_correct_media=CallbackNumberCorrectMedia
                               ,enter_callback_number_media=EnterCallbackNumberMedia
                              }=Menu, From) ->
    Prompt = [{'prompt', CalledBackAtMedia}
              ,{'say', From}
              ,{'prompt', CallbackNumberCorrectMedia}
             ],
    whapps_call_command:audio_macro(Prompt, Call),

    case whapps_call_command:collect_digits(?MENU_KEY_LENGTH, Call) of
        {'ok', <<>>} ->
            {'error', 'invalid_entry'};
        {'ok', <<"1">>} ->
            register_callback(Call, Menu, From);
        {'ok', <<"2">>} ->
            whapps_call_command:prompt(EnterCallbackNumberMedia, Call),
            case whapps_call_command:collect_digits(15, 30000, 3000, Call) of
                {'ok', <<>>} ->
                    callback_loop(Call, Menu, From);
                {'ok', Digits} ->
                    callback_loop(Call, Menu, Digits);
                _ ->
                    {'error', 'collect_failed'}
            end;
        {'ok', <<"3">>} ->
            'return';
        _ ->
            {'error', 'invalid_entry'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
register_callback(Call, #menu_data{cf_controller_queue='undefined'}, _Number) ->
    whapps_call_command:tts(<<"An error occurred">>, Call),
    {'error', 'controller_queue_undefined'};
register_callback(Call, #menu_data{callback_registered_media=CallbackRegisteredMedia
                                   ,cf_controller_queue=ControllerQueue
                                   ,custom_vars=CVs
                                  }, Number) ->
    lager:debug("sending call back request to controller queue ~p", [ControllerQueue]),
    %% Notify callflow to stop operating
    {'ok', Payload} = wh_api:prepare_api_payload(wh_api:default_headers(?APP_NAME, ?APP_VERSION), callback_values(),
        fun callback_event/1),
    amqp_util:targeted_publish(ControllerQueue, Payload),

    %% Notify queue listener and FSM that CHANNEL_DESTROY should not matter
    Payload2 = [{<<"Account-ID">>, whapps_call:account_id(Call)}
               ,{<<"Queue-ID">>, wh_json:get_value(<<"Queue-ID">>, CVs)}
               ,{<<"Call-ID">>, whapps_call:call_id(Call)}
               ,{<<"Number">>, Number}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wapi_acdc_queue:publish_member_callback_reg(Payload2),

    whapps_call_command:prompt(CallbackRegisteredMedia, Call),
    whapps_call_command:queued_hangup(Call),
    'callback_registered'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
callback_values() ->
    [{<<"Event-Category">>, <<"konami">>}
     ,{<<"Event-Name">>, <<"callback_reg">>}].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec callback_event(api_terms()) ->
                         {'ok', iolist()} |
                         {'error', string()}.
callback_event(Props) when is_list(Props) ->
    case callback_event_v(Props) of
        'true' -> wh_api:build_message(Props, [], []);
        'false' -> {'error', "Proplist failed validation for callback_event"}
    end;
callback_event(JObj) ->
    callback_event(wh_json:to_proplist(JObj)).

-spec callback_event_v(api_terms()) -> boolean().
callback_event_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, [], callback_values(), []);
callback_event_v(JObj) ->
    callback_event_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_menu_profile(wh_json:object()) -> menu().
get_menu_profile(Data) ->
    Default = #menu_data{},
    #menu_data{prompt = wh_json:get_value(<<"prompt">>, Data, <<"breakout-prompt">>)
               ,invalid_media = wh_json:get_value(<<"invalid_media">>, Data, <<"menu-invalid_entry">>)
               ,called_back_at_media = wh_json:get_value(<<"called_back_at_media">>, Data, <<"breakout-call_back_at">>)
               ,callback_number_correct_media = wh_json:get_value(<<"callback_number_correct_media">>, Data, <<"breakout-number_correct">>)
               ,enter_callback_number_media = wh_json:get_value(<<"enter_callback_number_media">>, Data, <<"breakout-enter_callback_number">>)
               ,callback_registered_media = wh_json:get_value(<<"callback_registered_media">>, Data, <<"breakout-callback_registered">>)
               % ,alt_moh
               ,cf_controller_queue = wh_json:get_value(<<"controller_queue">>, Data, Default#menu_data.cf_controller_queue)
               ,custom_vars = wh_json:get_value(<<"custom_vars">>, Data, Default#menu_data.custom_vars)
              }.
