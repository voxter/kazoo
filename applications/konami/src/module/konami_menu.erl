%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(konami_menu).

-export([handle/2]).
-export([callback_event/1]).

-include("../konami.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".menu">>).

-record(menu_keys, {
           %% Record Review
           callback = <<"1">> :: ne_binary()
          ,change_moh = <<"2">> :: ne_binary()
         }).
-type menu_keys() :: #menu_keys{}.
-define(MENU_KEY_LENGTH, 1).

-record(cf_menu_data, {
          menu_id :: api_binary()
         ,name = <<>> :: binary()
         ,retries = 3 :: pos_integer()
         ,timeout = 10000 :: pos_integer()
         ,greeting_id :: api_binary()
         ,exit_media = 'true' :: boolean() | ne_binary()
         ,invalid_media = 'true' :: boolean() | ne_binary()
         ,keys = #menu_keys{} :: menu_keys()
         ,interdigit_timeout = whapps_call_command:default_interdigit_timeout() :: pos_integer()
         ,alt_moh = wh_json:new() :: wh_json:object()
         ,cf_controller_queue = 'undefined'
         ,custom_vars = wh_json:new()
         }).
-type menu() :: #cf_menu_data{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    konami_event_listener:add_call_binding(whapps_call:call_id(Call), [<<"DTMF">>]),
    Menu = get_menu_profile(Data, Call),
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
menu_loop(#cf_menu_data{retries=Retries}, Call) when Retries =< 0 ->
    lager:info("maxium number of retries reached"),
    whapps_call_command:flush_dtmf(Call),
    whapps_call_command:hold(Call);
menu_loop(#cf_menu_data{retries=Retries
                        ,timeout=Timeout
                        ,interdigit_timeout=Interdigit
                       }=Menu, Call) ->
    whapps_call_command:flush(Call),
    whapps_call_command:hold(<<"silence_stream://0">>, Call),
    NoopId = whapps_call_command:prompt(<<"breakout-prompt">>, Call),

    case whapps_call_command:collect_digits(?MENU_KEY_LENGTH, Timeout, Interdigit, NoopId, Call) of
        {'ok', <<>>} ->
            lager:info("menu entry timeout"),
            menu_loop(Menu#cf_menu_data{retries=Retries - 1}, Call);
        {'ok', Digits} ->
            %% this try_match_digits calls hunt_for_callflow() based on the digits dialed
            %% if it finds a callflow, the main CFPid will move on to it and try_match_digits
            %% will return true, matching here, and causing menu_loop to exit; this is
            %% expected behaviour as CFPid has moved on from this invocation
            case try_match_digits(Digits, Menu, Call) of
                'return' ->
                    menu_loop(Menu#cf_menu_data{retries=Retries - 1}, Call);
                'true' ->
                    lager:debug("Successful breakout menu exec"),
                    konami_event_listener:rm_call_binding(whapps_call:call_id(Call), <<"DTMF">>);
                'false' ->
                    lager:info("invalid selection ~p", [Digits]),
                    whapps_call_command:play(<<"menu-invalid_entry">>, Call),
                    menu_loop(Menu#cf_menu_data{retries=Retries - 1}, Call)
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
-spec try_match_digits(ne_binary(), menu(), whapps_call:call()) -> boolean().
try_match_digits(Digits, Menu, Call) ->
    lager:info("trying to match digits ~s", [Digits]),

    case Digits of
        <<"1">> ->
            From = whapps_call:from_user(Call),
            callback_loop(Call, Menu, From);
        <<"2">> ->
            AltMoh1 = hd(Menu#cf_menu_data.alt_moh),
            lager:debug("Changing hold music ~p", [AltMoh1]),
            whapps_call_command:hold(AltMoh1, Call),
            'true';
        _ ->
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
callback_loop(Call, Menu, From) ->
    Prompt = [{'prompt', <<"breakout-call_back_at">>}
              ,{'say', From}
              ,{'prompt', <<"breakout-number_correct">>}
             ],
    whapps_call_command:audio_macro(Prompt, Call),

    case whapps_call_command:collect_digits(?MENU_KEY_LENGTH, Call) of
        {'ok', <<>>} ->
            'false';
        {'ok', <<"1">>} ->
            register_callback(Call, Menu#cf_menu_data.cf_controller_queue, Menu#cf_menu_data.custom_vars, From);
        {'ok', <<"2">>} ->
            whapps_call_command:prompt(<<"breakout-enter_callback_number">>, Call),
            case whapps_call_command:collect_digits(15, 30000, 3000, Call) of
                {'ok', <<>>} ->
                    callback_loop(Call, Menu, From);
                {'ok', Digits} ->
                    callback_loop(Call, Menu, Digits);
                _ ->
                    'false'
            end;
        {'ok', <<"3">>} ->
            'return';
        _ ->
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
register_callback(Call, 'undefined', _CVs, _Number) ->
    whapps_call_command:tts(<<"An error occurred">>, Call),
    'false';
register_callback(Call, ControllerQueue, CVs, Number) ->
    lager:debug("Sending call back request to controller queue ~p", [ControllerQueue]),
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

    whapps_call_command:prompt(<<"breakout-callback_registered">>, Call),
    whapps_call_command:queued_hangup(Call),
    'true'.

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
-spec get_menu_profile(wh_json:object(), whapps_call:call()) -> menu().
get_menu_profile(Data, Call) ->
    Id = wh_json:get_value(<<"id">>, Data),
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, Id) of
        {'ok', JObj} ->
            lager:info("loaded menu route ~s", [Id]),
            Default = #cf_menu_data{},
            #cf_menu_data{menu_id = Id
                          ,name =
                              wh_json:get_ne_value(<<"name">>, JObj, Id)
                          ,retries =
                              wh_json:get_integer_value(<<"retries">>, JObj, Default#cf_menu_data.retries)
                          ,timeout =
                              wh_json:get_integer_value(<<"timeout">>, JObj, Default#cf_menu_data.timeout)
                          ,greeting_id =
                              wh_json:get_ne_value([<<"media">>, <<"greeting">>], JObj)
                          ,exit_media =
                              (not wh_json:is_false([<<"media">>, <<"exit_media">>], JObj))
                          andalso wh_json:get_ne_value([<<"media">>, <<"exit_media">>], JObj, 'true')
                          ,invalid_media =
                              (not wh_json:is_false([<<"media">>, <<"invalid_media">>], JObj))
                          andalso wh_json:get_ne_value([<<"media">>, <<"invalid_media">>], JObj, 'true')
                          ,interdigit_timeout =
                              wh_util:to_integer(
                                wh_json:find(<<"interdigit_timeout">>
                                             ,[JObj, Data]
                                             ,whapps_call_command:default_interdigit_timeout()
                                            ))
                          ,alt_moh =
                              wh_json:get_value(<<"alt_moh">>
                                                ,Data
                                                ,Default#cf_menu_data.alt_moh
                                               )
                          ,cf_controller_queue =
                              wh_json:get_value(<<"controller_queue">>
                                                ,Data
                                                ,Default#cf_menu_data.cf_controller_queue
                                               )
                          ,custom_vars =
                              wh_json:get_value(<<"custom_vars">>
                                                ,Data
                                                ,Default#cf_menu_data.custom_vars
                                               )
                         };
        {'error', R} ->
            lager:info("failed to load menu route ~s, ~w", [Id, R]),
            #cf_menu_data{}
    end.
