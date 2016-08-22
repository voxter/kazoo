%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_user).

-include("callflow.hrl").

-export([handle/2
         ,get_endpoints/3
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    UserId = maybe_use_variable(Data, Call),
    Endpoints = get_endpoints(UserId, Data, Call),
    FailOnSingleReject = kz_json:get_value(<<"fail_on_single_reject">>, Data, 'undefined'),
    Timeout = kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
    Strategy = kz_json:get_binary_value(<<"strategy">>, Data, <<"simultaneous">>),
    IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),

    Command = [{<<"Application-Name">>, <<"bridge">>}
        ,{<<"Endpoints">>, Endpoints}
        ,{<<"Timeout">>, Timeout}
        ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
        ,{<<"Fail-On-Single-Reject">>, FailOnSingleReject}
        ,{<<"Dial-Endpoint-Method">>, Strategy}
        ,{<<"Ignore-Forward">>, <<"false">>}
    ],

    lager:info("attempting ~b user devices with strategy ~s", [length(Endpoints), Strategy]),
    case length(Endpoints) > 0
        andalso bridge(Command, Timeout, Call)
    of
        'false' ->
            lager:notice("user ~s has no endpoints", [UserId]),
            cf_exe:continue(Call);
        {'ok', _} ->
            lager:info("completed successful bridge to user"),
            cf_exe:stop(Call);
        {'fail', _}=Reason -> maybe_handle_bridge_failure(Reason, Call);
        {'error', _R} ->
            lager:info("error bridging to user: ~p", [_R]),
            cf_exe:continue(Call)
    end.

-spec maybe_use_variable(wh_json:object(), whapps_call:call()) -> api_binary().
maybe_use_variable(Data, Call) ->
    case wh_json:get_value(<<"var">>, Data) of
        'undefined' ->
            wh_doc:id(Data);
        Variable ->
            Value = wh_json:get_value(<<"value">>, cf_kvs_set:get_kv(Variable, Call)),
            case couch_mgr:open_cache_doc(whapps_call:account_db(Call), Value) of
                {'ok', _} -> Value;
                _ -> wh_doc:id(Data)
            end
    end.

-spec maybe_handle_bridge_failure(_, whapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case cf_util:handle_bridge_failure(Reason, Call) of
        'not_found' -> cf_exe:continue(Call);
        'ok' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the provided endpoints for the callflow and build the
%% json object used in the bridge API
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints(api_binary(), kz_json:object(), kapps_call:call()) ->
                           kz_json:objects().
get_endpoints('undefined', _, _) -> [];
get_endpoints(UserId, Data, Call) ->
    Params = kz_json:set_value(<<"source">>, ?MODULE, Data),
    kz_endpoints:by_owner_id(UserId, Params, Call).

-spec bridge(kz_proplist(), integer(), kapps_call:call()) -> kapps_api_bridge_return().
bridge(Command, Timeout, Call) ->
    kapps_call_command:send_command(Command, Call),
    kapps_call_command:b_bridge_wait(Timeout, Call).
