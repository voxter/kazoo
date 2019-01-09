%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_kvs_set).

-export([handle/2]).

-include("../callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    %% kvs_mode being deprecated, always doing JSON
    Data1 = kz_json:delete_key(<<"kvs_mode">>, Data),
    Data2 = kz_json:set_values([{<<"custom_application_vars">>, eval_expressions(Data1, Call)}
                               ,{<<"export">>, 'true'}
                               ]
                              ,kz_json:new()
                              ),
    cf_set_variables:handle(Data2, Call).

-spec eval_expressions(kz_json:object(), kapps_call:call()) -> kz_json:object().
eval_expressions(JObj, Call) ->
    kz_json:filtermap(fun(K, V) ->
                              case evaluate(K, V, JObj, Call) of
                                  'false' -> 'false';
                                  {'true', V1} -> {'true', {K, V1}}
                              end
                      end, JObj).

-spec evaluate(kz_json:key(), kz_json:json_term(), kz_json:object(), kapps_call:call()) -> {'true', kz_json:json_term()} | 'false'.
evaluate(_, <<"$_digits", _/binary>> = V, _, Call) ->
    case digits_ref(V) of
        {'error', ErrJObj} -> {'true', ErrJObj};
        CollectionName -> evaluate_digits(CollectionName, Call)
    end;
evaluate(K, <<"$", K/binary>>, _, _) ->
    %% Detected a ref loop, ignore key
    'false';
              evaluate(K, <<"$", Ref/binary>>, JObj, Call) ->
    case kz_json:get_value(Ref, JObj) of
        'undefined' ->
            %% Skip lookups that refer to nothing
            'false';
        V ->
            %% Update the key to its new value (also helps fix ref loops)
            JObj1 = kz_json:set_value(K, V, JObj),
            evaluate(Ref, V, JObj1, Call)
    end;
                            evaluate(K, V, JObj, Call) ->
                                  evaluate_ui(K, V, JObj, Call).

-spec evaluate_digits(kz_term:ne_binary(), kapps_call:call()) -> {'true', kz_term:binary()} | 'false'.
evaluate_digits(CollectionName, Call) ->
    case kapps_call:get_dtmf_collection(CollectionName, Call) of
        'undefined' -> 'false';
        Digits -> {'true', Digits}
    end.

-spec evaluate_ui(kz_json:key(), kz_json:json_term(), kz_json:object(), kapps_call:call()) -> {'true', kz_json:json_term()} | 'false'.
evaluate_ui(K, V, JObj, Call) ->
    case kz_json:is_json_object(V) of
        'true' -> evaluate_ui(kz_json:get_keys(V), K, V, JObj, Call);
        'false' -> {'true', V}
    end.

-spec evaluate_ui(kz_json:keys(), kz_json:key(), kz_json:object(), kz_json:object(), kapps_call:call()) -> {'true', kz_json:json_term()} | 'false'.
evaluate_ui([<<"type">>, <<"value">>], K, V, JObj, Call) ->
    V1 = kz_json:get_value(<<"value">>, V),
    evaluate(K, V1, JObj, Call);
evaluate_ui(_, _, V, _, _) ->
    {'true', V}.

-spec digits_ref(kz_term:ne_binary()) -> kz_term:ne_binary() | {'error', kz_json:object()}.
digits_ref(<<"$_digits">>) ->
    <<"default">>;
digits_ref(<<"$_digits[", CollectionName/binary>> = Ref) when byte_size(CollectionName) > 0 ->
    case binary:part(CollectionName, byte_size(CollectionName), -1) of
        <<"]">> -> binary:part(CollectionName, 0, byte_size(CollectionName) - 1);
        _ -> invalid_digits_ref_error(Ref)
    end;
digits_ref(Ref) ->
    invalid_digits_ref_error(Ref).

-spec invalid_digits_ref_error(kz_term:ne_binary()) -> {'error', kz_json:json_term()}.
invalid_digits_ref_error(Ref) ->
    Msg = "invalid kv lookup ref used",
    lager:info(Msg ++ ": ~s", [Ref]),
    {'error', kz_json:from_list([{<<"error">>, kz_term:to_binary(Msg)}
                                ,{<<"ref">>, Ref}
                                ])}.
