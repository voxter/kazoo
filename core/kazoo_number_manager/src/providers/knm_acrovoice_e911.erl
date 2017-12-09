%%%-------------------------------------------------------------------
%%% @copyright (C) 2017 Voxter Communications
%%% @doc
%%%
%%% Handle e911 provisioning
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(knm_acrovoice_e911).
-behaviour(knm_gen_provider).

-export([save/1
        ,delete/1
        ]).

-include("knm.hrl").

-define(MOD_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".acrovoice">>).
-define(RESELLER_ID(AccountId)
       ,kapps_account_config:get(AccountId, ?MOD_CONFIG_CAT, <<"reseller_id">>
                                ,kapps_config:get_binary(?MOD_CONFIG_CAT, <<"reseller_id">>, <<>>))).
-define(API_KEY(AccountId)
       ,kapps_account_config:get(AccountId, ?MOD_CONFIG_CAT, <<"api_key">>
                                ,kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"api_key">>))).

-define(T(Binary, Length), trim(Binary, Length)).

-define(BASE_URL, <<"https://update911.info/api3/">>).
-define(RESELLER_URL(AccountId), <<(?BASE_URL)/binary, (?RESELLER_ID(AccountId))/binary>>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    maybe_update_e911(Number);
save(Number, _State) ->
    delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    case feature(Number) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information"),
            case remove_number(Number) of
                'ok' ->
                    knm_services:deactivate_feature(Number, ?FEATURE_E911);
                {'error', E} ->
                    lager:error("information delete failed: ~p", [E]),
                    knm_errors:unspecified(E, Number)
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?FEATURE_E911).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_e911(knm_number:knm_number()) -> knm_number:knm_number().
-spec maybe_update_e911(knm_number:knm_number(), boolean()) -> knm_number:knm_number().
maybe_update_e911(Number) ->
    IsDryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    maybe_update_e911(Number, IsDryRun).

maybe_update_e911(Number, 'true') ->
    CurrentE911 = feature(Number),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("dry run: information has been removed, updating upstream"),
            knm_services:deactivate_feature(Number, ?FEATURE_E911);
        'false' when NotChanged  ->
            Number;
        'false' ->
            lager:debug("dry run: information has been changed: ~s", [kz_json:encode(E911)]),
            knm_services:activate_feature(Number, {?FEATURE_E911, E911})
    end;

maybe_update_e911(Number, 'false') ->
    CurrentE911 = feature(Number),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case empty_jobj(E911) of
        'true' ->
            lager:debug("information has been removed, updating upstream"),
            case remove_number(Number) of
                'ok' ->
                    knm_services:deactivate_feature(Number, ?FEATURE_E911);
                {'error', E} ->
                    lager:error("information delete failed: ~p", [E]),
                    knm_errors:unspecified(E, Number)
            end;
        'false' when NotChanged  ->
            Number;
        'false' ->
            lager:debug("information has been changed: ~s", [kz_json:encode(E911)]),
            case update_e911(Number, E911) of
                'ok' ->
                    knm_services:activate_feature(Number, {?FEATURE_E911, E911});
                {'error', E} ->
                    lager:error("information update failed: ~p", [E]),
                    knm_errors:unspecified(E, Number)
            end
    end.

-spec update_e911(knm_number:knm_number(), kz_json:object()) ->
                         'ok' | {'error', any()}.
-spec update_e911(knm_phone_number:knm_phone_number(), kz_json:object(), ne_binary()) ->
                         'ok' | {'error', any()}.
update_e911(Number, E911) ->
    PN = knm_number:phone_number(Number),
    DID = knm_phone_number:number(PN),

    case supported_classification(DID) of
        'true' -> update_e911(PN, E911, knm_converters:to_npan(DID));
        'false' -> {'error', 'unsupported_classification'}
    end.

update_e911(PN, E911, NPAN) ->
    AccountId = knm_phone_number:assigned_to(PN),

    case stamp_and_signature(NPAN, ?API_KEY(AccountId)) of
        {Stamp, Signature} ->
            do_update_e911(E911, AccountId, NPAN, Stamp, Signature);
        'undefined' -> {'error', 'no_auth_signature'}
    end.

-spec do_update_e911(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                            'ok' | {'error', any()}.
do_update_e911(E911, AccountId, NPAN, Stamp, Signature) ->
    try
        Props = [{<<"action">>, <<"update">>}
                ,{<<"dn">>, NPAN}
                ,{<<"stamp">>, Stamp}
                ,{<<"signature">>, Signature}
                 | address_fields(E911)
                ],
        Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
        Body = iolist_to_binary(kz_http_util:props_to_querystring(Props)),
        case kz_http:post(?RESELLER_URL(AccountId), Headers, Body) of
            {'ok', Code, _, RespData} -> handle_update_resp(Code, RespData);
            {'error', _}=E -> E
        end
    catch
        throw:TE -> {'error', TE}
    end.

-spec handle_update_resp(pos_integer(), text()) -> 'ok' | {'error', any()}.
handle_update_resp(200, RespData) ->
    case kz_http_util:parse_query_string(RespData) of
        [{<<"result">>, <<"ok">>}] -> 'ok';
        Props -> handle_update_resp_error(Props)
    end;
handle_update_resp(RespCode, RespData) ->
    {'error', <<(kz_term:to_binary(RespCode))/binary, ": ", RespData/binary>>}.

-spec handle_update_resp_error(kz_proplist()) -> {'error', any()}.
-spec handle_update_resp_error(binary(), binary(), kz_proplist()) -> {'error', any()}.
handle_update_resp_error(Props) ->
    Result = props:get_binary_value(<<"result">>, Props, <<>>),
    Error = props:get_binary_value(<<"error">>, Props, <<>>),
    handle_update_resp_error(Result, Error, Props).

handle_update_resp_error(<<"error">>, <<"addressvalidation">>, Props) ->
    case props:get_integer_value(<<"alternatives">>, Props) of
        0 -> {'error', <<"Invalid address">>};
        Count -> handle_update_resp_alternatives(Props, Count)
    end;
handle_update_resp_error(<<"error">>, Error, _) ->
    {'error', Error}.

-spec handle_update_resp_alternatives(kz_proplist(), non_neg_integer()) ->
                                             {'error', kz_json:object()}.
-spec handle_update_resp_alternatives(kz_proplist(), non_neg_integer(), binaries()) ->
                                             binaries().
handle_update_resp_alternatives(Props, Count) ->
    Alternatives = handle_update_resp_alternatives(Props, Count, []),
    {'error', <<"Invalid address, did you mean \""
                ,(kz_binary:join(Alternatives, <<"\" or \"">>))/binary
                ,"\"?"
              >>}.

handle_update_resp_alternatives(_, 0, Alternatives) -> Alternatives;
handle_update_resp_alternatives(Props, Count, Alternatives) ->
    Prefix = <<"alternative", (kz_term:to_binary(Count-1))/binary>>,
    StreetNum = props:get_binary_value(<<Prefix/binary, "_streetNum">>, Props, <<>>),
    StreetName = props:get_binary_value(<<Prefix/binary, "_streetName">>, Props, <<>>),
    City = props:get_binary_value(<<Prefix/binary, "_city">>, Props, <<>>),
    Province = props:get_binary_value(<<Prefix/binary, "_province">>, Props, <<>>),
    PostalCode = props:get_binary_value(<<Prefix/binary, "_postalCode">>, Props, <<>>),

    [kz_binary:strip(<<StreetNum/binary
                       ," "
                       ,StreetName/binary
                       ,", "
                       ,City/binary
                       ,", "
                       ,Province/binary
                       ," "
                       ,PostalCode/binary
                     >>)
     | Alternatives
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_number(knm_number:knm_number()) -> 'ok' | {'error', any()}.
-spec remove_number(knm_phone_number:knm_phone_number(), ne_binary()) ->
                           'ok' | {'error', any()}.
remove_number(Number) ->
    PN = knm_number:phone_number(Number),
    DID = knm_phone_number:number(PN),

    case supported_classification(DID) of
        'true' -> remove_number(PN, knm_converters:to_npan(DID));
        'false' ->
            lager:debug("unsupported, which means it shouldn't be in acrovoice anyway"),
            'ok'
    end.

remove_number(PN, NPAN) ->
    AccountId = knm_phone_number:assigned_to(PN),

    case stamp_and_signature(NPAN, ?API_KEY(AccountId)) of
        {Stamp, Signature} ->
            do_remove_number(AccountId, NPAN, Stamp, Signature);
        'undefined' -> {'error', 'no_auth_signature'}
    end.

-spec do_remove_number(ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                              'ok' | {'error', any()}.
do_remove_number(AccountId, NPAN, Stamp, Signature) ->
    Props = [{<<"action">>, <<"delete">>}
            ,{<<"dn">>, NPAN}
            ,{<<"stamp">>, Stamp}
            ,{<<"signature">>, Signature}
            ],
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = iolist_to_binary(kz_http_util:props_to_querystring(Props)),
    case kz_http:post(?RESELLER_URL(AccountId), Headers, Body) of
        {'ok', Code, _, RespData} -> handle_delete_resp(Code, RespData);
        {'error', _}=E -> E
    end.

-spec handle_delete_resp(pos_integer(), text()) -> 'ok' | {'error', any()}.
handle_delete_resp(200, RespData) ->
    case kz_http_util:parse_query_string(RespData) of
        [{<<"result">>, <<"ok">>}] -> 'ok';
        Props -> handle_delete_resp_error(Props)
    end;
handle_delete_resp(RespCode, RespData) ->
    {'error', <<(kz_term:to_binary(RespCode))/binary, ": ", RespData/binary>>}.

-spec handle_delete_resp_error(kz_proplist()) -> {'error', any()}.
handle_delete_resp_error(Props) ->
    Error = props:get_binary_value(<<"error">>, Props, <<>>),
    {'error', Error}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parse address fields from an E911 JSON object.
%% Omitted fields:
%%   newSuiteType
%%   newOther
%%   newLanguage
%% @end
%%--------------------------------------------------------------------
-spec address_fields(kz_json:object()) -> kz_proplist().
address_fields(E911) ->
    LastName = kz_json:get_binary_value(<<"customer_name">>, E911, <<>>),
    {StreetNum, StreetName} = address_parts(kz_json:get_binary_value(<<"street_address">>, E911, <<>>)),
    Province = get_state_province_abbreviation(kz_json:get_binary_value(<<"region">>, E911, <<>>)),

    props:filter_undefined([{<<"newLastName">>, ?T(LastName, 100)}
                           ,{<<"newSuite">>, ?T(kz_json:get_binary_value(<<"extended_address">>, E911, <<>>), 30)}
                           ,{<<"newStreetNum">>, ?T(StreetNum, 10)}
                           ,{<<"newStreetName">>, ?T(StreetName, 84)}
                           ,{<<"newCity">>, ?T(kz_json:get_binary_value(<<"locality">>, E911, <<>>), 38)}
                           ,{<<"newProvince">>, ?T(Province, 2)}
                           ,{<<"newPostalCode">>, ?T(kz_json:get_binary_value(<<"postal_code">>, E911, <<>>), 11)}
                           ]).

-spec address_parts(binary()) -> {binary(), binary()}.
address_parts(Address) ->
    case binary:split(Address, <<" ">>) of
        [Address1] -> {<<>>, Address1};
        [StreetNum, StreetName] ->
            try kz_term:to_integer(StreetNum) of
                _ -> {StreetNum, StreetName}
            catch
                error:badarg -> {<<>>, StreetName}
            end
    end.

-spec get_state_province_abbreviation(ne_binary()) -> ne_binary().
get_state_province_abbreviation(StateOrProvince) ->
    States = [{<<"alabama">>, <<"AL">>}
             ,{<<"alaska">>, <<"AK">>}
             ,{<<"american samoa">>, <<"AS">>}
             ,{<<"arizona">>, <<"AZ">>}
             ,{<<"arkansas">>, <<"AR">>}
             ,{<<"california">>, <<"CA">>}
             ,{<<"colorado">>, <<"CO">>}
             ,{<<"connecticut">>, <<"CT">>}
             ,{<<"delaware">>, <<"DE">>}
             ,{<<"district of columbia">>, <<"DC">>}
             ,{<<"federated states of micronesia">>, <<"FM">>}
             ,{<<"florida">>, <<"FL">>}
             ,{<<"georgia">>, <<"GA">>}
             ,{<<"guam">>, <<"GU">>}
             ,{<<"hawaii">>, <<"HI">>}
             ,{<<"idaho">>, <<"ID">>}
             ,{<<"illinois">>, <<"IL">>}
             ,{<<"indiana">>, <<"IN">>}
             ,{<<"iowa">>, <<"IA">>}
             ,{<<"kansas">>, <<"KS">>}
             ,{<<"kentucky">>, <<"KY">>}
             ,{<<"louisiana">>, <<"LA">>}
             ,{<<"maine">>, <<"ME">>}
             ,{<<"marshall islands">>, <<"MH">>}
             ,{<<"maryland">>, <<"MD">>}
             ,{<<"massachusetts">>, <<"MA">>}
             ,{<<"michigan">>, <<"MI">>}
             ,{<<"minnesota">>, <<"MN">>}
             ,{<<"mississippi">>, <<"MS">>}
             ,{<<"missouri">>, <<"MO">>}
             ,{<<"montana">>, <<"MT">>}
             ,{<<"nebraska">>, <<"NE">>}
             ,{<<"nevada">>, <<"NV">>}
             ,{<<"new hampshire">>, <<"NH">>}
             ,{<<"new jersey">>, <<"NJ">>}
             ,{<<"new mexico">>, <<"NM">>}
             ,{<<"new york">>, <<"NY">>}
             ,{<<"north carolina">>, <<"NC">>}
             ,{<<"north dakota">>, <<"ND">>}
             ,{<<"northern mariana islands">>, <<"MP">>}
             ,{<<"ohio">>, <<"OH">>}
             ,{<<"oklahoma">>, <<"OK">>}
             ,{<<"oregon">>, <<"OR">>}
             ,{<<"palau">>, <<"PW">>}
             ,{<<"pennsylvania">>, <<"PA">>}
             ,{<<"puerto rico">>, <<"PR">>}
             ,{<<"rhode island">>, <<"RI">>}
             ,{<<"south carolina">>, <<"SC">>}
             ,{<<"south dakota">>, <<"SD">>}
             ,{<<"tennessee">>, <<"TN">>}
             ,{<<"texas">>, <<"TX">>}
             ,{<<"utah">>, <<"UT">>}
             ,{<<"vermont">>, <<"VT">>}
             ,{<<"virgin islands">>, <<"VI">>}
             ,{<<"virginia">>, <<"VA">>}
             ,{<<"washington">>, <<"WA">>}
             ,{<<"west virginia">>, <<"WV">>}
             ,{<<"wisconsin">>, <<"WI">>}
             ,{<<"wyoming">>, <<"WY">>}

             ,{<<"alberta">>, <<"AB">>}
             ,{<<"british columbia">>, <<"BC">>}
             ,{<<"manitoba">>, <<"MB">>}
             ,{<<"new brunswick">>, <<"NB">>}
             ,{<<"newfoundland">>, <<"NL">>}
             ,{<<"labrador">>, <<"NL">>}
             ,{<<"newfoundland and labrador">>, <<"NL">>}
             ,{<<"northwest territories">>, <<"NT">>}
             ,{<<"nova scotia">>, <<"NS">>}
             ,{<<"nunavut">>, <<"NU">>}
             ,{<<"ontario">>, <<"ON">>}
             ,{<<"prince edward island">>, <<"PE">>}
             ,{<<"quebec">>, <<"QC">>}
             ,{<<"saskatchewan">>, <<"SK">>}
             ,{<<"yukon">>, <<"YT">>}
             ],

    State = kz_term:to_lower_binary(StateOrProvince),
    case props:get_ne_binary_value(State, States) of
        'undefined' ->
            case lists:keymember(StateOrProvince, 2, States) of
                'true' -> StateOrProvince;
                'false' -> throw(<<"Invalid province or state">>)
            end;
        Abbreviation -> Abbreviation
    end.

-spec supported_classification(ne_binary()) -> boolean().
supported_classification(DID) ->
    case knm_converters:classify(DID) of
        <<"did_us">> -> 'true';
        <<"tollfree_us">> -> 'true';
        Classification ->
            lager:error("unsupported number classification ~s (only North American numbers are supported by acrovoice)", [Classification]),
            'false'
    end.

-spec stamp_and_signature(ne_binary(), api_binary()) ->
                                 {ne_binary(), ne_binary()} |
                                 'undefined'.
stamp_and_signature(_, 'undefined') ->
    lager:error("no api key found in configuration"),
    'undefined';
stamp_and_signature(NPAN, APIKey) ->
    {{Year, Month, Day}, {Hour, Minute, _}} = calendar:universal_time(),
    Stamp = date_string(Year, Month, Day, Hour, Minute),
    MD5 = crypto:hash('md5', <<NPAN/binary, "*", APIKey/binary, "*", Stamp/binary>>),
    Signature = << <<(list_to_binary(io_lib:format("~2.16.0b", [C])))/binary>>
                   || <<C>> <= MD5 >>,
    {Stamp, Signature}.

%%%===================================================================
%%% Internal helper functions
%%%===================================================================

-spec trim(binary(), non_neg_integer()) -> binary().
trim(Binary, Length) ->
    try binary:part(Binary, {0, Length}) of
        Part -> Part
    catch
        error:badarg -> Binary
    end.

-spec pad_time(non_neg_integer()) -> ne_binary().
pad_time(Time) when Time < 10 ->
    <<"0", (kz_term:to_binary(Time))/binary>>;
pad_time(Time) ->
    kz_term:to_binary(Time).

-spec date_string(pos_integer(), pos_integer(), pos_integer(), non_neg_integer(), non_neg_integer()) ->
                         ne_binary().
date_string(Year, Month, Day, Hour, Minute) ->
    <<(kz_term:to_binary(Year))/binary
      ,(pad_time(Month))/binary
      ,(pad_time(Day))/binary
      ,(pad_time(Hour))/binary
      ,(pad_time(Minute))/binary>>.

-spec empty_jobj(kz_json:object()) -> boolean().
empty_jobj(JObj) ->
    kz_json:is_empty(kz_json:filter(fun({_, V}) ->
                                            not kz_term:is_empty(V)
                                    end, JObj)).
