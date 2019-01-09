%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Functions shared between crossbar modules
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_modules_util).

-export([pass_hashes/2
        ,get_devices_owned_by/2
        ,cavs_from_context/1
        ,is_parent_account/2

        ,attachment_name/2
        ,parse_media_type/1

        ,bucket_name/1
        ,token_cost/1, token_cost/2, token_cost/3
        ,bind/2

        ,take_sync_field/1

        ,remove_plaintext_password/1

        ,validate_number_ownership/2
        ,apply_assignment_updates/2
        ,log_assignment_updates/1

        ,normalize_media_upload/5

        ,update_voicemail_creds/4
        ,should_update_voicemail_creds/2

        ,get_request_action/1
        ,normalize_alphanum_name/1

        ,used_mac_address_message/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(PIN_PASS_SYNC(AccountId), kapps_account_config:get_global(AccountId
                                                                 ,<<"voicemail">>
                                                                 ,<<"pin_pass_sync">>
                                                                 ,'false'
                                                                 )).

-type binding() :: {kz_term:ne_binary(), atom()}.
-type bindings() :: [binding(),...].
-spec bind(atom(), bindings()) -> 'ok'.
bind(Module, Bindings) ->
    _ = [crossbar_bindings:bind(Binding, Module, Function)
         || {Binding, Function} <- Bindings
        ],
    'ok'.

-spec pass_hashes(kz_term:ne_binary(), kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
pass_hashes(Username, Password) ->
    Creds = list_to_binary([Username, ":", Password]),
    SHA1 = kz_term:to_hex_binary(crypto:hash('sha', Creds)),
    MD5 = kz_term:to_hex_binary(crypto:hash('md5', Creds)),
    {MD5, SHA1}.

-spec get_devices_owned_by(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:objects().
get_devices_owned_by(OwnerID, DB) ->
    case kz_datamgr:get_results(DB
                               ,<<"attributes/owned">>
                               ,[{'key', [OwnerID, <<"device">>]}
                                ,'include_docs'
                                ])
    of
        {'ok', JObjs} ->
            lager:debug("found ~b devices owned by ~s", [length(JObjs), OwnerID]),
            [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to fetch devices: ~p", [_R]),
            []
    end.

-spec cavs_from_context(cb_context:context()) -> kz_term:proplist().
cavs_from_context(Context) ->
    ReqData = cb_context:req_data(Context),
    QueryString = cb_context:query_string(Context),
    cavs_from_request(ReqData, QueryString).

-spec cavs_from_request(kz_json:object(), kz_json:object()) -> kz_term:proplist().
cavs_from_request(ReqData, QueryString) ->
    CAVs = kz_json:get_json_value(<<"custom_application_vars">>, ReqData, kz_json:new()),
    kapps_call_util:filter_ccvs(kz_json:merge(CAVs, QueryString)).

%%------------------------------------------------------------------------------
%% @doc Returns true if Account1 is a parent account of Account2
%% @end
%%------------------------------------------------------------------------------
-spec is_parent_account(kz_term:ne_binary() | cb_context:context(), kz_term:ne_binary()) -> boolean().
is_parent_account(<<_/binary>> = Account1, Account2) ->
    {'ok', JObj} = kzd_accounts:fetch(Account2),
    lists:member(Account1, kzd_accounts:tree(JObj));
is_parent_account(Context, Account2) ->
    is_parent_account(cb_context:auth_account_id(Context), Account2).

%%------------------------------------------------------------------------------
%% @doc Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%------------------------------------------------------------------------------
-spec attachment_name(binary(), kz_term:text()) -> kz_term:ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun(A) ->
                          case kz_term:is_empty(A) of
                              'true' -> kz_term:to_hex_binary(crypto:strong_rand_bytes(16));
                              'false' -> A
                          end
                  end
                 ,fun(A) ->
                          case kz_term:is_empty(filename:extension(A)) of
                              'false' -> A;
                              'true' ->
                                  <<A/binary, ".", (kz_mime:to_extension(CT))/binary>>
                          end
                  end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, Filename, Generators).

-spec parse_media_type(kz_term:ne_binary()) -> media_values() |
                                               {'error', 'badarg'}.
parse_media_type(MediaType) ->
    try cow_http_hd:parse_accept(MediaType)
    catch
        _E:_R ->
            lager:debug("failed to parse ~p: ~s: ~p", [MediaType, _E, _R]),
            {'error', 'badarg'}
    end.

-spec bucket_name(cb_context:context()) -> kz_term:ne_binary().
bucket_name(Context) ->
    bucket_name(cb_context:client_ip(Context), cb_context:account_id(Context)).

-spec bucket_name(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
bucket_name('undefined', 'undefined') ->
    <<"no_ip/no_account">>;
bucket_name(IP, 'undefined') ->
    <<IP/binary, "/no_account">>;
bucket_name('undefined', AccountId) ->
    <<"no_ip/", AccountId/binary>>;
bucket_name(IP, AccountId) ->
    <<IP/binary, "/", AccountId/binary>>.

-spec token_cost(cb_context:context()) -> non_neg_integer().
token_cost(Context) ->
    token_cost(Context, 1).

-spec token_cost(cb_context:context(), non_neg_integer() | kz_json:path()) -> non_neg_integer().
token_cost(Context, <<_/binary>> = Suffix) ->
    token_cost(Context, 1, [Suffix]);
token_cost(Context, [_|_]=Suffix) ->
    token_cost(Context, 1, Suffix);
token_cost(Context, Default) ->
    token_cost(Context, Default, []).

-spec token_cost(cb_context:context(), non_neg_integer(), kz_json:path()) -> non_neg_integer().
token_cost(Context, Default, Suffix) when is_integer(Default), Default >= 0 ->
    Costs = kapps_config:get(?CONFIG_CAT, <<"token_costs">>, 1),
    find_token_cost(Costs
                   ,Default
                   ,Suffix
                   ,cb_context:req_nouns(Context)
                   ,cb_context:req_verb(Context)
                   ,cb_context:account_id(Context)
                   ).

-spec find_token_cost(kz_json:object() | non_neg_integer()
                     ,Default
                     ,kz_json:path()
                     ,req_nouns()
                     ,http_method()
                     ,kz_term:api_ne_binary()
                     ) ->
                             integer() | Default.
find_token_cost(N, _Default, _Suffix, _Nouns, _ReqVerb, _AccountId) when is_integer(N) ->
    lager:debug("flat token cost of ~p configured", [N]),
    N;
find_token_cost(JObj, Default, Suffix, [{Endpoint, _} | _], ReqVerb, 'undefined') ->
    Keys = [[Endpoint, ReqVerb | Suffix]
           ,[Endpoint | Suffix]
           ],
    get_token_cost(JObj, Default, Keys);
find_token_cost(JObj, Default, Suffix, [{Endpoint, _}|_], ReqVerb, AccountId) ->
    Keys = [[AccountId, Endpoint, ReqVerb | Suffix]
           ,[AccountId, Endpoint | Suffix]
           ,[AccountId | Suffix]
           ,[Endpoint, ReqVerb | Suffix]
           ,[Endpoint | Suffix]
           ],
    get_token_cost(JObj, Default, Keys).

-spec get_token_cost(kz_json:object(), Default, kz_json:paths()) ->
                            integer() | Default.
get_token_cost(JObj, Default, Keys) ->
    case kz_json:get_first_defined(Keys, JObj) of
        'undefined' -> Default;
        V -> kz_term:to_integer(V)
    end.

-spec take_sync_field(cb_context:context()) -> cb_context:context().
take_sync_field(Context) ->
    Doc = cb_context:doc(Context),
    ShouldSync = kz_json:is_true(<<"sync">>, Doc, 'false'),
    CleansedDoc = kz_json:delete_key(<<"sync">>, Doc),
    cb_context:setters(Context, [{fun cb_context:store/3, 'sync', ShouldSync}
                                ,{fun cb_context:set_doc/2, CleansedDoc}
                                ]).

-spec remove_plaintext_password(cb_context:context()) -> cb_context:context().
remove_plaintext_password(Context) ->
    Doc = kz_json:delete_keys([<<"password">>
                              ,<<"confirm_password">>
                              ]
                             ,cb_context:doc(Context)
                             ),
    cb_context:set_doc(Context, Doc).

-spec validate_number_ownership(kz_term:ne_binaries(), cb_context:context()) ->
                                       cb_context:context().
validate_number_ownership(Numbers, Context) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}],
    #{ko := KOs} = knm_numbers:get(Numbers, Options),
    case maps:fold(fun validate_number_ownership_fold/3, [], KOs) of
        [] -> Context;
        Unauthorized ->
            Prefix = <<"unauthorized to use ">>,
            NumbersStr = kz_binary:join(Unauthorized, <<", ">>),
            Message = <<Prefix/binary, NumbersStr/binary>>,
            cb_context:add_system_error(403, 'forbidden', Message, Context)
    end.

-spec validate_number_ownership_fold(knm_numbers:num(), knm_numbers:ko(), kz_term:ne_binaries()) ->
                                            kz_term:ne_binaries().
validate_number_ownership_fold(_, Reason, Unauthorized) when is_atom(Reason) ->
    %% Ignoring atom reasons, i.e. 'not_found' or 'not_reconcilable'
    Unauthorized;
validate_number_ownership_fold(Number, ReasonJObj, Unauthorized) ->
    case knm_errors:error(ReasonJObj) of
        <<"forbidden">> -> [Number|Unauthorized];
        _ -> Unauthorized
    end.

-type assignment_to_apply() :: {kz_term:ne_binary(), kz_term:api_binary()}.
-type assignments_to_apply() :: [assignment_to_apply()].
-type port_req_assignment() :: {kz_term:ne_binary(), kz_term:api_binary(), kz_json:object()}.
-type port_req_assignments() :: [port_req_assignment()].
-type assignment_update() :: {kz_term:ne_binary(), knm_number:knm_number_return()} |
                             {kz_term:ne_binary(), {'ok', kz_json:object()}} |
                             {kz_term:ne_binary(), {'error', any()}}.
-type assignment_updates() :: [assignment_update()].

-spec apply_assignment_updates(assignments_to_apply(), cb_context:context()) ->
                                      assignment_updates().
apply_assignment_updates(Updates, Context) ->
    AccountId = cb_context:account_id(Context),
    {PRUpdates, NumUpdates} = lists:foldl(fun split_port_requests/2, {[], []}, Updates),
    PortAssignResults = assign_to_port_number(PRUpdates),
    AssignResults = maybe_assign_to_app(NumUpdates, AccountId),
    PortAssignResults ++ AssignResults.

%%------------------------------------------------------------------------------
%% @doc Split a list of assignment updates into a 2-element tuple; element
%% 1 is a list of port requests, element 2 is a list of numbers that
%% are already active.
%%
%% @end
%%------------------------------------------------------------------------------
-spec split_port_requests(assignment_to_apply(), {port_req_assignments(), assignments_to_apply()}) ->
                                 {port_req_assignments(), assignments_to_apply()}.
split_port_requests({DID, Assign}=ToApply, {PRUpdates, NumUpdates}) ->
    Num = knm_converters:normalize(DID),
    case knm_port_request:get(Num) of
        {'ok', JObj} ->
            {[{Num, Assign, JObj}|PRUpdates], NumUpdates};
        {'error', _} ->
            {PRUpdates, [ToApply|NumUpdates]}
    end.

-spec assign_to_port_number(port_req_assignments()) ->
                                   assignment_updates().
assign_to_port_number(PRUpdates) ->
    [{Num, knm_port_request:assign_to_app(Num, Assign, JObj)}
     || {Num, Assign, JObj} <- PRUpdates
    ].

-spec maybe_assign_to_app(assignments_to_apply(), kz_term:ne_binary()) ->
                                 assignment_updates().
maybe_assign_to_app(NumUpdates, AccountId) ->
    Options = [{'auth_by', AccountId}],
    Groups = group_by_assign_to(NumUpdates),
    maps:fold(fun(Assign, Nums, Acc) ->
                      Results = knm_numbers:assign_to_app(Nums, Assign, Options),
                      format_assignment_results(Results) ++ Acc
              end, [], Groups).

-type assign_to_groups() :: #{kz_term:api_binary() => kz_term:ne_binaries()}.

-spec group_by_assign_to(assignments_to_apply()) -> assign_to_groups().
group_by_assign_to(NumUpdates) ->
    group_by_assign_to(NumUpdates, #{}).

group_by_assign_to([], Groups) -> Groups;
group_by_assign_to([{DID, Assign}|NumUpdates], Groups) ->
    DIDs = maps:get(Assign, Groups, []),
    Groups1 = Groups#{Assign => [DID|DIDs]},
    group_by_assign_to(NumUpdates, Groups1).

-spec format_assignment_results(knm_numbers:ret()) -> assignment_updates().
format_assignment_results(#{ok := OKs
                           ,ko := KOs}) ->
    format_assignment_oks(OKs) ++ format_assignment_kos(KOs).

-spec format_assignment_oks(knm_number:knm_numbers()) -> assignment_updates().
format_assignment_oks(Numbers) ->
    [{knm_phone_number:number(PN), {'ok', Number}}
     || Number <- Numbers,
        PN <- [knm_number:phone_number(Number)]
    ].

-spec format_assignment_kos(knm_numbers:kos()) -> assignment_updates().
format_assignment_kos(KOs) ->
    maps:fold(fun format_assignment_kos_fold/3, [], KOs).

-spec format_assignment_kos_fold(knm_numbers:num(), knm_numbers:ko(), assignment_updates()) ->
                                        assignment_updates().
format_assignment_kos_fold(Number, Reason, Updates) when is_atom(Reason) ->
    [{Number, {'error', Reason}} | Updates];
format_assignment_kos_fold(Number, ReasonJObj, Updates) ->
    [{Number, {'error', ReasonJObj}} | Updates].

-spec log_assignment_updates(assignment_updates()) -> 'ok'.
log_assignment_updates(Updates) ->
    lists:foreach(fun log_assignment_update/1, Updates).

-spec log_assignment_update(assignment_update()) -> 'ok'.
log_assignment_update({DID, {'ok', _Number}}) ->
    lager:debug("successfully updated ~s", [DID]);
log_assignment_update({DID, {'error', E}}) ->
    lager:debug("failed to update ~s: ~p", [DID, E]).

-spec normalize_media_upload(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_media_util:normalization_options()) ->
                                    {cb_context:context(), kz_json:object()}.
normalize_media_upload(Context, FromExt, ToExt, FileJObj, NormalizeOptions) ->
    NormalizedResult = kz_media_util:normalize_media(FromExt
                                                    ,ToExt
                                                    ,kz_json:get_binary_value(<<"contents">>, FileJObj)
                                                    ,NormalizeOptions
                                                    ),
    handle_normalized_upload(Context, FileJObj, ToExt, NormalizedResult).

-spec handle_normalized_upload(cb_context:context(), kz_json:object(), kz_term:ne_binary(), kz_media_util:normalized_media()) ->
                                      {cb_context:context(), kz_json:object()}.
handle_normalized_upload(Context, FileJObj, ToExt, {'ok', Contents}) ->
    lager:debug("successfully normalized to ~s", [ToExt]),
    {Major, Minor, _} = cow_mimetypes:all(<<"foo.", (ToExt)/binary>>),

    NewFileJObj = kz_json:set_values([{[<<"headers">>, <<"content_type">>], <<Major/binary, "/", Minor/binary>>}
                                     ,{[<<"headers">>, <<"content_length">>], iolist_size(Contents)}
                                     ,{<<"contents">>, Contents}
                                     ]
                                    ,FileJObj
                                    ),

    UpdatedContext = cb_context:setters(Context
                                       ,[{fun cb_context:set_req_files/2, [{<<"original_media">>, FileJObj}
                                                                          ,{<<"normalized_media">>, NewFileJObj}
                                                                          ]
                                         }
                                        ,{fun cb_context:set_doc/2, kz_json:delete_key(<<"normalization_error">>, cb_context:doc(Context))}
                                        ]
                                       ),
    {UpdatedContext, NewFileJObj};
handle_normalized_upload(Context, FileJObj, ToExt, {'error', _R}) ->
    lager:warning("failed to convert to ~s: ~p", [ToExt, _R]),
    Reason = <<"failed to communicate with conversion utility">>,
    UpdatedDoc = kz_json:set_value(<<"normalization_error">>, Reason, cb_context:doc(Context)),
    UpdatedContext = cb_context:set_doc(Context, UpdatedDoc),
    {UpdatedContext, FileJObj}.

%% Before, we used cb_context:req_value/2 which searched "data" then the envelope
%% but we want "action" on the envelope to be respected for these PUTs against
%% /channels or /conferences, so we reverse the order here (just in case people are
%% only putting "action" in "data"
-spec get_request_action(cb_context:context()) -> kz_term:api_ne_binary().
get_request_action(Context) ->
    kz_json:find(<<"action">>, [cb_context:req_json(Context)
                               ,cb_context:req_data(Context)
                               ]
                ).

%% Update voicemail PIN at the same time as a password update
-spec update_voicemail_creds(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) ->
                                    'ok'.
update_voicemail_creds(UserId, Username, Password, Context) ->
    AccountId = cb_context:account_id(Context),
    case should_update_voicemail_creds(Username, AccountId) of
        'true' ->
            AccountDb = cb_context:account_db(Context),
            case maybe_matching_vmbox(AccountDb, UserId, Username) of
                'undefined' -> 'ok';
                Doc ->
                    Doc1 = kz_json:set_value(<<"pin">>, Password, Doc),
                    kz_datamgr:save_doc(AccountDb, Doc1),
                    'ok'
            end;
        'false' -> 'ok'
    end.

-spec should_update_voicemail_creds(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
should_update_voicemail_creds(Username, AccountId) ->
    case ?PIN_PASS_SYNC(AccountId) of
        'true' -> is_username_integer(Username);
        'false' -> 'false'
    end.

-spec is_username_integer(kz_term:ne_binary()) -> boolean().
is_username_integer(Username) ->
    case catch kz_term:to_integer(Username) of
        {'EXIT', _} ->
            lager:debug("username is not integer-convertible, not updating creds"),
            'false';
        _ -> 'true'
    end.

-spec maybe_matching_vmbox(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
maybe_matching_vmbox(AccountDb, UserId, Username) ->
    case kz_datamgr:get_results(AccountDb
                               ,<<"vmboxes/listing_by_mailbox">>
                               ,[{'key', kz_term:to_integer(Username)}]
                               ) of
        {'ok', [JObj]} ->
            {'ok', Doc} = kz_datamgr:open_doc(AccountDb
                                             ,kz_json:get_value(<<"id">>, JObj)),
            case kz_json:get_value(<<"owner_id">>, Doc) of
                UserId -> Doc;
                _ ->
                    lager:debug("user ~s doesn't own the matching voicemail box", [UserId]),
                    'undefined'
            end;
        {'ok', []} ->
            lager:debug("no vmbox with username matching ~s", [Username]),
            'undefined';
        {'error', E} ->
            lager:debug("error (~p) when getting listing_by_mailbox", [E]),
            'undefined'
    end.

-spec normalize_alphanum_name(kz_term:api_binary() | cb_context:context()) -> cb_context:context() | kz_term:api_binary().
normalize_alphanum_name('undefined') ->
    'undefined';
normalize_alphanum_name(Name) when is_binary(Name) ->
    re:replace(kz_term:to_lower_binary(Name), <<"[^a-z0-9]">>, <<>>, [global, {return, binary}]);
normalize_alphanum_name(Context) ->
    Doc = cb_context:doc(Context),
    Name = kz_json:get_ne_binary_value(<<"name">>, Doc),
    cb_context:set_doc(Context, kz_json:set_value(<<"pvt_alphanum_name">>, normalize_alphanum_name(Name), Doc)).

-spec used_mac_address_message(kz_term:ne_binary(), cb_context:context()) -> kz_term:ne_binary().
used_mac_address_message(MacAddress, Context) ->
    Prefix = <<"Mac address already in use">>,
    case provisioner_util:mac_in_use_by(Context, MacAddress) of
        {'error', 'unsupported'} -> Prefix;
        %% Happens if provisioner doesn't know who owns the MAC, but it's in
        %% use in the current account
        'undefined' -> Prefix;
        JObj -> used_mac_address_message(Prefix, JObj, Context)
    end.

-spec used_mac_address_message(kz_term:ne_binary(), kz_json:object(), cb_context:context()) -> kz_term:ne_binary().
used_mac_address_message(Prefix, JObj, Context) ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, JObj),
    AuthAccountId = cb_context:auth_account_id(Context),
    case kzd_accounts:is_in_account_hierarchy(AuthAccountId, AccountId, 'true') of
        'true' ->
            {'ok', Account} = kzd_accounts:fetch(AccountId),
            AccountName = kz_json:get_ne_binary_value(<<"name">>, Account),
            DeviceName = kz_json:get_ne_binary_value(<<"device_name">>, JObj),
            <<Prefix/binary, " by device \"", DeviceName/binary, "\" in account \"", AccountName/binary, "\"">>;
        'false' ->
            Prefix
    end.
