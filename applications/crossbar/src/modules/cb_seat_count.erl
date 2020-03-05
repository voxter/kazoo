%%%-------------------------------------------------------------------
%%% @author Cody Antcliffe <cody.antcliffe@ooma.com>
%%% @copyright (C) 2020-, Voxter Communications
%%% @doc Returns seat type counts by location
%%% @end
%%%-------------------------------------------------------------------
-module(cb_seat_count).

%% API
-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-include("crossbar.hrl").

-define(DESCENDANTS, <<"descendants">>).
-define(LOCATION_SEAT_TYPE_VIEW, <<"users/location_and_seat_type_listing">>).
-define(ACCOUNT_CONFIG, kz_json:get_json_value(<<"default">>, kapps_config_doc:config_with_defaults(<<"accounts">>))).

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.seat_count">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.seat_count">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.seat_count">>, ?MODULE, 'validate'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_seat_type_count(Context, cb_context:req_verb(Context)).

-spec validate_seat_type_count(cb_context:context(), http_method()) -> cb_context:context().
validate_seat_type_count(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    Descendants = cb_accounts:validate(Context, AccountId, ?DESCENDANTS),
    get_seat_types_by_location(get_account_docs(Descendants), [], Descendants).

%%------------------------------------------------------------------------------
%% @doc Prepares the response data for the request
%% @end
%%------------------------------------------------------------------------------
-spec get_seat_types_by_location(kz_term:ne_binaries(), kz_term:ne_binaries(), cb_context:context()) -> cb_context:context().
get_seat_types_by_location([], AccountDocList, Context) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, AccountDocList}
                                ,{fun cb_context:set_resp_status/2, 'success'}]);
get_seat_types_by_location([AccountDoc | T], AccountDocList, Context) ->
    AccountId = kz_doc:id(AccountDoc),
    LocationSummary = cb_context:resp_data(load_location_summary(prepare_context(AccountId, Context))),
    AccountDoc1 = kz_json:set_value(<<"locations">>, kz_json:new(), AccountDoc),
    case location_to_account(LocationSummary, AccountDoc1) of
        {'ok', NewAccountDoc} -> get_seat_types_by_location(T, [NewAccountDoc | AccountDocList], Context);
        'error' -> crossbar_util:response('error', <<"unable to determine default account location">>, 500, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Adds the 'locations' field with seat type counts to each account doc
%% @end
%%------------------------------------------------------------------------------
-spec location_to_account(kz_term:ne_binaries(), kz_json:object()) -> {'ok', kz_json:object()} | 'error'.
location_to_account([], AccountDoc) ->
    {'ok', AccountDoc};
location_to_account([LocationSummary | LocationSummaryList], AccountDoc) ->
    case kz_json:get_binary_value([<<"key">>, <<"location">>], LocationSummary) of
        'undefined' ->
            Location = kz_json:get_binary_value([<<"locations">>, <<"default_location">>, <<"id">>]
                                               ,?ACCOUNT_CONFIG
                                               ),
            set_location(Location, [LocationSummary | LocationSummaryList], AccountDoc);
        Location -> set_location(Location, [LocationSummary | LocationSummaryList], AccountDoc)
    end.

%%------------------------------------------------------------------------------
%% @doc Adds the 'locations' field with seat type counts to each account doc
%% @end
%%------------------------------------------------------------------------------
-spec set_location(kz_term:ne_binary(), kz_term:ne_binaries(), kz_json:object()) -> {'ok', kz_json:object()} | 'error'.
set_location('undefined', _LocationSummaryList, _AccountDoc) ->
    'error';
set_location(Location, [LocationSummary | LocationSummaryList], AccountDoc) ->
    SeatType = kz_json:get_binary_value([<<"key">>, <<"seat_type">>], LocationSummary),
    Count = kz_json:get_integer_value(<<"value">>, LocationSummary, 0),
    AccountDoc1 = kz_json:set_value([<<"locations">>, Location, SeatType], Count, AccountDoc),
    location_to_account(LocationSummaryList, AccountDoc1).

%%------------------------------------------------------------------------------
%% @doc Returns counts of seat types by location
%% @end
%%------------------------------------------------------------------------------
-spec load_location_summary(cb_context:context()) -> kz_json:objects().
load_location_summary(Context) ->
    ViewOptions = ['group'],
    crossbar_doc:load_view(?LOCATION_SEAT_TYPE_VIEW, ViewOptions, Context).

%%------------------------------------------------------------------------------
%% @doc Prepares context for DB lookups on different accounts
%% @end
%%------------------------------------------------------------------------------
-spec prepare_context(kz_term:api_ne_binary(), cb_context:context()) -> cb_context:context().
prepare_context('undefined', Context) ->
    cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB);
prepare_context(AccountId, Context) ->
    AccountDb = kz_util:format_account_db(AccountId),
    prepare_context(Context, AccountId, AccountDb).

-spec prepare_context(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
prepare_context(Context, AccountId, AccountDb) ->
    cb_context:setters(Context, [{fun cb_context:set_account_db/2, AccountDb}
                                ,{fun cb_context:set_account_id/2, AccountId}
                                ]).

%%------------------------------------------------------------------------------
%% @doc Gets the list of account docs to use in response.
%% If there is no 'next_start_key' field present, add the account doc for the account_id used for the request.
%% @end
%%------------------------------------------------------------------------------
-spec get_account_docs(cb_context:context()) -> cb_context:context().
get_account_docs(Context) ->
    case kz_json:get_binary_value(<<"next_start_key">>, cb_context:resp_envelope(Context)) of
        'undefined' -> [get_account_doc(Context, cb_context:account_id(Context))|cb_context:resp_data(Context)];
        _NextStartKey -> cb_context:resp_data(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Gets json object for a given account id
%% @end
%%------------------------------------------------------------------------------
-spec get_account_doc(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
get_account_doc(Context, AccountId) ->
    AccountDoc = cb_accounts:validate(Context, AccountId),
    kz_json:set_values([{<<"name">>, kzd_accounts:name(cb_context:resp_data(AccountDoc))}
                       ,{<<"realm">>, kzd_accounts:realm(cb_context:resp_data(AccountDoc))}
                       ,{<<"tree">>, kzd_accounts:tree(cb_context:doc(AccountDoc))}
                       ,{<<"flags">>, kzd_accounts:flags(cb_context:resp_data(AccountDoc), [])}
                       ,{<<"id">>, AccountId}]
                      ,kz_json:new()
                      ).
