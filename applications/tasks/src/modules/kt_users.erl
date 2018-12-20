%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2018, Voxter
%%% @doc
%%% @author Ben Bradford
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_users).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,cleanup/2
        ]).

%% Verifiers
-export([verify_account_id/1
        ,verify_user_id/1
        ,verify_email/1
        ]).

%% Appliers
-export([import/3
        ,delete/3
        ]).

-include("tasks.hrl").
-include_lib("kazoo_tasks/include/task_fields.hrl").

-define(LIST_BY_USERNAME, <<"users/list_by_username">>).

-define(MATCH_USER_ID(UserId)
       ,<<(UserId):32/binary>>
       ).

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".users">>).
-define(DB_DUMP_BULK_SIZE
       ,kapps_config:get_integer(?MOD_CAT, <<"db_page_size">>, 1000)
       ).


-define(CATEGORY, "users").
-define(ACTIONS, [<<"import">>
                 ,<<"delete">>
                 ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".cleanup">>, ?MODULE, 'cleanup'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".account_id">>, ?MODULE, 'verify_account_id'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".email">>, ?MODULE, 'verify_email'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".user_id">>, ?MODULE, 'verify_user_id'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header(?NE_BINARY) ->
    result_output_header().

-spec result_output_header() -> kz_tasks:output_header().
result_output_header() ->
    {'replace', output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]}.

-spec output_header() -> kz_tasks:output_header().
output_header() ->
    [<<"id">>
    ,<<"username">>
    ,<<"first_name">>
    ,<<"last_name">>
    ,<<"account_id">>
    ].

-spec mandatory_import_fields() -> kz_type:proplist().
mandatory_import_fields() ->
    [<<"account_id">>
    ,<<"first_name">>
    ,<<"last_name">>
    ,<<"username">>
    ,<<"email">>
    ,<<"password">>
    ,<<"api_url">>
    ].
-spec optional_import_fields() -> kz_type:proplist().
optional_import_fields() ->
    [<<"timezone">>
    ].

-spec mandatory_delete_fields() -> kz_type:proplist().
mandatory_delete_fields() ->
    [<<"account_id">>
    ,<<"user_id">>
    ].
-spec optional_delete_fields() -> kz_type:proplist().
optional_delete_fields() -> [].

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_map(action(Action)), JObj).

%%%=============================================================================
%%% Actions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec action(kz_term:ne_binary()) -> map().
action(<<"import">>) ->
    #{<<"description">> => <<"Bulk-import of users">>
     ,<<"doc">> => <<"Bulk create a user for the defined account id.\n"
                     "For each user created, return fields:\n"
                     "* `id`: doc id it is assigned to (32 alphanumeric characters).\n"
                     "* `username`: The users username (Unique)\n"
                     "* `first_name`: The users first_name\n"
                     "* `last_name`: The users last_name\n"
                     "* `account_id`: account it is assigned to (32 alphanumeric characters).\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on the affecting account.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => mandatory_import_fields()
     ,<<"optional">> => optional_import_fields()
     ,<<"return_headers">> => output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]
     };

action(<<"delete">>) ->
    #{<<"description">> => <<"Bulk-remove users">>
     ,<<"doc">> => <<"Forces users to be deleted from an account\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on the affecting account.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => mandatory_delete_fields()
     ,<<"optional">> => optional_delete_fields()
     ,<<"return_headers">> => output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]
     }.

%%%=============================================================================
%%% Verifiers
%%% These are called during CSV first upload on all rows to verify the CSV data
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_account_id(kz_term:ne_binary()) -> boolean().
verify_account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
verify_account_id(_) -> 'false'.

-spec verify_user_id(kz_term:ne_binary()) -> boolean().
verify_user_id(?MATCH_USER_ID(_)) -> 'true';
verify_user_id(_) -> 'false'.

-spec verify_email(kz_term:ne_binary()) -> boolean().
verify_email(Cell) ->
    EmailRegExp = "^([a-zA-Z0-9_.]+)@([a-zA-Z0-9_.]+)\.([a-zA-Z]{2,5})$",
    case re:run(Cell, EmailRegExp) of
        {match, _} -> 'true';
        nomatch -> 'false'
    end.

%%%=============================================================================
%%% Appliers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Handle an import action
%% @end
%%------------------------------------------------------------------------------
-spec import(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), sets:set()}.
import(ExtraArgs, 'init', Args) ->
    IterValue = sets:new(),
    import(ExtraArgs, IterValue, Args);
import(#{account_id := Account
        ,auth_account_id := AuthAccountId
        }
      ,AccountIds
      ,Args=#{<<"account_id">> := AccountId0}
      ) ->
    AccountId = select_account_id(AccountId0, Account),
    Resp = case is_authorized_account(AuthAccountId, AccountId) of
        'true' ->
            validate_and_save_user(AccountId, Args);
        'false' ->
            {'error', <<"Access denied, Auth Account does not have access to account">>}
    end,
    Row = handle_result(Args, Resp, 'import'),
    {Row, sets:add_element(AccountId, AccountIds)}.

-spec delete(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), sets:set()}.
delete(ExtraArgs, 'init', Args) ->
    IterValue = sets:new(),
    delete(ExtraArgs, IterValue, Args);
delete(#{account_id := Account
        ,auth_account_id := AuthAccountId
        }
      ,AccountIds
      ,Args=#{<<"account_id">> := AccountId0}
      ) ->
    AccountId = select_account_id(AccountId0, Account),
    Resp = case is_authorized_account(AuthAccountId, AccountId) of
        'true' ->
            delete_user(AccountId, Args);
        'false' ->
            {'error', <<"Access denied, Auth Account does not have access to account">>}
    end,
    Row = handle_result(Args, Resp, 'delete'),
    {Row, sets:add_element(AccountId, AccountIds)}.


%%%=============================================================================
%%% Return value functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process the CSV row result
%% @end
%%------------------------------------------------------------------------------
-spec handle_result(kz_tasks:args(), {'ok', kz_doc:object()} | {'error', atom() | kz_type:ne_binary()}, atom()) -> kz_tasks:return().
handle_result(Args, {'ok', Doc}, Action) ->
    format_result(Args, Doc, Action);
handle_result(Args, {'error', Reason}, Action)
  when is_atom(Reason) ->
    format_result(Args, kz_term:to_binary(Reason), Action);
handle_result(Args, {'error', Reason}, Action) ->
    format_result(Args, Reason, Action).

%%------------------------------------------------------------------------------
%% @doc Format the row response to be returned to the user in the CSV
%% @end
%%------------------------------------------------------------------------------
-spec format_result(kz_tasks:args(), kz_term:ne_binary() | knm_number:knm_number(), atom()) -> kz_csv:mapped_row().
format_result(Args, Reason=?NE_BINARY, Action) ->
    Map = generate_return_values_from_args(Args, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => Reason};
format_result(_Args, Doc, Action) ->
    Map = generate_return_values_from_doc(Doc, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => undefined}.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the supplied csv inputs  (Error case)
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_args(map(), atom()) -> map().
generate_return_values_from_args(Args ,'import') ->
    #{<<"id">> => <<"undefined">>
     ,<<"username">> => maps:get(<<"username">>, Args)
     ,<<"first_name">> => maps:get(<<"first_name">>, Args)
     ,<<"last_name">> => maps:get(<<"last_name">>, Args)
     ,<<"account_id">> => maps:get(<<"account_id">>, Args)
     };
generate_return_values_from_args(Args ,'delete') ->
    #{<<"id">> => maps:get(<<"user_id">>, Args)
     ,<<"username">> => <<"undefined">>
     ,<<"first_name">> => <<"undefined">>
     ,<<"last_name">> => <<"undefined">>
     ,<<"account_id">> => maps:get(<<"account_id">>, Args)
     }.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the user doc
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_doc(kz_doc:object(), atom()) -> map().
generate_return_values_from_doc(Doc, 'import') ->
    lager:debug("generating import resp row from doc: ~p", [Doc]),
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"username">> => kzd_users:username(Doc)
     ,<<"first_name">> => kzd_user:first_name(Doc)
     ,<<"last_name">> => kzd_user:last_name(Doc)
     ,<<"account_id">> => kz_doc:account_id(Doc)
     };
generate_return_values_from_doc(Doc, 'delete') ->
    lager:debug("generating delete resp row from doc: ~p", [Doc]),
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"username">> => <<"undefined">>
     ,<<"first_name">> => <<"undefined">>
     ,<<"last_name">> => <<"undefined">>
     ,<<"account_id">> => kz_doc:account_id(Doc)
     }.


%%%=============================================================================
%%% Cleanup functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup(kz_term:ne_binary(), any()) -> any().
cleanup(<<"import">>, 'init') ->
    %% Hit if no rows at all succeeded.
    'ok';
%%cleanup(<<"import">>, AccountIds) ->
%%    F = fun (AccountId) ->
%%                lager:debug("reconciling account ~s", [AccountId]),
%%                kz_services:reconcile(AccountId, <<"user">>)
%%        end,
%%    lists:foreach(F, sets:to_list(AccountIds)),
%%    kz_datamgr:enable_change_notice();
cleanup(?NE_BINARY, _) -> 'ok'.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns true if Account1 is authorised to make changes to Account2
%% @end
%%------------------------------------------------------------------------------
-spec is_authorized_account(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_authorized_account(Account1, Account1) -> true;
is_authorized_account(Account1, Account2) -> is_parent_account(Account1, Account2).

%%------------------------------------------------------------------------------
%% @doc Returns true if Account1 is a parent account of Account2
%% @end
%%------------------------------------------------------------------------------
-spec is_parent_account(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_parent_account(<<_/binary>> = Account1, Account2) ->
    {'ok', JObj} = kzd_accounts:fetch(Account2),
    lists:member(Account1, kzd_accounts:tree(JObj)).

%%------------------------------------------------------------------------------
%% @doc Return the first account id if its in the coret format, else return the second
%% @end
%%------------------------------------------------------------------------------
-spec select_account_id(kz_type:ne_binary(), kz_type:ne_binary()) -> kz_type:ne_binary().
select_account_id(?MATCH_ACCOUNT_RAW(_)=AccountId, _) -> AccountId;
select_account_id(_, AccountId) -> AccountId.

%%------------------------------------------------------------------------------
%% @doc Verify the user passes validation and save the user to kazoo
%% @end
%%------------------------------------------------------------------------------
-spec validate_and_save_user(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
validate_and_save_user(AccountId, Args) ->
    case validate_user(AccountId, Args) of
        {'error', Cause} = Error ->
            lager:error("user failed validation: ~p", [Cause]),
            Error;
        'ok' ->
            prepare_and_save_user(AccountId, Args)
     end.

%%------------------------------------------------------------------------------
%% @doc Build User doc and save user to kazoo storage
%% @end
%%------------------------------------------------------------------------------
-spec prepare_and_save_user(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
prepare_and_save_user(AccountId ,Args) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    UserDoc = generate_user_doc(AccountId, Args),
    case kz_datamgr:save_doc(AccountDb, UserDoc) of
        {'error', Reason} ->
            lager:error("failed to save doc, Reason: ~p, Doc: ~p", [Reason, UserDoc]),
            {'error', kz_term:to_binary(Reason)};
        {'ok', _User} = Ok ->
            Ok
    end.

%%------------------------------------------------------------------------------
%% @doc Build User doc from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec generate_user_doc(kz_type:ne_binary(), kz_tasks:args()) -> kz_doc:object().
generate_user_doc(AccountId, Args) ->
    PubDoc = kz_json:from_list(lists:flatten(doc_public_fields(Args))),
    add_private_fields(AccountId, PubDoc, Args).

%%------------------------------------------------------------------------------
%% @doc Build Users public doc fields from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec doc_public_fields(kz_tasks:args()) -> kz_type:proplist().
doc_public_fields(Args) ->
    [props:filter_undefined(
        [{<<"username">>,maps:get(<<"username">>, Args)}
        ,{<<"email">>, maps:get(<<"email">>, Args)}
        ,{<<"timezone">>, maps:get(<<"timezone">>, Args)}
        ,{<<"first_name">>, maps:get(<<"first_name">>, Args)}
        ,{<<"last_name">>, maps:get(<<"last_name">>, Args)}
        ,{<<"enabled">>, <<"true">>}
        ,{<<"priv_level">>, <<"user">>}
        ,{<<"require_password_update">>, <<"false">>}
        ,{<<"verified">>, <<"false">>}
        ,{<<"vm_to_email_enabled">>, <<"true">>}
        ,{<<"fax_to_email_enabled">>, <<"true">>}
        ])
    ,apps(
        user_portal(props:filter_undefined(
            [{<<"label">>, <<"User Portal">>}
            ,{<<"icon">>, <<"userportal">>}
            ,{<<"api_url">>, maps:get(<<"api_url">>, Args)}
            ]))
         )
    ,[{<<"call_forward">>,
        {[{<<"keep_caller_id">>, <<"true">>}
        ,{<<"substitute">>, <<"false">>}
        ,{<<"direct_calls_only">>, <<"false">>}
        ,{<<"enabled">>, <<"false">>}
        ,{<<"failover">>, <<"false">>}
        ,{<<"ignore_early_media">>, <<"true">>}
        ,{<<"require_keypress">>, <<"true">>}
        ]}}]
    ,{<<"call_restriction">>, {[]}}
    ,{<<"contact_list">>, {[]}}
    ,{<<"music_on_hold">>, {[]}}
    ,{<<"ringtones">>, {[]}}
    ].

apps(Props) -> maybe_nest(<<"apps">>, Props).
user_portal(Props) -> maybe_nest(<<"user_portal">>, Props).

-spec maybe_nest(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
maybe_nest(_, []) -> [];
maybe_nest(Feature, Props) -> [{Feature, kz_json:from_list(Props)}].

%%------------------------------------------------------------------------------
%% @doc Build Users private doc fields from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec add_private_fields(kt_task:args(), kz_doc:object(), kz_type:ne_binary()) -> kz_doc:object().
add_private_fields(AccountId ,Doc ,Args) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    {MD5, SHA1} = pass_hashes(maps:get(<<"username">>, Args), maps:get(<<"password">>, Args)),
    AlphaNumName = normalize_alphanum_name(maps:get(<<"first_name">>, Args), maps:get(<<"last_name">>, Args)),
    PvtOptions = [{'type', kzd_user:type()}
                 ,{'account_id', AccountId}
                 ,{'account_db', AccountDb}
                 ,{'crossbar_doc_vsn', <<"1">>}
                 ],
    UpdatedDoc = kz_doc:update_pvt_parameters(Doc, AccountDb, PvtOptions),
    OtherPvtValues = [{<<"pvt_md5_auth">>, MD5}
                     ,{<<"pvt_sha1_auth">>, SHA1}
                     ,{<<"pvt_alphanum_name">>, AlphaNumName}
                     ],
    kz_json:set_values(OtherPvtValues, UpdatedDoc).


%%------------------------------------------------------------------------------
%% @doc Validate the CSV args are valid to create a user
%% @end
%%------------------------------------------------------------------------------
-spec validate_user(kz_type:ne_binary(), kz_tasks:args()) -> 'ok' | {'error', kz_type:ne_binary()}.
validate_user(AccountId, #{<<"username">> := Username}) ->
    validate_username(AccountId, Username).

%%------------------------------------------------------------------------------
%% @doc Validate the CSV supplied username is valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_username(kz_term:ne_binary(), kz_type:ne_binary()) -> 'ok' | {'error', kz_type:ne_binary()}.
validate_username(AccountId, Username) ->
    LowerUsername = kz_term:to_lower_binary(Username),
    case is_username_unique(AccountId, Username) of
        'true' ->
            check_username_length(AccountId, LowerUsername);
        'false' ->
            {'error', <<"Username is not unique">>}
    end.

%%------------------------------------------------------------------------------
%% @doc Validate the users username is unique
%% @end
%%------------------------------------------------------------------------------
-spec is_username_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_username_unique(AccountId, Username) ->
    ViewOptions = [{'key', Username}],
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_USERNAME, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [_JObj|_]} -> 'false';
        _Else ->
            lager:error("error ~p checking view ~p in ~p", [_Else, ?LIST_BY_USERNAME, AccountDb]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Validate the users username meets min lenght if defined
%% @end
%%------------------------------------------------------------------------------
-spec check_username_length(kz_type:ne_binary(), kz_type:ne_binary()) -> 'ok' | {'error', kz_type:ne_binary()}.
check_username_length(AccountId, Username) ->
    CheckMinLength = kapps_config:get_is_true(<<"user">>, <<"enforce_min_length">>, 'false'),
    check_username_length(AccountId, Username, CheckMinLength).

-spec check_username_length(kz_type:ne_binary(), kz_type:ne_binary(), boolean()) -> 'ok' | {'error', kz_type:ne_binary()}.
check_username_length(_AccountId, _Username, 'false') -> 'ok';
check_username_length(AccountId, Username, 'true') ->
    MinLength = kapps_account_config:get_global(AccountId, <<"user">>, <<"min_user_length">>, 3),
    case length(Username) of
        N when N < MinLength ->
            {'error', <<"Username is not long enough, Min length required: ", MinLength/binary>>};
        _Else ->
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a user from kazoo
%% @end
%%------------------------------------------------------------------------------
-spec delete_user(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
delete_user(AccountId ,#{<<"user_id">> := UserId}) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:del_doc(AccountDb, UserId) of
        {'error', Reason} ->
            lager:error("failed to del doc, Reason: ~p, Doc id: ~p", [Reason, UserId]),
            {'error', kz_term:to_binary(Reason)};
        {'ok', _UserDoc} = Ok ->
            Ok
    end.

%%------------------------------------------------------------------------------
%% @doc Generate MD5 and SHA1 hashes from a username and password
%% @end
%%------------------------------------------------------------------------------
-spec pass_hashes(kz_term:ne_binary(), kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
pass_hashes(Username, Password) ->
    Creds = list_to_binary([Username, ":", Password]),
    SHA1 = kz_term:to_hex_binary(crypto:hash('sha', Creds)),
    MD5 = kz_term:to_hex_binary(crypto:hash('md5', Creds)),
    {MD5, SHA1}.

%%------------------------------------------------------------------------------
%% @doc Generate alphanum from the users first and last name combination
%% @end
%%------------------------------------------------------------------------------
-spec normalize_alphanum_name(kz_type:ne_binary(), kz_type:ne_binary()) -> kz_type:ne_binary().
normalize_alphanum_name(<<"undefined">>, <<"undefined">>) -> normalize_alphanum_name(<<"undefined">>);
normalize_alphanum_name(FirstName, <<"undefined">>) -> normalize_alphanum_name(FirstName);
normalize_alphanum_name(<<"undefined">>, LastName) -> normalize_alphanum_name(LastName);
normalize_alphanum_name(FirstName, LastName) -> normalize_alphanum_name(<<FirstName/binary, LastName/binary>>).

-spec normalize_alphanum_name(kz_type:ne_binary()) -> kz_type:ne_binary().
normalize_alphanum_name(<<"undefined">>) ->
    <<"undefined">>;
normalize_alphanum_name(Name) ->
    re:replace(kz_term:to_lower_binary(Name), <<"[^a-z0-9]">>, <<>>, [global, {return, binary}]).

%%% End of Module.
