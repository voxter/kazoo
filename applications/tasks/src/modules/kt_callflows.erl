%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2018, Voxter
%%% @doc
%%% @author Ben Bradford
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_callflows).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,cleanup/2
        ]).

%% Verifiers
-export([verify_account_id/1
%%        ,verify_flow/1
        ]).

%% Appliers
-export([import/3
        ,delete/3
        ]).

-include("tasks.hrl").
-include_lib("kazoo_tasks/include/task_fields.hrl").

-define(CB_LIST_BY_NUMBER, <<"callflows/listing_by_number">>).
-define(NUMBER_SEPERATOR, <<";">>).

-define(MATCH_USER_ID(UserId)
       ,<<(UserId):32/binary>>
       ).

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".callflows">>).
-define(DB_DUMP_BULK_SIZE
       ,kapps_config:get_integer(?MOD_CAT, <<"db_page_size">>, 1000)
       ).


-define(CATEGORY, "callflows").
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
%%    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".flow">>, ?MODULE, 'verify_flow'),
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
%%    ,<<"flow">>
    ,<<"numbers">>
    ,<<"account_id">>
    ].

-spec mandatory_import_fields() -> kz_type:proplist().
mandatory_import_fields() ->
    [%%<<"flow">>
    <<"numbers">>
    ,<<"account_id">>
    ,<<"user_id">>
    ,<<"vmbox_id">>
    ].
-spec optional_import_fields() -> kz_type:proplist().
optional_import_fields() ->
    [].

-spec mandatory_delete_fields() -> kz_type:proplist().
mandatory_delete_fields() ->
    [<<"account_id">>
    ,<<"callflow_id">>
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
    #{<<"description">> => <<"Bulk-import of callflows">>
     ,<<"doc">> => <<"Bulk create callflows for the defined account id.\n"
                     "For each callflow created, return fields:\n"
                     "* `id`: doc id it is assigned to (32 alphanumeric characters).\n"
                     "* `flow`: The JSON callflow as supplied\n"
                     "* `numbers`: List of numbers for the callflow as suppliedd\n"
                     "* `account_id`: account it is assigned to (32 alphanumeric characters).\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on the affecting account.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => mandatory_import_fields()
     ,<<"optional">> => optional_import_fields()
     ,<<"return_headers">> => output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]
     };

action(<<"delete">>) ->
    #{<<"description">> => <<"Bulk-remove callflows">>
     ,<<"doc">> => <<"Forces callflows to be deleted from an account\n"
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

%%-spec verify_flow(kz_term:ne_binary()) -> boolean().
%%verify_flow(Value) ->
%%    JObj = kz_json:decode(Value),
%%    not kz_json:is_empty(JObj).

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
            validate_and_save_callflow(AccountId, Args);
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
            delete_callflow(AccountId, Args);
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
%% @doc Format the row response to be returned in the CSV
%% @end
%%------------------------------------------------------------------------------
-spec format_result(kz_tasks:args(), kz_term:ne_binary() | knm_number:knm_number(), atom()) -> kz_csv:mapped_row().
format_result(Args, Reason=?NE_BINARY, Action) ->
    Map = generate_return_values_from_args(Args, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => Reason};
format_result(Args, Doc, Action) ->
    Map = generate_return_values_from_doc(Doc, Args, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => undefined}.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the supplied csv inputs  (Error case)
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_args(map(), atom()) -> map().
generate_return_values_from_args(Args ,'import') ->
     Numbers = binary:replace(maps:get(<<"numbers">>, Args), ?NUMBER_SEPERATOR, <<",">>),
    #{<<"id">> => <<"undefined">>
%%     ,<<"flow">> => maps:get(<<"flow">>, Args)
     ,<<"numbers">> =>  Numbers
     ,<<"account_id">> => maps:get(<<"account_id">>, Args)
     };
generate_return_values_from_args(Args ,'delete') ->
    #{<<"id">> => maps:get(<<"device_id">>, Args)
%%     ,<<"flow">> => <<"undefined">>
     ,<<"numbers">> => <<"undefined">>
     ,<<"account_id">> => maps:get(<<"account_id">>, Args)
     }.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the callflow doc
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_doc(kz_doc:object(), map(), atom()) -> map().
generate_return_values_from_doc(Doc, Args, 'import') ->
     Numbers = binary:replace(maps:get(<<"numbers">>, Args), ?NUMBER_SEPERATOR, <<",">>),
    #{<<"id">> => kz_doc:id(Doc)
%%     ,<<"flow">> => kzd_callflows:flow(Doc)
     ,<<"numbers">> => Numbers
     ,<<"account_id">> => kz_doc:account_id(Doc)
     };
generate_return_values_from_doc(Doc, _Args, 'delete') ->
    #{<<"id">> => kz_doc:id(Doc)
%%     ,<<"flow">> => <<"undefined">>
     ,<<"numbers">> => <<"undefined">>
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
%% @doc Return the first account id if its in the correct format, else return the second
%% @end
%%------------------------------------------------------------------------------
-spec select_account_id(kz_type:ne_binary(), kz_type:ne_binary()) -> kz_type:ne_binary().
select_account_id(?MATCH_ACCOUNT_RAW(_)=AccountId, _) -> AccountId;
select_account_id(_, AccountId) -> AccountId.

%%------------------------------------------------------------------------------
%% @doc Verify the device passes validation and save the device to kazoo
%% @end
%%------------------------------------------------------------------------------
-spec validate_and_save_callflow(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
validate_and_save_callflow(AccountId, Args) ->
    case validate_callflow(AccountId, Args) of
        {'error', Cause} = Error ->
            lager:error("device failed validation: ~p", [Cause]),
            Error;
        'ok' ->
            prepare_and_save_callflow(AccountId, Args)
     end.

%%------------------------------------------------------------------------------
%% @doc Build callflow doc and save to kazoo storage
%% @end
%%------------------------------------------------------------------------------
-spec prepare_and_save_callflow(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
prepare_and_save_callflow(AccountId ,Args) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Doc = generate_callflow_doc(AccountId, Args),
    case kz_datamgr:save_doc(AccountDb, Doc) of
        {'error', Reason} ->
            lager:error("failed to save doc, Reason: ~p, Doc: ~p", [Reason, Doc]),
            {'error', kz_term:to_binary(Reason)};
        {'ok', _} = Ok ->
            Ok
    end.

%%------------------------------------------------------------------------------
%% @doc Validate the CSV args are valid to create a callflow
%% @end
%%------------------------------------------------------------------------------
-spec validate_callflow(kz_type:ne_binary(), kz_tasks:args()) -> 'ok' | {'error', kz_type:ne_binary()}.
validate_callflow(_AccountId, #{<<"numbers">> := <<"undefined">>}) -> 'ok';
validate_callflow(AccountId, #{<<"numbers">> := Numbers}) ->
    NumbersList = binary:split(Numbers, ?NUMBER_SEPERATOR, [global]),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case  validate_unique_numbers(AccountDb, NumbersList) of
        'true' ->
            validate_numbers_length(AccountId, NumbersList);
        'false' -> {'error', <<"numbers are unique in account">>}
    end.

-spec validate_unique_numbers(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok' | {'error', kz_type:ne_binary()}.
validate_unique_numbers(_AccountDb, []) -> 'ok';
validate_unique_numbers(AccountDb, Numbers) ->
    Options = [{'keys', Numbers}],
    case kz_datamgr:get_results(AccountDb, ?CB_LIST_BY_NUMBER, Options) of
        {'ok', []} -> 'true';
        {'ok', [_JObj]} -> 'false';
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Validate the numbers meet min length if defined
%% @end
%%------------------------------------------------------------------------------
-spec validate_numbers_length(kz_type:ne_binary(), kz_type:ne_binarys()) -> 'ok' | {'error', kz_type:ne_binary()}.
validate_numbers_length(_AccountId, []) -> 'ok';
validate_numbers_length(AccountId, Numbers) ->
    case kapps_config:get_is_true(<<"user">>, <<"enforce_min_length">>, 'false') of
        'false' -> 'ok';
        'true' ->
            MinLength = kapps_account_config:get_global(AccountId, <<"user">>, <<"min_user_length">>, 3),
            check_numbers_length(Numbers, MinLength)
    end.

-spec check_numbers_length(kz_type:ne_binarys(), integer()) -> 'ok' | {'error', kz_type:ne_binary()}.
check_numbers_length([], _MinLength) -> 'ok';
check_numbers_length([Number | Numbers], MinLength) ->
    case length(Number) of
        N when N < MinLength ->
            {'error', <<"Number ",Number/binary, " is not long enough, Min length required: ", MinLength/binary>>};
        _Else ->
            check_numbers_length(Numbers, MinLength)
    end.

%%------------------------------------------------------------------------------
%% @doc Build callflow doc from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec generate_callflow_doc(kz_type:ne_binary(), kz_tasks:args()) -> kz_doc:object().
generate_callflow_doc(AccountId, Args) ->
    %%Flow = kz_json:decode(maps:get(<<"flow">>, Args)),
    Flow = kz_json:from_list_recursive([{<<"data">>,[{<<"id">>, maps:get(<<"user_id">>, Args)}
                                                    ,{<<"delay">>, 0}
                                                    ,{<<"strategy">>, <<"simultaneous">>}
                                                    ,{<<"timeout">>, 20}
                                                    ]
                                        }
                                        ,{<<"module">>, <<"user">>}
                                        ,{<<"children">>, [{<<"_">>, [{<<"data">>, [{<<"id">>, maps:get(<<"vmbox_id">>, Args)}
                                                                                   ,{<<"action">>, <<"compose">>}
                                                                                   ,{<<"callerid_match_login">>, 'false'}
                                                                                   ,{<<"interdigit_timeout">>, 2000}
                                                                                   ,{<<"max_message_length">>, 500}
                                                                                   ,{<<"single_mailbox_login">>, 'false'}
                                                                                   ]
                                                                      }
                                                                      ,{<<"module">>, <<"voicemail">>}
                                                                      ,{<<"children">>, [{}]}
                                                                     ]
                                                          }]
                                        }
                                      ]),
    Numbers = binary:split(maps:get(<<"numbers">>, Args), ?NUMBER_SEPERATOR, [global]),
    DocFuns = [{fun kzd_callflows:set_flow/2, Flow}
              ,{fun kzd_callflows:set_numbers/2, Numbers}
              ,{fun kzd_callflows:set_patterns/2, []}
              ],
    PubDoc = lists:foldl(fun({Fun, Arg}, Doc) -> Fun(Doc, Arg) end, kzd_callflows:new(), DocFuns),
    add_private_fields(AccountId, PubDoc).

%%------------------------------------------------------------------------------
%% @doc Add private doc fields from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec add_private_fields(kz_type:ne_binary(), kz_doc:object()) -> kz_doc:object().
add_private_fields(AccountId ,Doc) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    PvtOptions = [{'type', <<"callflow">>}
                 ,{'account_id', AccountId}
                 ,{'account_db', AccountDb}
                 ,{'crossbar_doc_vsn', <<"1">>}
                 ],
    kz_doc:update_pvt_parameters(Doc, AccountDb, PvtOptions).

%%------------------------------------------------------------------------------
%% @doc Delete a callflow from kazoo
%% @end
%%------------------------------------------------------------------------------
-spec delete_callflow(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
delete_callflow(AccountId ,#{<<"callflow_id">> := CallflowId}) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:del_doc(AccountDb, CallflowId) of
        {'error', Reason} ->
            lager:error("failed to del doc, Reason: ~p, Doc id: ~p", [Reason, CallflowId]),
            {'error', kz_term:to_binary(Reason)};
        {'ok', _Doc} = Ok ->
            Ok
    end.

%%% End of Module.
