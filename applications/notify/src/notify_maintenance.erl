%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(notify_maintenance).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-export([check_initial_call/1]).
-export([check_initial_registration/1]).
-export([refresh/0]).

-type input_term() :: atom() | string() | ne_binary().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns wether an accounts initial call notification has been sent
%% @end
%%--------------------------------------------------------------------
-spec check_initial_call(input_term()) -> 'ok' | 'failed'.
check_initial_call(Account) when is_binary(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} ->
            case wh_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>], JObj) of
                'true' -> 'yes';
                'false' -> 'no'
            end;
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns wether the initial_registration notification has been sent
%% @end
%%--------------------------------------------------------------------
-spec check_initial_registration(input_term()) -> 'ok' | 'failed'.
check_initial_registration(Account) when is_binary(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} ->
            case wh_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>], JObj) of
                'true' -> 'yes';
                'false' -> 'no'
        end;
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R])
    end.

-spec refresh() -> ok.
refresh() ->
    couch_mgr:db_create(?WH_ACCOUNTS_DB),
    Views = [whapps_util:get_view_json(notify, <<"views/notify.json">>)],
    whapps_util:update_views(?WH_ACCOUNTS_DB, Views),
    ok.
