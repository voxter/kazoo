%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communications
%%% @doc
%%% @author Dustin Brett
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_follow_me).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-record(follow_me, {enabled = 'false' :: boolean()
                   ,jobj = kz_json:new() :: kz_json:object()
                   ,account_db :: kz_term:ne_binary()
                   }).
-type follow_me() :: #follow_me{}.

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case maybe_build_follow_me_record(Data, Call) of
        {'error', _} ->
            lager:info("unable to determine what document to apply follow-me against"),
            _ = kapps_call_command:b_prompt(<<"follow-me-not_available">>, Call),
            cf_exe:stop(Call);
        {'ok', #follow_me{}=FollowMe} ->
            _ = kapps_call_command:answer(Call),
            Action = kz_json:get_ne_binary_value(<<"action">>, Data),
            _ = maybe_execute_action(Action, FollowMe, Call),
            cf_exe:continue(Call)
    end.

-spec maybe_build_follow_me_record(kz_json:object(), kapps_call:call()) ->
                                          {'ok', follow_me()} |
                                          {'error', any()}.
maybe_build_follow_me_record(Data, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case maybe_get_data_id(AccountDb, Data, Call) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            lager:info("changing follow-me settings on document ~s", [kz_doc:id(JObj)]),
            {'ok', #follow_me{enabled=kz_json:is_true([<<"follow_me">>], JObj)
                             ,jobj=JObj
                             ,account_db=AccountDb
                             }}
    end.

-spec maybe_get_data_id(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> kz_term:jobj_return().
maybe_get_data_id(AccountDb, Data, Call) ->
    Id = kz_json:get_ne_binary_value(<<"id">>, Data),
    case maybe_get_doc(AccountDb, Id) of
        {'error', _} ->
            lager:info("follow-me feature callflow does not specify a document"),
            maybe_get_authorizing_device(AccountDb, Call);
        {'ok', _}=Ok -> Ok
    end.

-spec maybe_get_authorizing_device(kz_term:ne_binary(), kapps_call:call()) -> kz_term:jobj_return().
maybe_get_authorizing_device(AccountDb, Call) ->
    AuthorizingId = kapps_call:authorizing_id(Call),
    maybe_get_doc(AccountDb, AuthorizingId).

-spec maybe_get_doc(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:jobj_return().
maybe_get_doc(_, 'undefined') ->
    {'error', 'no_device_id'};
maybe_get_doc('undefined', _) ->
    {'error', 'no_account_db'};
maybe_get_doc(AccountDb, Id) ->
    case kz_datamgr:open_doc(AccountDb, Id) of
        {'error', _R}=E ->
            lager:info("unable to open ~s: ~p", [Id, _R]),
            E;
        {'ok', _}=Ok -> maybe_return_doc(Ok)
    end.

-spec maybe_return_doc(kz_term:jobj_return()) -> kz_term:jobj_return().
maybe_return_doc({'ok', JObj}=Ok) ->
    case kz_doc:type(JObj) of
        <<"device">> -> Ok;
        _Else ->
            lager:info("follow-me can not be applied against a doc of type ~s", [_Else]),
            {'error', 'not_found'}
    end.

-spec maybe_execute_action(kz_term:ne_binary(), follow_me(), kapps_call:call()) -> any().
maybe_execute_action(<<"activate">>, #follow_me{enabled='true'}, Call) ->
    lager:info("follow-me is already enabled on this document"),
    kapps_call_command:b_prompt(<<"follow-me-activated">>, Call);
maybe_execute_action(<<"activate">>, #follow_me{enabled='false'}=FollowMe, Call) ->
    activate_follow_me(FollowMe, Call);
maybe_execute_action(<<"deactivate">>, #follow_me{enabled='false'}, Call) ->
    lager:info("follow-me is already disabled on this document"),
    kapps_call_command:b_prompt(<<"follow-me-deactivated">>, Call);
maybe_execute_action(<<"deactivate">>, #follow_me{enabled='true'}=FollowMe, Call) ->
    deactivate_follow_me(FollowMe, Call);
maybe_execute_action(<<"toggle">>, #follow_me{enabled='false'}=FollowMe, Call) ->
    maybe_execute_action(<<"activate">>, FollowMe, Call);
maybe_execute_action(<<"toggle">>, #follow_me{enabled='true'}=FollowMe, Call) ->
    maybe_execute_action(<<"deactivate">>, FollowMe, Call);
maybe_execute_action(_Action, _, Call) ->
    lager:info("follow-me action ~s is invalid", [_Action]),
    kapps_call_command:b_prompt(<<"follow-me-not_available">>, Call).

-spec activate_follow_me(follow_me(), kapps_call:call()) -> any().
activate_follow_me(#follow_me{jobj=JObj
                             ,account_db=AccountDb
                             }, Call) ->
    case maybe_update_doc('true', JObj, AccountDb) of
        {'error', _} -> 'ok';
        {'ok', _} -> kapps_call_command:b_prompt(<<"follow-me-activated">>, Call)
    end.

-spec deactivate_follow_me(follow_me(), kapps_call:call()) -> any().
deactivate_follow_me(#follow_me{jobj=JObj
                               ,account_db=AccountDb
                               }, Call) ->
    case maybe_update_doc('false', JObj, AccountDb) of
        {'error', _} -> 'ok';
        {'ok', _} -> kapps_call_command:b_prompt(<<"follow-me-deactivated">>, Call)
    end.

-spec maybe_update_doc(boolean(), kz_json:object(), kz_term:ne_binary()) -> kz_term:jobj_return().
maybe_update_doc(Enabled, JObj, AccountDb) ->
    Updated = kz_json:set_value([<<"follow_me">>], Enabled, JObj),
    case kz_datamgr:save_doc(AccountDb, Updated) of
        {'error', 'conflict'} -> maybe_update_conflicting_doc(Enabled, AccountDb, kz_doc:id(JObj));
        {'error', _}=E -> E;
        {'ok', _}=Ok ->
            lager:info("follow-me set to ~s on document ~s", [Enabled, kz_doc:id(JObj)]),
            Ok
    end.

-spec maybe_update_conflicting_doc(boolean(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:jobj_return().
maybe_update_conflicting_doc(Enabled, AccountDb, Id) ->
    case maybe_get_doc(AccountDb, Id) of
        {'error', _}=E -> E;
        {'ok', NewJObj} -> maybe_update_doc(Enabled, NewJObj, AccountDb)
    end.
