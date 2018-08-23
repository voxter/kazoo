%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voxter Communications Inc
%%% @doc
%%%
%%% Data: {
%%%   "id":"queue id",
%%%   "window":900 // Window over which average wait time is calc'd
%%% }
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(cf_acdc_wait_time).

-export([handle/2]).

-behaviour(gen_cf_action).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% Handle execution of this callflow module
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    AccountId = kapps_call:account_id(Call),
    QueueId = maybe_use_variable(Data, Call),
    Skills = maybe_include_skills(QueueId, Call),
    Window = kz_json:get_integer_value(<<"window">>, Data),

    case Skills of
        'undefined' -> 'ok';
        _ -> lager:info("evaluating average wait time for skill set ~p", [Skills])
    end,

    case Window of
        'undefined' -> 'ok';
        _ -> lager:info("evaluating average wait time over last ~b seconds", [Window])
    end,

    case acdc_stats:average_wait_time_estimation(AccountId, QueueId, Skills, Window) of
        {'ok', AverageWaitTime} ->
            %% Save the estimated wait time so it can later be included in the
            %% call stat if the caller enters the queue
            kapps_call:kvs_store('acdc_average_wait_time_estimation', AverageWaitTime, Call),

            {'branch_keys', BranchKeys} = cf_exe:get_branch_keys(Call),
            evaluate_average_wait_time(AverageWaitTime, BranchKeys, Call);
        {'error', _} ->
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a variable id is specified and refers to an existing document,
%% use that variable id instead of the one specified in the module's
%% data
%% @end
%%--------------------------------------------------------------------
-spec maybe_use_variable(kz_json:object(), kapps_call:call()) -> api_binary().
maybe_use_variable(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"var">>, Data) of
        'undefined' ->
            kz_json:get_ne_binary_value(<<"id">>, Data);
        Variable ->
            Value = kz_json:get_value(<<"value">>, cf_kvs_set:get_kv(Variable, Call)),
            case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), Value) of
                {'ok', _} -> Value;
                _ -> kz_doc:id(Data)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the selected strategy on the requested queue is skills-based
%% round robin, skills should be considered in the wait time eval.
%% @end
%%--------------------------------------------------------------------
-spec maybe_include_skills(ne_binary(), kapps_call:call()) -> api_ne_binaries().
maybe_include_skills(QueueId, Call) ->
    AccountDb = kapps_call:account_db(Call),
    {'ok', JObj} = kz_datamgr:open_cache_doc(AccountDb, QueueId),
    case kz_json:get_ne_binary_value(<<"strategy">>, JObj) of
        <<"skills_based_round_robin">> ->
            kapps_call:kvs_fetch('acdc_required_skills', [], Call);
        _ -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Continue to the branch of the callflow with the highest exceeded
%% threshold
%% @end
%%--------------------------------------------------------------------
-spec evaluate_average_wait_time(non_neg_integer(), kz_json:path(), kapps_call:call()) -> 'ok'.
evaluate_average_wait_time(AverageWaitTime, Keys, Call) ->
    Keys1 = lists:sort(fun(Key1, Key2) ->
                               kz_term:to_integer(Key1) >= kz_term:to_integer(Key2)
                       end, Keys),
    evaluate_average_wait_time2(AverageWaitTime, Keys1, Call).

-spec evaluate_average_wait_time2(non_neg_integer(), kz_json:path(), kapps_call:call()) -> 'ok'.
evaluate_average_wait_time2(_, [], Call) ->
    cf_exe:continue(Call);
evaluate_average_wait_time2(AverageWaitTime, [Key|Keys], Call) ->
    Threshold = kz_term:to_integer(Key),
    case AverageWaitTime >= Threshold of
        'true' ->
            lager:info("average wait time exceeded threshold ~B", [Threshold]),
            cf_exe:continue(Key, Call);
        'false' ->
            evaluate_average_wait_time2(AverageWaitTime, Keys, Call)
    end.
