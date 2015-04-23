%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(cf_variable_route).

-include("../callflow.hrl").

-export([handle/2
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
	ChildId = check_branches(wh_json:get_value(<<"branches">>, Data), Data, Call),
	cf_exe:continue(ChildId, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
check_branches([], Data, Call) ->
	<<"_">>;
check_branches([H|T], Data, Call) ->
	case evaluate_branch(H, Call) of
		'false' ->
			check_branches(T, Data, Call);
		'true' ->
			wh_json:get_value(<<"child_id">>, H)
	end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
evaluate_branch(Branch, Call) ->
	case whapps_call:custom_channel_var(wh_json:get_value(<<"variable">>, Branch)) of
		'undefined' ->
			'false';
		CallValue ->
			evaluate_branch(CallValue, wh_json:get_value(<<"operator">>, Branch), wh_json:get_value(<<"value">>, Branch))
	end.

evaluate_branch(CallValue, <<"<">>, Value) ->
	CallValue < Value;
evaluate_branch(CallValue, <<"<=">>, Value) ->
	CallValue <= Value;
evaluate_branch(CallValue, <<"=">>, Value) ->
	CallValue =:= Value;
evaluate_branch(CallValue, <<"=/=">>, Value) ->
	CallValue =/= Value;
evaluate_branch(CallValue, <<">=">>, Value) ->
	CallValue >= Value;
evaluate_branch(CallValue, <<">">>, Value) ->
	CallValue > Value.
