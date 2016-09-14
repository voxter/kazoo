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
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
	ChildId = check_branches(kz_json:get_value(<<"branches">>, Data), Data, Call),
	cf_exe:continue(ChildId, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
check_branches([], _Data, _Call) ->
	<<"_">>;
check_branches([H|T], Data, Call) ->
	case evaluate_branch(H, Call) of
		'false' ->
			check_branches(T, Data, Call);
		'true' ->
			kz_json:get_value(<<"child_id">>, H)
	end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
evaluate_branch(Branch, Call) ->
	case kapps_call:custom_channel_var(kz_json:get_value(<<"variable">>, Branch), Call) of
		'undefined' ->
			'false';
		CallValue ->
			evaluate_branch(CallValue, kz_json:get_value(<<"operator">>, Branch), kz_json:get_value(<<"value">>, Branch))
	end.

evaluate_branch(CallValue, <<"<">>, Value) ->
	CallValue < Value;
evaluate_branch(CallValue, <<"<=">>, Value) ->
	CallValue =< Value;
evaluate_branch(CallValue, <<"=">>, Value) ->
	CallValue =:= Value;
evaluate_branch(CallValue, <<"=/=">>, Value) ->
	CallValue =/= Value;
evaluate_branch(CallValue, <<">=">>, Value) ->
	CallValue >= Value;
evaluate_branch(CallValue, <<">">>, Value) ->
	CallValue > Value.
