%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc Data: {
%%%   "add": [
%%%     "skill1",
%%%     ...
%%%   ],
%%%   "remove": [
%%%     "skill2",
%%%     ...
%%%   ]
%%% }
%%%
%%%
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_acdc_required_skills).

-export([handle/2]).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-define(KVS_KEY, 'acdc_required_skills').

%%------------------------------------------------------------------------------
%% Handle execution of this callflow module
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Add = kz_json:get_list_value(<<"add">>, Data, []),
    Remove = kz_json:get_list_value(<<"remove">>, Data, []),

    Skills = kapps_call:kvs_fetch(?KVS_KEY, [], Call),
    Skills1 = remove_skills(Remove, add_skills(Add, Skills)),

    lager:info("resulting list of required skills: ~p", [Skills1]),

    Call1 = kapps_call:kvs_store(?KVS_KEY, Skills1, Call),
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1).

%%------------------------------------------------------------------------------
%% Add new skills to a list of required skills
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_skills(kz_term:ne_binaries(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
add_skills(Add, Skills) ->
    lists:usort(Skills ++ Add).

%%------------------------------------------------------------------------------
%% Remove skills from a list of required skills
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_skills(kz_term:ne_binaries(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
remove_skills(Remove, Skills) ->
    lists:filter(fun(Skill) ->
                         not lists:member(Skill, Remove)
                 end
                ,Skills
                ).
