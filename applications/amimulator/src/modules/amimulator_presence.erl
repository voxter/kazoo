-module(amimulator_presence).

-export([init/1
        ,bindings/1
        ,responders/1
        ,get_extra_props/1
        ,handle_event/2
        ]).

-include("../amimulator.hrl").

-define(CF_CONFIG_CAT, <<"callflow">>).
-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".park">>).
-define(DEFAULT_RINGBACK_TM, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_ringback_time">>, 120000)).

%%
%% Public functions
%%

-spec init(ne_binary()) -> 'ok'.
init(_) -> 'ok'.

-spec bindings(kz_proplist()) -> kz_proplist().
bindings(Props) ->
    AccountId = props:get_value("AccountId", Props),
    Realm = kz_util:get_account_realm(AccountId),
    PresenceUsers = expand_feature_codes(presence_feature_codes(AccountId)),
    PresenceIds = lists:map(fun(User) ->
                                    <<User/binary, "@", Realm/binary>>
                            end, PresenceUsers),
    lists:map(fun(PresenceId) ->
                      {'presence', [{'restrict_to', ['update']}
                                   ,{'presence-id', PresenceId}]}
              end, PresenceIds).

-spec responders(kz_proplist()) -> kz_proplist().
responders(_) ->
    [{<<"presence">>, <<"update">>}].

-spec get_extra_props(ne_binary()) -> kz_proplist().
get_extra_props(AccountId) ->
    [{<<"Presence-Feature-Codes">>, presence_feature_codes(AccountId)}].

-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_event(EventJObj, Props) ->
    handle_specific_event(kz_util:get_event_type(EventJObj)
                         ,EventJObj
                         ,Props).

%%
%% Event type handlers
%%

-spec handle_specific_event({api_binary(), api_binary()}
                           ,kz_json:object()
                           ,kz_proplist()) -> 'ok'.
handle_specific_event({<<"presence">>, <<"update">>}, EventJObj, Props) ->
    State = kz_json:get_value(<<"State">>, EventJObj),
    handle_presence_update(EventJObj, State, Props).

%%
%% Private functions
%%

%%--------------------------------------------------------------------
%% @doc
%% Handle presence updates on feature code keys (used for park)
%% @end
%%--------------------------------------------------------------------
-spec handle_presence_update(kz_json:object(), ne_binary(), kz_proplist()) ->
                                    'ok'.
handle_presence_update(EventJObj, <<"early">>, Props) ->
    PresenceUser = presence_user(EventJObj),
    FeatureCodes = props:get_value(<<"Presence-Feature-Codes">>, Props),
    case presence_user_match(PresenceUser, FeatureCodes) of
        [PresenceUser, Slot] ->
            TargetCallId = kz_json:get_value(<<"Target-Call-ID">>, EventJObj),
            Call = ami_sm:call(TargetCallId),
            Payload = [{<<"Event">>, <<"ParkedCall">>}
                      ,{<<"Privilege">>, <<"call,all">>}
                      ,{<<"Exten">>, PresenceUser}
                      ,{<<"Channel">>, amimulator_call:channel(Call)}
                      ,{<<"Parkinglot">>, <<"parkedcalls", Slot/binary>>}
                      ,{<<"From">>, amimulator_call:channel(Call)}
                      ,{<<"Timeout">>, ?DEFAULT_RINGBACK_TM div 1000}
                      ,{<<"CallerIDNum">>, amimulator_call:id_number(Call)}
                      ,{<<"CallerIDName">>, amimulator_call:id_name(Call)}
                      ,{<<"ConnectedLineNum">>, <<"<unknown>">>}
                      ,{<<"ConnectedLineName">>, <<"<unknown>">>}
                      ,{<<"Uniqueid">>, TargetCallId}
                      ],
            amimulator_event_listener:publish_amqp_event({'publish', Payload}
                                                        ,props:get_value(<<"AccountId">>, Props));
        _ -> 'ok'
    end;
handle_presence_update(EventJObj, <<"terminated">>, Props) ->
    PresenceUser = presence_user(EventJObj),
    FeatureCodes = props:get_value(<<"Presence-Feature-Codes">>, Props),
    case presence_user_match(PresenceUser, FeatureCodes) of
        [PresenceUser, Slot] ->
            TargetCallId = kz_json:get_value(<<"Target-Call-ID">>, EventJObj),
            Call = ami_sm:call(TargetCallId),
            Payload = [{<<"Event">>, <<"UnParkedCall">>}
                      ,{<<"Privilege">>, <<"call,all">>}
                      ,{<<"Exten">>, PresenceUser}
                      ,{<<"Channel">>, amimulator_call:channel(Call)}
                      ,{<<"Parkinglot">>, <<"parkedcalls", Slot/binary>>}
                      ,{<<"From">>, amimulator_call:channel(Call)}
                      ,{<<"CallerIDNum">>, amimulator_call:id_number(Call)}
                      ,{<<"CallerIDName">>, amimulator_call:id_name(Call)}
                      ,{<<"ConnectedLineNum">>, <<"<unknown>">>}
                      ,{<<"ConnectedLineName">>, <<"<unknown>">>}
                      ,{<<"Uniqueid">>, TargetCallId}
                      ],
            amimulator_event_listener:publish_amqp_event({'publish', Payload}
                                                        ,props:get_value(<<"AccountId">>, Props));
        _ -> 'ok'
    end;
handle_presence_update(_, _, _) -> 'ok'.

-spec presence_user(kz_json:object()) -> ne_binary().
presence_user(EventJObj) ->
    PresenceId = kz_json:get_value(<<"Presence-ID">>, EventJObj),
    hd(binary:split(PresenceId, <<"@">>)).

-spec presence_user_match(ne_binary(), kz_proplist()) -> ne_binaries() | 'false'.
presence_user_match(_, []) -> 'false';
presence_user_match(PresenceUser, [{Pattern, _}|FeatureCodes]) ->
    case re:run(PresenceUser, Pattern, [{'capture', 'all', 'binary'}]) of
        {'match', Matches} -> Matches;
        'nomatch' -> presence_user_match(PresenceUser, FeatureCodes)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Fetch and format feature codes list for account (currently only
%% capturing park_and_retrieve)
%% @end
%%--------------------------------------------------------------------
-spec presence_feature_codes(ne_binary() | kz_json:objects()) ->
                                    kz_proplist().
presence_feature_codes(AccountId) when is_binary(AccountId) ->
    case kz_datamgr:get_results(kz_util:format_account_id(AccountId, 'encoded')
                               ,<<"callflows/listing_by_pattern">>
                               ,['include_docs']) of
        {'ok', JObjs} -> presence_feature_codes(JObjs);
        {'error', E} ->
            lager:debug("could not open callflows/crossbar_listing to get feature codes (~p)", [E]),
            []
    end;
presence_feature_codes(JObjs) ->
    lists:foldl(fun add_featurecode/2, [], JObjs).

-spec add_featurecode(kz_json:object(), kz_proplist()) ->
                             kz_proplist().
add_featurecode(JObj, FeatureCodes) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    FeatureCode = kz_json:get_value(<<"featurecode">>, Doc),
    maybe_add_featurecode(kz_json:get_value(<<"name">>, FeatureCode)
                         ,kz_json:get_value(<<"key">>, JObj)
                         ,FeatureCodes).

-spec maybe_add_featurecode(api_binary(), ne_binary(), kz_proplist()) ->
                                   kz_proplist().
maybe_add_featurecode(<<"park_and_retrieve">>, Pattern, FeatureCodes) ->
    [{Pattern, <<"park_and_retrieve">>} | FeatureCodes];
maybe_add_featurecode(_, _, FeatureCodes) -> FeatureCodes.

%%--------------------------------------------------------------------
%% @doc
%% Handle expansion of feature code regular expressions to generate
%% a list of presence users whose presence state should be subscribed
%% to
%% @end
%%--------------------------------------------------------------------
-spec expand_feature_codes(kz_proplist()) -> ne_binaries().
expand_feature_codes(FeatureCodes) ->
    lists:foldl(fun({Pattern, <<"park_and_retrieve">>}, Acc) ->
                        Acc ++ expand_pattern(Pattern)
                end, [], FeatureCodes).

-spec expand_pattern(binary()) -> ne_binaries().
expand_pattern(Pattern) -> expand_pattern(Pattern, []).

expand_pattern(<<>>, Acc) -> lists:usort(Acc);
expand_pattern(<<"^", Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, Acc);
expand_pattern(<<"$", Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, Acc);
expand_pattern(<<"\\", Escaped:1/binary, Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, pattern_acc_append(Acc, Escaped));
expand_pattern(<<"?", Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, pattern_acc_last_optional(Acc));
%% We don't expand repeats so as to make finite list of expansions
%% But we do use * as optional
expand_pattern(<<"*", Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, pattern_acc_last_optional(Acc));
expand_pattern(<<"+", Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, Acc);
%% TODO handle groups
expand_pattern(<<"(", Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, Acc);
expand_pattern(<<")", Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, Acc);
expand_pattern(<<"[", _/binary>>=Pattern, Acc) ->
    {Pattern1, Characters} = expand_character_class(Pattern),
    expand_pattern(Pattern1, pattern_acc_append_all(Acc, Characters));
expand_pattern(<<Char:1/binary, Pattern/binary>>, Acc) ->
    expand_pattern(Pattern, pattern_acc_append(Acc, Char)).

-spec pattern_acc_append(ne_binaries(), binary()) -> ne_binaries().
pattern_acc_append([], Char) -> [Char];
pattern_acc_append(Acc, Char) ->
    lists:foldl(fun(AccItem, Acc1) ->
                        [<<AccItem/binary, Char/binary>> | Acc1]
                end, [], Acc).

-spec pattern_acc_append_all(ne_binaries(), ne_binaries()) -> ne_binaries().
-spec pattern_acc_append_all(ne_binaries(), ne_binaries(), ne_binaries()) ->
                                    ne_binaries().
pattern_acc_append_all([], Chars) -> Chars;
pattern_acc_append_all(Acc, Chars) ->
    pattern_acc_append_all(Acc, Chars, []).

pattern_acc_append_all(_, [], Appended) -> Appended;
pattern_acc_append_all(OriginalAcc, [Char|Chars], Appended) ->
    Appended1 = Appended ++ pattern_acc_append(OriginalAcc, Char),
    pattern_acc_append_all(OriginalAcc, Chars, Appended1).

-spec pattern_acc_last_optional(ne_binaries()) -> ne_binaries().
-spec pattern_acc_last_optional(ne_binaries(), ne_binaries()) ->
                                       ne_binaries().
pattern_acc_last_optional(Binaries) ->
    pattern_acc_last_optional(Binaries, []).

pattern_acc_last_optional([], Acc) -> Acc;
pattern_acc_last_optional([Binary|Binaries], Acc) ->
    Acc1 = [Binary, binary:part(Binary, 0, byte_size(Binary)-1) | Acc],
    pattern_acc_last_optional(Binaries, Acc1).

%%--------------------------------------------------------------------
%% @doc
%% Expand a regex character class into it's possible combinations.
%% Returns a tuple of remaining characters (after the character class
%% closing bracket) and the combinations
%% @end
%%--------------------------------------------------------------------
-spec expand_character_class(ne_binary()) -> {binary(), ne_binaries()}.
-spec expand_character_class(ne_binary(), ne_binaries()) ->
                                    {binary(), ne_binaries()}.
expand_character_class(Pattern) ->
    expand_character_class(Pattern, []).

expand_character_class(<<"[", Pattern/binary>>, Acc) ->
    expand_character_class(Pattern, Acc);
expand_character_class(<<"]", Pattern/binary>>, Acc) -> {Pattern, Acc};
expand_character_class(<<Char1:1/binary, "-", Char2:1/binary, Pattern/binary>>, Acc) ->
    Acc1 = Acc ++ character_range(Char1, Char2),
    expand_character_class(Pattern, Acc1);
%% It doesn't seem worth expanding dot for purposes of this module
expand_character_class(<<".", Pattern/binary>>, Acc) ->
    expand_character_class(Pattern, Acc);
expand_character_class(<<Char:1/binary, Pattern/binary>>, Acc) ->
    expand_character_class(Pattern, [Char | Acc]).

-spec character_range(ne_binary() | non_neg_integer()
                     ,ne_binary() | non_neg_integer()) ->
                             ne_binaries().
-spec character_range(non_neg_integer(), non_neg_integer(), ne_binaries()) ->
                             ne_binaries().
character_range(<<StartValue>>, End) ->
    character_range(StartValue, End);
character_range(Start, <<EndValue>>) ->
    character_range(Start, EndValue);
character_range(Start, End) when Start > End ->
    character_range(End, Start);
character_range(Start, End) ->
    character_range(Start, End, []).

character_range(Start, End, Acc) when Start > End -> Acc;
character_range(Start, End, Acc) ->
    character_range(Start+1, End, [<<Start>> | Acc]).
