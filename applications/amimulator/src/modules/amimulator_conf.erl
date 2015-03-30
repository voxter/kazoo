-module(amimulator_conf).

-export([init/1, account_conferences/1, handle_event/1]).

-include("../amimulator.hrl").

init(_AccountId) ->
    ok.

account_conferences(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    {ok, Conferences} = couch_mgr:get_results(AccountDb, <<"conferences/crossbar_listing">>),
    lists:foldl(fun(Conference, Ids) ->
        [wh_json:get_value(<<"id">>, Conference) | Ids]
    end, [], Conferences).

handle_event(EventJObj) ->
    {_EventType, EventName} = wh_util:get_event_type(EventJObj),
    handle_specific_event(EventName, EventJObj).

%%
%% Event type handlers
%%

handle_specific_event(<<"participants_event">>, EventJObj) ->
    Payload = participants_cache_change(EventJObj),
    amimulator_amqp:publish_amqp_event({publish, Payload});
handle_specific_event(_, EventJObj) ->
    lager:debug("unhandled event ~p", [EventJObj]).

%%
%% Private functions
%%

participants_cache_change(EventJObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, EventJObj),
    {ok, ConferenceNumber} = amimulator_util:find_id_number(ConferenceId, AccountDb),
    CachedParticipants = case amimulator_store:get(<<"conf-", ConferenceId/binary>>) of
        undefined ->
            [];
        Something ->
            Something
    end,

    Current = wh_json:get_value(<<"Participants">>, EventJObj),
    Removed = removed(removed_participants(CachedParticipants, Current), ConferenceNumber),
    Added = added(added_participants(CachedParticipants, Current), ConferenceNumber),
    update_cache(Removed, Added, CachedParticipants, ConferenceId),

    %Removed ++ Added.
    Added.

update_cache(Removed, Added, Cached, ConferenceId) ->
    Done1 = lists:foldl(fun(Remove, Acc) ->
        lists:delete(proplists:get_value(<<"Uniqueid">>, Remove), Acc)
    end, Cached, Removed),
    Done2 = lists:foldl(fun(Add, Acc) ->
        CallId = proplists:get_value(<<"Uniqueid">>, Add),

        %% Here we are caching things like CID for cleanup later on
        %% The cleanup will happen in the CHANNEL_DESTROY event
        {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
        Cache = [
            {<<"Channel">>, proplists:get_value(<<"Channel">>, Add)},
            {<<"Meetme">>, proplists:get_value(<<"Meetme">>, Add)},
            {<<"Usernum">>, proplists:get_value(<<"Usernum">>, Add)},
            {<<"CallerIDnum">>, proplists:get_value(<<"CallerIDnum">>, Add)},
            {<<"Timestamp">>, MegaSecs * 1000000 + Secs}
        ],
        amimulator_store:put(<<"conf-cache-", CallId/binary>>, Cache),
        [CallId | Acc]
    end, Done1, Added),
    amimulator_store:put(<<"conf-", ConferenceId/binary>>, Done2).

removed_participants(Cached, Current) ->
    removed_participants(Cached, Current, []).

removed_participants([], _Current, Removed) ->
    Removed;
removed_participants([First|Others], Current, Removed) ->
    case find_in_current(First, Current) of
        true ->
            removed_participants(Others, Current, Removed);
        false ->
            removed_participants(Others, Current, [First | Removed])
    end.

removed(CallIds, ConferenceNumber) when is_list(CallIds) ->
    [removed(CallId, ConferenceNumber) || CallId <- CallIds];

removed(CallId, _ConferenceNumber) ->
    _Call = amimulator_store:get(<<"call-", CallId/binary>>),

    %EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),
    %CallerId = props:get_value(<<"aleg_cid">>, Call),

    [
        {<<"Event">>, <<"MeetmeLeave">>}%,
        %{<<"Conference">>, ConferenceNumber},
        %{<<"Channel">>, EndpointName},
        %{<<"Uniqueid">>, CallId},
        %{<<"CallerIDnum">>, CallerId},
        %{<<"CallerIDname">>, CallerId}
    ].

find_in_current(_First, []) ->
    false;
find_in_current(First, [H|T]) ->
    case wh_json:get_value(<<"Call-ID">>, H) of
        First ->
            true;
        _ ->
            find_in_current(First, T)
    end.

added_participants(Cached, Current) ->
    added_participants(Cached, Current, []).

added_participants(_Cached, [], Added) ->
    Added;
added_participants(Cached, [First|Others], Added) ->
    case find_in_cached(First, Cached) of
        true ->
            added_participants(Cached, Others, Added);
        false ->
            added_participants(Cached, Others, [wh_json:get_value(<<"Call-ID">>, First) | Added])
    end.

added(CallIds, ConferenceNumber) when is_list(CallIds) ->
    [added(CallId, ConferenceNumber) || CallId <- CallIds];

added(CallId, ConferenceNumber) ->
    Call = amimulator_store:get(<<"call-", CallId/binary>>),

    _Exten = props:get_value(<<"aleg_exten">>, Call),
    EndpointName = props:get_value(<<"aleg_ami_channel">>, Call),
    CallerId = props:get_value(<<"aleg_cid">>, Call),

    [
        {<<"Event">>, <<"MeetmeJoin">>},
        {<<"Privilege">>, <<"call,all">>},
        {<<"Channel">>, EndpointName},
        {<<"Uniqueid">>, CallId},
        {<<"Meetme">>, ConferenceNumber},
        {<<"Usernum">>, 1},
        {<<"CallerIDnum">>, CallerId},
        {<<"CallerIDname">>, CallerId},
        {<<"ConnectedLineNum">>, <<"unknown">>},
        {<<"ConnectedLineName">>, <<"unknown">>}
    ].

find_in_cached(_First, []) ->
    false;
find_in_cached(First, [H|T]) ->
    case wh_json:get_value(<<"Call-ID">>, First) of
        H ->
            true;
        _ ->
            find_in_current(First, T)
    end.




