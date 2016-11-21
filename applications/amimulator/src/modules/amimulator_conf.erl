-module(amimulator_conf).

-export([init/1, bindings/1, responders/1, handle_event/2]).

-include("../amimulator.hrl").

%%
%% Public functions
%%

-spec init(ne_binary()) -> 'ok'.
init(_AccountId) ->
    ok.

-spec bindings(kz_proplist()) -> kz_proplist().
bindings(Props) ->
    AccountId = props:get_value("AccountId", Props),
    [
        {conference, [
            {restrict_to, conference_bindings(account_conferences(AccountId), [])}
        ]}
    ].

-spec responders(kz_proplist()) -> kz_proplist().
responders(_Props) ->
    [{<<"conference">>, <<"participants_event">>}].

-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_event(EventJObj, Props) ->
    AccountId = props:get_value(<<"AccountId">>, Props),
    {_EventType, EventName} = kz_util:get_event_type(EventJObj),
    handle_specific_event(EventName, kz_json:set_value(<<"Account-ID">>, AccountId, EventJObj)).

%%
%% Event type handlers
%%

handle_specific_event(<<"participants_event">>, EventJObj) ->
    Payload = participants_cache_change(EventJObj),
    amimulator_event_listener:publish_amqp_event({publish, Payload}, kz_json:get_value(<<"Account-ID">>, EventJObj));
handle_specific_event(_, EventJObj) ->
    lager:debug("unhandled event ~p", [EventJObj]).

%%
%% Private functions
%%

%% Find all conferences belonging to an account
account_conferences(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    {ok, Conferences} = kz_datamgr:get_results(AccountDb, <<"conferences/crossbar_listing">>),
    lists:foldl(fun(Conference, Ids) ->
        [kz_json:get_value(<<"id">>, Conference) | Ids]
    end, [], Conferences).

conference_bindings([], Bindings) ->
    Bindings;
conference_bindings([ConferenceId|Ids], Bindings) ->
    conference_bindings(Ids, [{event, ConferenceId} | Bindings]).

participants_cache_change(EventJObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = kz_util:format_account_id(AccountId, encoded),
    ConferenceId = kz_json:get_value(<<"Conference-ID">>, EventJObj),
    {ok, ConferenceNumber} = amimulator_util:find_id_number(ConferenceId, AccountDb),
    CachedParticipants = ami_sm:conf_parts(ConferenceId),

    Current = kz_json:get_value(<<"Participants">>, EventJObj, []),
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
        ami_sm:cache_conf_part(CallId, Cache),
        [CallId | Acc]
    end, Done1, Added),
    ami_sm:update_conf_parts(ConferenceId, Done2).

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

removed(_CallId, _ConferenceNumber) ->
    %_Call = amimulator_store:get(<<"call-", CallId/binary>>),

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
    case kz_json:get_value(<<"Call-ID">>, H) of
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
        true -> added_participants(Cached, Others, Added);
        false -> added_participants(Cached, Others, [kz_json:get_value(<<"Call-ID">>, First) | Added])
    end.

added(CallIds, ConferenceNumber) when is_list(CallIds) ->
    [added(CallId, ConferenceNumber) || CallId <- CallIds];

added(CallId, ConferenceNumber) ->
    lager:debug("adding call ~s to conference", [CallId]),
    Call = ami_sm:call(CallId),

    EndpointName = amimulator_call:channel(Call),
    CallerId = amimulator_call:id_name(Call),

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
    case kz_json:get_value(<<"Call-ID">>, First) of
        H ->
            true;
        _ ->
            find_in_cached(First, T)
    end.




