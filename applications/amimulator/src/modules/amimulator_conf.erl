-module(amimulator_conf).

-export([init/1
        ,bindings/1
        ,responders/1
        ,handle_event/2
        ]).

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
    [{<<"conference">>, <<"event">>}].

-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_event(EventJObj, _) ->
    Event = kz_json:get_value(<<"Event">>, EventJObj),
    handle_specific_event(Event, EventJObj).

%%
%% Event type handlers
%%

handle_specific_event(<<"add-member">>, EventJObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    ConferenceId = kz_json:get_value(<<"Conference-ID">>, EventJObj),
    {'ok', ConferenceNumber} = amimulator_util:find_id_number(ConferenceId, AccountDb),
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    Call = ami_sm:call(CallId),
    Channel = amimulator_call:channel(Call),
    CallerIdName = amimulator_call:id_name(Call),

    add_participants_cache_entry(CallId, Channel, CallerIdName),

    Payload = [{<<"Event">>, <<"MeetmeJoin">>}
              ,{<<"Privilege">>, <<"call,all">>}
              ,{<<"Channel">>, Channel}
              ,{<<"Uniqueid">>, CallId}
              ,{<<"Meetme">>, ConferenceNumber}
              ,{<<"Usernum">>, kz_json:get_integer_value(<<"Participant-ID">>, EventJObj)}
              ,{<<"CallerIDnum">>, CallerIdName}
              ,{<<"CallerIDname">>, CallerIdName}
              ,{<<"ConnectedLineNum">>, <<"unknown">>}
              ,{<<"ConnectedLineName">>, <<"unknown">>}
              ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, AccountId);
handle_specific_event(<<"del-member">>, EventJObj) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, EventJObj),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    ConferenceId = kz_json:get_value(<<"Conference-ID">>, EventJObj),
    {'ok', ConferenceNumber} = amimulator_util:find_id_number(ConferenceId, AccountDb),
    CallId = kz_json:get_value(<<"Call-ID">>, EventJObj),
    CachedConfPart = ami_sm:conf_cache(CallId),
    Channel = props:get_value(<<"Channel">>, CachedConfPart),
    CallerIdName = props:get_value(<<"CallerIDnum">>, CachedConfPart),
    Timestamp = props:get_value(<<"Timestamp">>, CachedConfPart),
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    Duration = (MegaSecs * 1000000 + Secs) - Timestamp,

    del_participants_cache_entry(CallId),

    Payload = [{<<"Event">>, <<"MeetmeLeave">>}
              ,{<<"Privilege">>, <<"call,all">>}
              ,{<<"Channel">>, Channel}
              ,{<<"Uniqueid">>, CallId}
              ,{<<"Meetme">>, ConferenceNumber}
              ,{<<"Usernum">>, kz_json:get_value(<<"Participant-ID">>, EventJObj)}
              ,{<<"CallerIDNum">>, CallerIdName}
              ,{<<"CallerIDName">>, CallerIdName}
              ,{<<"ConnectedLineNum">>, <<"<unknown>">>}
              ,{<<"ConnectedLineName">>, <<"<unknown>">>}
              ,{<<"Duration">>, Duration}
              ],
    amimulator_event_listener:publish_amqp_event({'publish', Payload}, AccountId);
handle_specific_event(Evt, _) ->
    lager:debug("unhandled event ~p", [Evt]).

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

-spec add_participants_cache_entry(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
add_participants_cache_entry(CallId, Channel, CallerIdName) ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),

    Cache = [{<<"Channel">>, Channel}
            ,{<<"CallerIDnum">>, CallerIdName}
            ,{<<"Timestamp">>, MegaSecs * 1000000 + Secs}
            ],
    ami_sm:cache_conf_part(CallId, Cache).

-spec del_participants_cache_entry(ne_binary()) -> 'ok'.
del_participants_cache_entry(CallId) ->
    ami_sm:delete_cached_conf_part(CallId).
