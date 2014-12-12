%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Conferences module
%%%
%%% Handle client requests for conference documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_conferences).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,validate/1, validate/2, validate/3, validate/4
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"conferences/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.conferences">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.conferences">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.conferences">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.conferences">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.conferences">>, ?MODULE, post),
    crossbar_bindings:bind(<<"*.execute.delete.conferences">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_, <<"details">>) ->
	[?HTTP_GET].
allowed_methods(_, _, _) ->
	[?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, <<"details">>) -> 'true'.
resource_exists(_, _, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_conferences(Context, cb_context:req_verb(Context)).

validate_conferences(Context, ?HTTP_GET) ->
    load_conference_summary(Context);
validate_conferences(Context, ?HTTP_PUT) ->
    create_conference(Context).

validate(Context, Id) ->
    validate_conference(Context, Id, cb_context:req_verb(Context)).

validate_conference(Context, Id, ?HTTP_GET) ->
    load_conference(Id, Context);
validate_conference(Context, Id, ?HTTP_POST) ->
    update_conference(Id, Context);
validate_conference(Context, Id, ?HTTP_DELETE) ->
    load_conference(Id, Context).
    
validate(Context, ConfId, <<"details">>) ->
    crossbar_doc:handle_json_success(lookup_participants(conference_details(Context, ConfId)), Context).
validate(Context, ConfId, Action, ParticipantId) ->
	Conference = whapps_conference:set_id(ConfId, whapps_conference:new()),
	crossbar_doc:handle_json_success(wh_json:set_value(<<"resp">>, perform_conference_action(Conference, Action, ParticipantId), wh_json:new()), Context).
	
%%
%% Perform conference kick/mute/unmute via API
%%
perform_conference_action(Conference, <<"mute">>, ParticipantId) ->
	whapps_conference_command:mute_participant(ParticipantId, Conference);
perform_conference_action(Conference, <<"unmute">>, ParticipantId) ->
	whapps_conference_command:unmute_participant(ParticipantId, Conference);
perform_conference_action(Conference, <<"kick">>, ParticipantId) ->
	whapps_conference_command:kick(ParticipantId, Conference).
    
%%
%% Returns some details about the users connected to a conference
%%
conference_details(Context, ConfId) ->
	lager:info("lookup_confs"),
	AccountId = cb_context:account_id(Context),
    AccountDb = cb_context:account_db(Context),
    AccountRealm = wh_util:get_account_realm(AccountDb, AccountId),
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, []}
           ,{<<"Conference-ID">>, ConfId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
	lager:info("before pool collect"),
    ReqResp = whapps_util:amqp_pool_collect(Req
                                            ,fun wapi_conference:publish_search_req/1
                                            ,{'ecallmgr', 'true'}
                                           ),
    case ReqResp of
        {'error', _} -> [];
        {_, JObjs} ->
        	hd(JObjs)
    end.
    
lookup_participants(JObjs) ->
	lager:debug("Looking up participants of conference"),
	Data = wh_json:get_value(<<"Participants">>, JObjs),
	Details = participant_details(Data),
	channel_details(Details).
	
participant_details(undefined) -> [];
participant_details(Participants) ->
	lists:foldl(fun(Participant, Acc) ->
		CallId = wh_json:get_value(<<"Call-ID">>, Participant),
		ParticipantId = wh_json:get_value(<<"Participant-ID">>, Participant),
		Mute = not wh_json:get_value(<<"Speak">>, Participant),
		lager:debug("Call-ID found: ~p", [CallId]),
		lager:debug("Participant-ID found: ~p", [Participant]),
		Acc ++ [{CallId, ParticipantId, Mute}] end,
		[], Participants).
		
channel_details(Details) ->
	lists:foldl(fun({UUID, ParticipantId, Mute}, Acc) ->
		Channel = hd(element(1, rpc:call('ecallmgr@awe01.tor1.voxter.net', ecallmgr_fs_channels, get_channels, [UUID]))),
		ChannelJSON = wh_json:set_values(
			[{<<"participant_id">>, ParticipantId}, {<<"mute">>, Mute}],
			ecallmgr_fs_channel:to_json(Channel)),
		lager:debug("Got channel JSON: ~p", [ChannelJSON]),
		Acc ++ [ChannelJSON] end,
		[], Details).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_conference_summary(cb_context:context()) -> cb_context:context().
load_conference_summary(Context) ->
	log_nouns(cb_context:req_nouns(Context)),
    case lists:nth(2, cb_context:req_nouns(Context)) of
        {<<"users">>, [UserId]} ->
            Filter = fun(J, A) ->
                             normalize_users_results(J, A, UserId)
                     end,
            crossbar_doc:load_view(?CB_LIST, [], Context, Filter);
        {?WH_ACCOUNTS_DB, _} ->
            crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
        _ ->
            cb_context:add_system_error(faulty_request, Context)
    end.
    
log_nouns(Nouns) ->
	lager:info("Nouns log: " ++ Nouns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new conference document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_conference(cb_context:context()) -> cb_context:context().
create_conference(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a conference document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_conference(ne_binary(), cb_context:context()) -> cb_context:context().
load_conference(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing conference document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_conference(ne_binary(), cb_context:context()) -> cb_context:context().
update_conference(DocId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_value(<<"pvt_type">>, <<"conference">>, cb_context:doc(Context))
                      );
on_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_users_results(wh_json:object(), wh_json:objects(), ne_binary()) ->
                                     api_objects().
normalize_users_results(JObj, Acc, UserId) ->
    case wh_json:get_value([<<"value">>, <<"owner_id">>], JObj) of
        'undefined' -> normalize_view_results(JObj, Acc);
        UserId -> normalize_view_results(JObj, Acc);
        _ -> Acc
    end.
