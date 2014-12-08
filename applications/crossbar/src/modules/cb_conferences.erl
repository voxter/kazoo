%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
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
resource_exists() ->
    true.
resource_exists(_) ->
    true.
resource_exists(_, <<"details">>) ->
	true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_conference_summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create_conference(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    load_conference(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update_conference(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    load_conference(Id, Context).
    
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ConfId, <<"details">>) ->
    conference_details(Context, ConfId).
    
%%
%% Returns some details about the users connected to a conference
%%
conference_details(Context, ConfId) ->
	log_context(Context),
    lookup_confs(Context, ConfId).
    
lookup_confs(Context, ConfId) ->
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
        	crossbar_doc:handle_json_success(lookup_participants(JObjs), Context)
            %merge_responses(JObjs)
    end.
    
lookup_participants(JObjs) ->
	lager:debug("Looking up participants of conference"),
	Data = wh_json:get_value(<<"Participants">>, hd(JObjs)),
	CallUUIDs = get_call_uuids(Data),
	get_channel_details(CallUUIDs).
	%get_users_details(get_participants_details(PartData)).
	%get_users_details(PartData).
	%get_callers_details(PartData).
	
get_call_uuids(undefined) -> [];
get_call_uuids(Participants) ->
	lists:foldl(fun(Participant, Acc) ->
		CallID = wh_json:get_value(<<"Call-ID">>, Participant),
		lager:debug("Call-ID found: ~p", [CallID]),
		Acc ++ [CallID] end,
		[], Participants).
		
get_channel_details(UUIDs) ->
	lists:foldl(fun(UUID, Acc) ->
		Channel = hd(element(1, rpc:call('ecallmgr@awe01.tor1.voxter.net', ecallmgr_fs_channels, get_channels, [UUID]))),
		ChannelJSON = ecallmgr_fs_channel:to_json(Channel),
		lager:debug("Got channel JSON: ~p", [ChannelJSON]),
		Acc ++ [ChannelJSON] end,
		[], UUIDs).
	
%get_participants_details(Participants) ->
%	[get_participant_details(P) || P <- Participants].

%get_participant_details(Participant) ->
%	Req = [{<<"Call-ID">>, Participant}
%           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
%          ],
%    ReqResp = whapps_util:amqp_pool_collect(Req
%                                            ,fun wapi_call:publish_channel_status_req/1
%                                            ,{'ecallmgr', 'true'}
%                                           ),
%    case ReqResp of
%        {'error', _} -> [];
%        {_, JObjs} ->
%        	lager:debug("Got these participant details: ~p", [hd(JObjs)]),
%        	hd(JObjs)
%    end.
    
%get_users_details(Participants) ->
%	[get_user_details(P) || P <- Participants].
	
%get_user_details(Participant) ->
%	lager:info("Trying to retrieve ~p", [Participant]),
%	whapps_call:to_json(element(2, whapps_call:retrieve(Participant, <<"callflow">>))).
	
%get_callers_details(Users) ->
%	[get_caller_details(P) || P <- Users].
	
%get_caller_details(User) ->
%	EmptyChannel = ecallmgr:channel()
%	MatchSpec = [{#channel{uuid='$1', _ = '_'}
%                  ,[{'=:=', '$1', {'const', UUID}}]
%                  ,['$_']
%                 }],
%    print_details(ets:select(?CHANNELS_TBL, MatchSpec, 1))
	
log_context(#cb_context{req_nouns=Nouns}) ->
	lists:foreach(fun(Noun) ->
			lager:info("Context log: " ++ element(1, Noun))
		end,
		Nouns
	).

-spec post(#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put(#cb_context{}) -> #cb_context{}.
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
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
-spec load_conference_summary(#cb_context{}) -> #cb_context{}.
load_conference_summary(#cb_context{req_nouns=Nouns}=Context) ->
	log_context(Context),
	log_nouns(Nouns),
    case lists:nth(2, Nouns) of
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
-spec create_conference(#cb_context{}) -> #cb_context{}.
create_conference(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(undefined, C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a conference document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_conference(ne_binary(), #cb_context{}) -> #cb_context{}.
load_conference(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing conference document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_conference(ne_binary(), #cb_context{}) -> #cb_context{}.
update_conference(DocId, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation('undefined' | ne_binary(), #cb_context{}) -> #cb_context{}.
on_successful_validation(undefined, #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"conference">>, JObj)};
on_successful_validation(DocId, #cb_context{}=Context) ->
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
                                          ['undefined' | wh_json:object(),...] | [].
normalize_users_results(JObj, Acc, UserId) ->
    case wh_json:get_value([<<"value">>, <<"owner_id">>], JObj) of
        undefined -> normalize_view_results(JObj, Acc);
        UserId -> normalize_view_results(JObj, Acc);
        _ -> [undefined|Acc]
    end.
