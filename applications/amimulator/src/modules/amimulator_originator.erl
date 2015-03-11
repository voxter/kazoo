-module(amimulator_originator).
-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("../amimulator.hrl").

-record(state, {
}).

start_link() ->
    gen_listener:start_link(?MODULE
                           ,[{'bindings', []}
                            ,{'responders', []}
                            ], []).

init([]) ->
    lager:debug("AMI: Started originator for handling AMI dials ~p", [self()]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    lager:debug("AMI: unhandled call"),
    {reply, {error, not_implemented}, State}.

handle_cast({"originate", Props}, State) ->
	first_leg(Props),
	{noreply, State};
handle_cast({gen_listener, {created_queue, _QueueName}}, State) ->
    {noreply, State};
handle_cast({gen_listener, {is_consuming, _IsConsuming}}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:debug("AMI: unhandled cast"),
    {noreply, State}.

first_leg(Props) ->
  	UpdatedProps = update_props(Props),
    Call = create_call_from_props(UpdatedProps),

  	%% Lookup the endpoint for the user placing the call
  	%% Will produce originate req from this
    DestExten = proplists:get_value(<<"Exten">>, UpdatedProps),
    _SourceExten = proplists:get_value(<<"SourceExten">>, UpdatedProps),
    SourceEndpoints = get_endpoints(UpdatedProps, Call),

    CCVs = [{<<"Account-ID">>, proplists:get_value(<<"Account">>, UpdatedProps)}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
            ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
           ],
    MsgId = case proplists:get_value(<<"ActionID">>, UpdatedProps) of
                undefined -> wh_util:rand_hex_binary(16);
                ActionID -> ActionID
            end,
    %MsgId = case wh_util:is_empty(cb_context:req_id(Context)) of
    %            'true' -> wh_util:rand_hex_binary(16);
    %            'false' -> cb_context:req_id(Context)
    %        end,
    Request = [{<<"Application-Name">>, <<"transfer">>}
               ,{<<"Application-Data">>, wh_json:from_list([{<<"Route">>, DestExten}])}
               ,{<<"Msg-ID">>, MsgId}
               ,{<<"Endpoints">>, SourceEndpoints}
               ,{<<"Timeout">>, <<"30">>}
               ,{<<"Ignore-Early-Media">>, <<"true">>}
               ,{<<"Media">>, <<"process">>}
               ,{<<"Outbound-Caller-ID-Name">>, <<"Web Dial ", DestExten/binary>>}
               ,{<<"Outbound-Caller-ID-Number">>, DestExten}
               ,{<<"Outbound-Callee-ID-Name">>, whapps_call:to(Call)}
               ,{<<"Outbound-Callee-ID-Number">>, whapps_call:to(Call)}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
               | wh_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
              
    lager:debug("AMI: originate request ~p", [Request]),
    wapi_resource:publish_originate_req(props:filter_undefined(Request)).

%% Augment props received via AMI with additional data for call origination
update_props(Props) ->
    AccountId = proplists:get_value(<<"Account">>, Props),
    Routines = [
        fun(Props2) -> [{<<"AccountDb">>, wh_util:format_account_id(AccountId, 'encoded')}] ++ Props2 end,
        fun(Props2) ->
            [{<<"SourceExten">>, binary:replace(proplists:get_value(<<"Channel">>, Props2), <<"SIP/">>, <<"">>)}]
            ++ Props2 end
    ],
    lists:foldl(fun(F, Props2) -> F(Props2) end, Props, Routines).

%% Using the props computed from Originate action, establish a whapps_call
create_call_from_props(Props) ->
    Routines = [
        fun(C) -> whapps_call:set_account_db(proplists:get_value(<<"AccountDb">>, Props), C) end,
        fun(C) -> whapps_call:set_account_id(proplists:get_value(<<"Account">>, Props), C) end,
        %fun(C) ->
        %                 case wh_json:get_ne_value(<<"owner_id">>, cb_context:doc(Context)) of
        %                     'undefined' -> C;
        %                     OwnerId -> whapps_call:set_owner_id(OwnerId, C)
        %                 end
        %         end
        fun(C) -> maybe_assign_aleg_props(Props, C) end
    ],
    lists:foldl(fun(F, C) -> F(C) end, whapps_call:new(), Routines).

%% If we can get the authorizing id from the originating channel, set some extra props
maybe_assign_aleg_props(Props, Call) ->
    case aleg_authorizing_id(Props) of
        {error, E} ->
            lager:debug("AMI: origination could not find aleg authorizing id (~p)", [E]),
            Call;
        {ok, AuthorizingId} ->
            assign_aleg_props(AuthorizingId, Props, Call)
    end.

%% Set extra props for originating channel
assign_aleg_props(AuthorizingId, Props, Call) ->
    To = proplists:get_value(<<"SourceExten">>, Props),
    Routines = [
        fun(C) -> whapps_call:set_authorizing_id(AuthorizingId, C) end,
        fun(C) -> whapps_call:set_authorizing_type(<<"user">>, C) end,
        fun(C) -> whapps_call:set_request(<<To/binary, "@blackholeami">>, C) end,
        fun(C) -> whapps_call:set_to(<<To/binary, "@blackholeami">>, C) end
    ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

%% _id from couch for destination endpoint
aleg_authorizing_id(Props) ->
    ViewOptions = [{key, proplists:get_value(<<"SourceExten">>, Props)}],
    case couch_mgr:get_results(proplists:get_value(<<"AccountDb">>, Props), <<"users/list_by_username">>, ViewOptions) of
        {ok, []} ->
            {error, endpoint_not_found};
        {ok, [Result]} ->
            {ok, wh_json:get_value(<<"id">>, Result)}
    end.

%% Find the endpoints associated with the user placing the originate request
get_endpoints(Props, Call) ->
    UserId = whapps_call:authorizing_id(Call),
    Number = proplists:get_value(<<"SourceExten">>, Props),
    Properties = wh_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                    ,{<<"source">>, 'cb_users'}
                                   ]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Properties, aleg_cid(Number, Call)) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end, [], cf_attributes:owned_by(UserId, <<"device">>, Call)).

%% Caller ID properties for call coming from originate request
aleg_cid(Number, Call) ->
    Routines = [fun(C) -> whapps_call:set_custom_channel_var(<<"Retain-CID">>, <<"true">>, C) end
                ,fun(C) -> whapps_call:set_caller_id_name(wh_util:to_binary(Number), C) end
                ,fun(C) -> whapps_call:set_caller_id_number(wh_util:to_binary(Number), C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

handle_info(_Info, State) ->
    lager:debug("AMI: unhandled info"),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

terminate(Reason, _State) ->
    lager:debug("AMI: Originator on pid ~p terminating: ~p", [self(), Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
