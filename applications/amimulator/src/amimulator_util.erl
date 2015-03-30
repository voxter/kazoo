-module(amimulator_util).

-include("amimulator.hrl").

-export([parse_payload/1, format_prop/1, format_binary/1, format_json_events/1, initial_calls/1, 
    endpoint_exten/2, whapps_call/1,
    maybe_get_exten/1, maybe_get_endpoint_name/1, endpoint_name/2, maybe_get_cid_name/1,
    find_id_number/2, queue_for_number/2,
    filter_registered_events/4, whapps_call_from_cf_exe/1, channel_tail/1]).

%% AMI commands broken up by newlines
parse_payload(Payload) ->
    Lines = filter_empty(binary:split(Payload, <<"\r\n">>, [global])),
    lists:foldl(fun(Parameter, Acc) ->
        KV = binary:split(Parameter, <<":">>),
        {K, V} = {lists:nth(1, KV), lists:nth(2, KV)},
        Prop = {K, binary:replace(V, <<" ">>, <<>>)},
        [Prop] ++ Acc
        end, [], Lines).
    
%% Eliminates trailing lines from client payload
filter_empty(Parameters) ->
    lists:foldl(fun(Param, Acc) ->
        case Param of
            <<>> ->
                Acc;
            _ ->
                [Param] ++ Acc
        end end, [], Parameters).

%% Recursive proplist formatting for writes to socket
format_prop({V}) ->
    <<(wh_util:to_binary(V))/binary, "\r\n">>;
format_prop({K, V}) ->
    <<(wh_util:to_binary(K))/binary, ": ", (wh_util:to_binary(V))/binary, "\r\n">>.

format_binary([KV|Rest]) ->
    Head = format_prop(KV),
    Tail = format_binary(Rest),
    <<Head/binary, Tail/binary>>;
format_binary([]) ->
    <<"\r\n">>.

%% Format a set of events for publishing to AMQP
format_json_events(Events) ->
    format_json_events(Events, []).

format_json_events([], Acc) ->
    Acc;
format_json_events([{_K, _V}|_Other]=KVs, _Acc) ->
    [{KVs}];
format_json_events([Event|Events], Acc) ->
    format_json_events(Events, Acc ++ [{Event}]).





%% Fetches all whapps calls for an account and adds them to the data store
initial_calls(AccountId) ->
    Req = [
        {<<"Account-ID">>, AccountId},
        {<<"Active-Only">>, 'true'}
        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ],
    case wh_amqp_worker:call_collect(
        Req,
        fun wapi_call:publish_query_account_channels_req/1,
        {'ecallmgr', fun wapi_call:query_account_channels_resp_v/1}
    ) of
        {'ok', RespJObjs} ->
            %% Store the channels as raw data in the amimulator_store first of all
            %% They will be used for data lookups
            lists:foreach(fun(RespJObj) ->
                Channels = wh_json:get_value(<<"Channels">>, RespJObj),
                case Channels of
                    undefined -> ok;
                    _ -> initialize_datastore(Channels)
                end
            end, RespJObjs),

            %% Now we can produce all the channels and update the amimulator_store
            lists:foreach(fun(RespJObj) ->
                Channels = wh_json:get_value(<<"Channels">>, RespJObj),
                case Channels of
                    undefined -> ok;
                    _ -> calls_from_channel_json(Channels)
                end
            end, RespJObjs),

            %% Finally, make these calls not suck
            lists:foldl(fun(RespJObj, Acc) ->
                Channels = wh_json:get_value(<<"Channels">>, RespJObj),
                case Channels of
                    undefined -> Acc;
                    _ -> better_calls(Channels) ++ Acc
                end
            end, [], RespJObjs);
        E ->
            lager:debug("Could not get channel statuses: ~p", [E])
    end.

initialize_datastore(ChannelJObjs) ->
    lists:foreach(fun(ChannelJObj) ->
        Key = "channel-" ++ wh_util:to_list(wh_json:get_value(<<"uuid">>, ChannelJObj)),
        case amimulator_store:get(Key) of
            undefined ->
                amimulator_store:put(Key, ChannelJObj);
            _ ->
                ok
        end
    end, ChannelJObjs).

calls_from_channel_json(ChannelJObjs) ->
    lists:foreach(fun(ChannelJObj) ->
        Routines = [
            fun(Call) -> whapps_call:set_authorizing_id(wh_json:get_value(<<"authorizing_id">>, ChannelJObj), Call) end,
            fun(Call) -> whapps_call:set_account_id(wh_json:get_value(<<"account_id">>, ChannelJObj), Call) end,
            fun(Call) -> whapps_call:set_account_db(wh_util:format_account_id(whapps_call:account_id(Call), encoded), Call) end,
            fun(Call) -> whapps_call:set_call_id(wh_json:get_value(<<"uuid">>, ChannelJObj), Call) end,
            fun(Call) -> whapps_call:set_other_leg_call_id(wh_json:get_value(<<"other_leg">>, ChannelJObj), Call) end
        ],
        Call = lists:foldl(fun(F, Call) -> F(Call) end, whapps_call:new(), Routines),
        Key = "whapps_call-" ++ wh_util:to_list(whapps_call:call_id(Call)),
        case amimulator_store:get(Key) of
            undefined ->
                amimulator_store:put(Key, Call);
            _ ->
                ok
        end
    end, ChannelJObjs).

%% Adds a whole bunch of extra data to make whapps_calls more useful
better_calls(ChannelJObjs) ->
    lists:foldl(fun(ChannelJObj, Calls) ->
        Call = amimulator_store:get("whapps_call-" ++ wh_util:to_list(wh_json:get_value(<<"uuid">>, ChannelJObj))),
        Routines = [
            fun aleg_cid/2,
            fun aleg_exten/2,
            fun aleg_ami_channel/2,
            fun bleg_cid/2,
            fun bleg_ami_channel/2,
            fun(Call2, ChannelJObj2) -> props:set_value(<<"username">>, wh_json:get_value(<<"username">>, ChannelJObj2), Call2) end,
            fun(Call2, ChannelJObj2) -> props:set_value(<<"answered">>, wh_json:get_value(<<"answered">>, ChannelJObj2), Call2) end,
            fun(Call2, ChannelJObj2) -> props:set_value(<<"elapsed_s">>, wh_json:get_value(<<"elapsed_s">>, ChannelJObj2), Call2) end
        ],
        [lists:foldl(
            fun(F, BCall) -> F(BCall, ChannelJObj) end,
            [{<<"call">>, Call}],
            Routines
        )] ++ Calls
    end, [], ChannelJObjs).

aleg_cid(Call, _ChannelJObj) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    case cf_endpoint:get(WhappsCall) of
        %% An external endpoint
        {error, _E} ->
            %% TODO
            <<"todo">>;
        %% Some internal extension
        {ok, Endpoint} ->
            props:set_value(<<"aleg_cid">>, endpoint_cid(Endpoint, whapps_call:account_db(WhappsCall)), Call)
    end.

aleg_exten(Call, _ChannelJObj) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    case cf_endpoint:get(WhappsCall) of
        %% An external endpoint
        {error, _E} ->
            %% TODO
            <<"todo">>;
        %% Some internal extension
        {ok, Endpoint} ->
            props:set_value(<<"aleg_exten">>, endpoint_exten(Endpoint, whapps_call:account_db(WhappsCall)), Call)
    end.

aleg_ami_channel(Call, _ChannelJObj) ->
    WhappsCall = props:get_value(<<"call">>, Call),
    case cf_endpoint:get(WhappsCall) of
        %% An external endpoint
        {error, _E} ->
            %% TODO
            <<"todo">>;
        %% Some internal extension
        {ok, Endpoint} ->
            props:set_value(<<"aleg_ami_channel">>, endpoint_channel(Endpoint, whapps_call:account_db(WhappsCall), whapps_call:call_id(WhappsCall)), Call)
    end.

bleg_cid(Call, ChannelJObj) ->
    case amimulator_store:get("whapps_call-" ++ wh_util:to_list(wh_json:get_value(<<"other_leg">>, ChannelJObj))) of
        undefined ->
            %% TODO, find the call somehow
            <<"todo">>;
        OtherCall ->
            props:set_value(<<"bleg_cid">>,
                props:get_value(<<"aleg_cid">>, aleg_cid([{<<"call">>, OtherCall}], undefined)), Call)
    end.

bleg_ami_channel(Call, ChannelJObj) ->
    case amimulator_store:get("whapps_call-" ++ wh_util:to_list(wh_json:get_value(<<"other_leg">>, ChannelJObj))) of
        undefined ->
            %% TODO, find the call somehow
            <<"todo">>;
        OtherCall ->
            props:set_value(<<"bleg_ami_channel">>,
                props:get_value(<<"aleg_ami_channel">>, aleg_ami_channel([{<<"call">>, OtherCall}], undefined)), Call)
    end.

endpoint_cid(Endpoint, AccountDb) ->
    case wh_json:get_value(<<"owner_id">>, Endpoint) of
        undefined ->
            wh_json:get_value(<<"name">>, Endpoint);
        OwnerId ->
            {ok, Owner} = couch_mgr:open_doc(AccountDb, OwnerId),
            <<(wh_json:get_value(<<"first_name">>, Owner))/binary, " ",
                (wh_json:get_value(<<"last_name">>, Owner))/binary, " <",
                (wh_json:get_value(<<"username">>, Owner))/binary, ">">>
    end.

endpoint_channel(Endpoint, AccountDb, CallId) ->
    <<"SIP/", (endpoint_exten(Endpoint, AccountDb))/binary, "-", (channel_tail(CallId))/binary>>.

endpoint_exten(Endpoint, AccountDb) ->
    case wh_json:get_value(<<"owner_id">>, Endpoint) of
        undefined ->
            wh_json:get_value(<<"name">>, Endpoint);
        OwnerId ->
            {ok, Owner} = couch_mgr:open_doc(AccountDb, OwnerId),
            <<(wh_json:get_value(<<"username">>, Owner))/binary>>
    end.





whapps_call(CallRef) ->
    Call = case wh_json:is_json_object(CallRef) of
        true ->
            whapps_call_from_json(CallRef);
        false ->
            whapps_call_from_ecallmgr(CallRef)
    end,
    Updaters = [
        fun(Call2) -> case whapps_call:account_id(Call2) of
            undefined ->
                Call2;
            AccountId ->
                whapps_call:set_account_db(wh_util:format_account_id(AccountId, encoded), Call2)
        end end
    ],
    lists:foldl(fun(F, Call2) -> F(Call2) end, Call, Updaters).

whapps_call_from_json(EventJObj) ->
    Call = whapps_call:from_json(EventJObj),
    CCVs = whapps_call:ccvs(Call),
    Call2 = case wh_json:get_value(<<"Authorizing-ID">>, CCVs) of
        undefined ->
            Call;
        AuthId ->
            whapps_call:set_authorizing_id(AuthId, Call)
    end,
    case wh_json:get_value(<<"Account-ID">>, CCVs) of
        undefined ->
            Call2;
        AccountId ->
            whapps_call:set_account_id(AccountId, Call2)
    end.

whapps_call_from_ecallmgr(CallId) ->
    case ecallmgr_rpc(ecallmgr_fs_channel, fetch, [CallId, record]) of
        {error, E} ->
            lager:debug("Error ~p when fetching channel from ecallmgr", [E]),
            error;
        {ok, Channel} ->
            whapps_call:from_json(wh_json:from_list(ecallmgr_fs_channel:to_api_props(Channel)))
    end.

ecallmgr_rpc(Mod, Fun, Params) ->
    {ok, Shortname} = inet:gethostbyname(element(2, inet:gethostname())),
    case element(2, Shortname) of
        undefined ->
            {error, undefined_hostname};
        Hostname ->
            RemoteNode = wh_util:to_atom("ecallmgr@" ++ Hostname),
            Result = rpc:call(RemoteNode, Mod, Fun, Params),
            % TODO: disconnect from all new nodes
            erlang:disconnect_node(RemoteNode),
            Result
    end.

maybe_get_exten(Call) ->
    case cf_endpoint:get(Call) of
        %% An external endpoint
        {error, _E} ->
            whapps_call:callee_id_name(Call);
        %% Some internal extension
        {ok, Endpoint} ->
            endpoint_name(whapps_call:account_db(Call), Endpoint)
    end.

maybe_get_endpoint_name(Call) ->
    Exten = maybe_get_exten(Call),
    <<"SIP/", Exten/binary, "-", (channel_tail(whapps_call:call_id(Call)))/binary>>.

endpoint_name(AcctDb, Endpoint) ->
    case wh_json:get_value(<<"pvt_type">>, Endpoint) of
        <<"device">> ->
            {ok, EndpointDevice} = couch_mgr:open_doc(AcctDb, wh_json:get_value(<<"_id">>, Endpoint)),
            wh_json:get_value(<<"name">>, EndpointDevice);
        _ ->
            wh_json:get_value(<<"name">>, Endpoint)
    end.

maybe_get_cid_name(Call) ->
    case cf_endpoint:get(Call) of
        %% An external endpoint
        {error, _E} ->
            whapps_call:callee_id_name(Call);
        %% Some internal extension
        {ok, Endpoint} ->
            cid_name(whapps_call:account_db(Call), Endpoint)
    end.

cid_name(AcctDb, Endpoint) ->
    case wh_json:get_value(<<"owner_id">>, Endpoint) of
        undefined ->
            wh_json:get_value(<<"name">>, Endpoint);
        OwnerId ->
            {ok, Owner} = couch_mgr:open_doc(AcctDb, OwnerId),
            <<(wh_json:get_value(<<"first_name">>, Owner))/binary, " ",
                (wh_json:get_value(<<"last_name">>, Owner))/binary, " <",
                (wh_json:get_value(<<"username">>, Owner))/binary, ">">>
    end.

find_id_number(Id, AccountDb) ->
    {ok, Results} = couch_mgr:get_results(AccountDb, <<"callflows/crossbar_listing">>),
    maybe_id_in_callflows(Id, Results, AccountDb).

maybe_id_in_callflows(_, [], _) ->
    {error, not_found};
maybe_id_in_callflows(Id, [Result|Results], AccountDb) ->
    CFId = wh_json:get_value(<<"id">>, Result),
    case maybe_id_in_callflow(Id, CFId, AccountDb) of
        false ->
            maybe_id_in_callflows(Id, Results, AccountDb);
        Number ->
            {ok, Number}
    end.

maybe_id_in_callflow(Id, CFId, AccountDb) ->
    {ok, CFDoc} = couch_mgr:open_doc(AccountDb, CFId),
    case maybe_id_in_callflow(Id, wh_json:get_value(<<"flow">>, CFDoc)) of
        false ->
            false;
        true ->
            hd(wh_json:get_value(<<"numbers">>, CFDoc))
    end.
    
maybe_id_in_callflow(Id, Flow) ->
    Data = wh_json:get_value(<<"data">>, Flow),
    %% Skipping queue login and queue logout possibilities
    case {wh_json:get_value(<<"id">>, Data), wh_json:get_value(<<"module">>, Flow)} of
        {_, <<"acdc_queue">>} ->
            recurse_to_child_callflow(Id, Flow);
        {Id, _} ->
            true;
        _ ->
            recurse_to_child_callflow(Id, Flow)
    end.

recurse_to_child_callflow(Id, Flow) ->
    Children = wh_json:get_value(<<"children">>, Flow),
    case wh_json:get_value(<<"_">>, Children) of
        undefined ->
            false;
        SubFlow ->
            maybe_id_in_callflow(Id, SubFlow)
    end.

queue_for_number(Number, AccountDb) ->
    case couch_mgr:get_results(AccountDb, <<"callflow/listing_by_number">>, [{key, Number}]) of
        {ok, []} ->
            {error, number_not_found};
        {ok, [Result]} ->
            {ok, CFDoc} = couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"id">>, Result)),
            case maybe_queue_in_flow(wh_json:get_value(<<"flow">>, CFDoc)) of
                {error, E} ->
                    {error, E};
                {ok, QueueId} ->
                    couch_mgr:open_doc(AccountDb, QueueId)
            end
    end.
    
maybe_queue_in_flow(Flow) ->
    case wh_json:get_value(<<"module">>, Flow) of
        <<"acdc_member">> ->
            {ok, wh_json:get_value(<<"id">>, wh_json:get_value(<<"data">>, Flow))};
        _ ->
            case wh_json:get_value(<<"_">>, wh_json:get_value(<<"flow">>, Flow)) of
                undefined ->
                    {error, invalid_queue_extension};
                SubFlow ->
                    maybe_queue_in_flow(SubFlow)
            end
    end.

%% Ensures the message belongs to the current listener process
%% and publishes it to the Mod:handle_specific_event/2 function
%% in the Mod supplied
filter_registered_events(EventName, EventJObj, CommPid, Mod) ->
    AccountId = gen_server:call(CommPid, account_id),
    case wh_json:get_value(
        [<<"Custom-Channel-Vars">>, <<"Account-ID">>],
        EventJObj
    ) of
        %% Maybe sometimes, the events come in via different formats
        undefined ->
            lager:debug("May need to add additional account id check for event! ~p", [EventJObj]);
        %% Event is destined to be published!
        AccountId ->
            Mod:handle_specific_event(EventName, EventJObj);
        %% Event does not belong to this listener
        _ ->
            ok
    end.

%% Look through active calls and find the cf_exe process with the desired call ID
whapps_call_from_cf_exe(CallId) ->
    whapps_call_from_cf_exe(CallId, cf_exe_sup:workers()).

whapps_call_from_cf_exe(_CallId, []) ->
    not_found;
whapps_call_from_cf_exe(CallId, [Worker|Workers]) ->
    case cf_exe:get_call(Worker) of
        {ok, Call} ->
            Call;
        _ ->
            whapps_call_from_cf_exe(CallId, Workers)
    end.

%% Returns an 8-digit tail for channels for AMI calls
channel_tail(CallId) ->
    HeadLength = (byte_size(CallId)-8)*8,
    <<_:HeadLength, Tail:64>> = CallId,
    tail_convert(binary_to_list(<<Tail:64>>), []).

tail_convert([], Acc) ->
    wh_util:to_binary(Acc);
tail_convert([Char|Chars], Acc) when Char < 58, Char > 47 ->
    tail_convert(Chars, [Char] ++ Acc);
tail_convert([Char|Chars], Acc) when Char < 91, Char > 64 ->
    tail_convert(Chars, [((Char - 17) rem 10) + 48] ++ Acc);
tail_convert([Char|Chars], Acc) when Char < 123, Char > 96 ->
    tail_convert(Chars, [((Char - 49) rem 10) + 48] ++ Acc);
tail_convert([_Char|Chars], Acc) ->
    tail_convert(Chars, [48] ++ Acc).











