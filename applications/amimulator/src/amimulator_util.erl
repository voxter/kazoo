-module(amimulator_util).

-include("amimulator.hrl").

-export([parse_payload/1, format_prop/1, format_binary/1, format_json_events/1, whapps_call/1,
    maybe_get_exten/1, maybe_get_endpoint_name/1, endpoint_name/2, find_id_number/2]).
-export([queue_for_number/2]).

%% AMI commands broken up by newlines
parse_payload(Payload) ->
    %lager:debug("AMI commander payload: ~p", [Payload]),
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
        {error, _E} ->
            % TODO: find out proper value for this EndpointName
            <<"external">>;
        {ok, Endpoint} ->
            endpoint_name(whapps_call:account_db(Call), Endpoint)
    end.

maybe_get_endpoint_name(Call) ->
    Exten = maybe_get_exten(Call),
    <<"SIP/", Exten/binary, "-00000000">>.

endpoint_name(AcctDb, Endpoint) ->
    case wh_json:get_value(<<"pvt_type">>, Endpoint) of
        <<"device">> ->
            {ok, EndpointDevice} = couch_mgr:open_doc(AcctDb, wh_json:get_value(<<"_id">>, Endpoint)),
            wh_json:get_value(<<"name">>, EndpointDevice);
        _ ->
            wh_json:get_value(<<"name">>, Endpoint)
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
    case wh_json:get_value(<<"id">>, Data) of
        Id ->
            true;
        _ ->
            Children = wh_json:get_value(<<"children">>, Flow),
            case wh_json:get_value(<<"_">>, Children) of
                undefined ->
                    false;
                SubFlow ->
                    maybe_id_in_callflow(Id, SubFlow)
            end
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








