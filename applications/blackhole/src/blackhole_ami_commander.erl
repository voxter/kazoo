-module(blackhole_ami_commander).

-export([login/1, handle/2]).

-include("blackhole.hrl").

-define(AMI_DB, <<"ami">>).
    
%% Essentially handles payload for start-of-connect auth
login(Payload) ->
    Parameters = parse_payload(Payload),
    handle_event(Parameters).

%% Handle a payload sent as an AMI command
handle(Payload, AccountId) ->
    Parameters = parse_payload(Payload),
    UpdatedParams = [<<"Account: ", AccountId/binary>>] ++ Parameters,
    handle_event(UpdatedParams).
    
%% AMI commands broken up by newlines
parse_payload(Payload) ->
    binary:split(Payload, <<"\r\n">>).
    
handle_event(Parameters) ->
    Props = lists:foldl(fun(Parameter, Acc) ->
        {K,V} = binary:split(Parameter, <<":">>),
        Prop = {K, binary:replace(V, <<" ">>, <<>>)},
        Prop ++ Acc
        end, [], Parameters),
    handle_event(proplists:get_value(<<"Action">>, Props), Props).
    
% TODO: add AMI username lookup db initialization
handle_event(<<"Login">>, Props) ->
    Username = proplists:get_value(<<"Username">>, Props),
    Secret = proplists:get_value(<<"Secret">>, Props),
    AMIDoc = couch_mgr:open_doc(?AMI_DB, Username),
    case wh_json:get_value(<<"Secret">>, AMIDoc) of
        Secret ->
            %% Successful login
            {ok, wh_json:get_value(<<"account_id">>, AMIDoc)};
        _ ->
            %% Failed login
            {error, invalid_creds}
    end;
handle_event(<<"Originate">>, Props) ->
    case proplists:get_value(<<"Channel">>, Props) of
        undefined ->
            {error, channel_not_specified};
        _ ->
            originate(Props)
    end.
    
originate(Props) ->
    CCVs = [{<<"Account-ID">>, proplists:get_value(<<"Account">>, Props)}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            %,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
            %,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
           ],
    MsgId = case proplists:get_value(<<"ActionID">>, Props) of
                undefined -> wh_util:rand_hex_binary(16);
                ActionID -> ActionID
            end,
    Request = [{<<"Application-Name">>, <<"blackhole-ami">>}
               %,{<<"Application-Data">>, get_application_data(Context)}
               ,{<<"Msg-ID">>, MsgId}
               %,{<<"Endpoints">>, maybe_auto_answer(Endpoints)}
               %,{<<"Timeout">>, get_timeout(Context)}
               ,{<<"Timeout">>, <<"30">>}
               %,{<<"Ignore-Early-Media">>, get_ignore_early_media(Context)}
               ,{<<"Ignore-Early-Media">>, <<"true">>}
               %,{<<"Media">>, get_media(Context)}
               ,{<<"Outbound-Caller-ID-Name">>, proplists:get_value(<<"Channel">>, Props)}
               %,{<<"Outbound-Caller-ID-Number">>, whapps_call:request_user(Call)}
               %,{<<"Outbound-Callee-ID-Name">>, get_caller_id_name(Context)}
               %,{<<"Outbound-Callee-ID-Number">>, get_caller_id_number(Context)}
               ,{<<"Outbound-Caller-ID-Number">>, proplists:get_value(<<"Channel">>, Props)}
               ,{<<"Outbound-Callee-ID-Name">>, proplists:get_value(<<"Exten">>, Props)}
               ,{<<"Outbound-Callee-ID-Number">>, proplists:get_value(<<"Exten">>, Props)}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
               %,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>]}
               | wh_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
              
    lager:debug("AMI: originate request ~p", [Request]),
    
    wapi_resource:publish_originate_req(props:filter_undefined(Request)).