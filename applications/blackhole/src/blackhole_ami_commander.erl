-module(blackhole_ami_commander).

-export([login/1, handle/2, filter_empty/1]).

-include("blackhole.hrl").

-define(AMI_DB, <<"ami">>).
    
%% Essentially handles payload for start-of-connect auth
%login(Payload) ->
%    Parameters = parse_payload(Payload),
%    handle_event(Parameters).

%% Handle a payload sent as an AMI command
handle(Payload, AccountId) ->
    Props = blackhole_ami_util:parse_payload(Payload),
    UpdatedProps = [{<<"Account">>, AccountId}] ++ Props,
    handle_event(UpdatedProps).
    
handle_event(Props) ->
    %lager:debug("AMI event params: ~p", [Parameters]),
    Action = string:to_lower(wh_util:to_list(proplists:get_value(<<"Action">>, Props))),
    handle_event(Action, Props).
    
% TODO: add AMI username lookup db initialization
% TODO: validate md5 key on login
% TODO: validate secret mode login (secret in TCP payload)
handle_event("login", Props) ->
    Username = proplists:get_value(<<"Username">>, Props),
    %Secret = proplists:get_value(<<"Secret">>, Props),
    Secret = undefined,
    ActionID = proplists:get_value(<<"ActionID">>, Props),
    _AMIDoc = couch_mgr:open_doc(?AMI_DB, Username),
    %case wh_json:get_value(<<"Secret">>, AMIDoc) of
    case Secret of
        undefined ->
            %% Successful login
            %{ok, wh_json:get_value(<<"account_id">>, AMIDoc)};
            Payload = [
                {<<"Response">>, <<"Success">>},
                {<<"ActionID">>, ActionID},
                {<<"Message">>, <<"Authentication accepted">>}
            ],
            {ok, {Payload, broken}};
            
            % TODO trigger a FullyBooted event
        _ ->
            %% Failed login
            Payload = [
                {<<"Response">>, <<"Error">>},
                {<<"ActionID">>, ActionID},
                {<<"Message">>, <<"Authentication failed">>}
            ],
            {ok, {Payload, n}}
    end;
handle_event("challenge", Props) ->
    Challenge = random:uniform(899999999) + 100000000,
    ActionID = proplists:get_value(<<"ActionID">>, Props),
    Payload = [
        {<<"Asterisk Call Manager/1.1">>},
        {<<"Response">>, <<"Success">>},
        {<<"Challenge">>, Challenge},
        {<<"ActionID">>, ActionID}
    ],
    {ok, Payload};
handle_event("originate", Props) ->
    case proplists:get_value(<<"Channel">>, Props) of
        undefined ->
            {error, channel_not_specified};
        _ ->
            originate(Props)
    end;
handle_event(undefined, Props) ->
    lager:debug("AMI commander undefined action with props ~p", [Props]),
    {error, no_action}.
    
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