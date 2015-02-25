%% @author root
%% @doc @todo Add description to kazoo_oauth_util.


-module(kazoo_oauth_service).

-include("kazoo_oauth.hrl").

-export([service_token/2, service_token/3]).


-spec service_token(api_binary() | oauth_service_app(), api_binary()) -> api_object().
service_token('undefined', _) ->
    'undefined';
service_token(_, 'undefined') ->
    'undefined';
service_token(AppId, Scopes) when is_binary(AppId) ->
    service_token(AppId, Scopes, <<"2.0">>);
service_token(ServiceApp, Scopes) ->
    service_token(ServiceApp, Scopes, <<"2.0">>).

service_token(ProviderId, {ConsumerKey, ConsumerSecret}, <<"1.0a">>) ->
    {ok, #oauth_provider{auth_url=URL}} = kazoo_oauth_util:get_oauth_provider(ProviderId),
    {MegaSecs, Secs, _} = os:timestamp(),
    ConsumerKey2 = binary_to_list(ConsumerKey),
    Nonce = binary_to_list(base64:encode(crypto:rand_bytes(32))),
    Timestamp = integer_to_list(MegaSecs * 1000000 + Secs),
    Params = [{"oauth_consumer_key", ConsumerKey2},
              %{"oauth_consumer_key", "WEhGuJZWUasHg"},
              {"oauth_nonce", Nonce},
              %{"oauth_nonce", "zSs4RFI7lakpADpSsv"},
              {"oauth_signature_method", "HMAC-SHA1"},
              {"oauth_timestamp", Timestamp},
              %{"oauth_timestamp", "1330442419"},
              {"oauth_version", "1.0"}],
    NormalizedParams = lists:foldl(fun({K,V}, Acc) ->
        [{wh_util:uri_encode(K), wh_util:uri_encode(V)}] ++ Acc end,
        [], Params),
    ParamsString = wh_util:uri_encode(string:join(lists:foldl(fun({K,V}, Acc) ->
        [K ++ "=" ++ V] ++ Acc end,
        [], NormalizedParams), "&")),
    SigBaseString = "GET&" ++ wh_util:to_list(URL) ++ "&" ++ ParamsString,
    Signature = base64:encode(crypto:sha_mac(binary_to_list(ConsumerSecret) ++ "&", SigBaseString)),
    
    SendParams = lists:keysort(1, [{"oauth_signature", binary_to_list(Signature)}] ++ Params),
    NormalizedSendParams = lists:foldl(fun({K,V}, Acc) ->
        [{wh_util:uri_encode(K), wh_util:uri_encode(V)}] ++ Acc end,
        [], SendParams),
    ParamsSendString = string:join(lists:foldl(fun({K,V}, Acc) ->
        [K ++ "=" ++ V] ++ Acc end,
        [], NormalizedSendParams), "&"),
    FinalURL = wh_util:to_list(URL) ++ "?" ++ ParamsSendString,
    
    case ibrowse:send_req(FinalURL, [], get, []) of
        {ok, "200", _RespHeaders, RespXML} ->
            RespXML;
        {_, Code, _, _} ->
            {Code, Signature}
    end;
service_token(AppId, Scopes, <<"2.0">>) when is_binary(AppId) ->
    lager:debug("getting service app"),
    case kazoo_oauth_util:get_oauth_service_app(AppId) of
        {'ok', App } -> service_token(App, Scopes);
        {'error', Error} ->
            lager:debug("service token ~p",[Error]),'undefined'
    end;
service_token(#oauth_service_app{private_key=_PrivateKey
                                 ,provider=#oauth_provider{auth_url=URL}
                                }=ServiceApp, Scopes, <<"2.0">>) ->
    Assertion = kazoo_oauth_util:jwt(ServiceApp, Scopes),
    GrantType = wh_util:to_list(wh_util:uri_encode(?OAUTH_GRANT_TYPE)),
    Headers = [{"Content-Type","application/x-www-form-urlencoded"}
               ,{"User-Agent", "Kazoo"}
              ],
    Fields = [{"grant_type", GrantType}
              ,{"assertion", wh_util:to_list(wh_util:uri_encode(Assertion))}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=") ] end, Fields)),"&"),
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'post', Body) of
        {'ok', "200", _RespHeaders, RespXML} ->
            wh_json:decode(RespXML);
        _Else ->
            lager:debug("unable to request service token: ~p", [_Else]),
            'undefined'
    end.
