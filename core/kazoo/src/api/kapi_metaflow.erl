%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Metaflow requests, responses, and errors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapi_metaflow).

-export([req/1, req_v/1]).
-export([update/1, update_v/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_req/1, publish_req/2]).
-export([publish_update/1, publish_update/2]).

-include_lib("kazoo/include/kz_api.hrl").

%% Metaflow Request - when streaming is needed
-define(METAFLOW_REQ_HEADERS, [<<"Action">>, <<"Call-ID">>]).
-define(OPTIONAL_METAFLOW_REQ_HEADERS, [<<"Data">>]).
-define(METAFLOW_REQ_VALUES, [{<<"Event-Category">>, <<"metaflow">>}
                              ,{<<"Event-Name">>, <<"req">>}
                              ,{<<"Action">>, [<<"transfer">>, <<"hangup">>, <<"callflow">>, <<"break">>]}
                             ]).
-define(METAFLOW_REQ_TYPES, []).

-define(METAFLOW_REQ_ROUTING_KEY(CallId, Action)
        ,<<"metaflow.", (amqp_util:encode(CallId))/binary, ".", (Action)/binary>>
       ).

-define(METAFLOW_UPDATE_HEADERS, [<<"Call-ID">>, <<"Data">>]).
-define(OPTIONAL_METAFLOW_UPDATE_HEADERS, []).
-define(METAFLOW_UPDATE_VALUES, [{<<"Event-Category">>, <<"metaflow">>}
                                 ,{<<"Event-Name">>, <<"update">>}
                                ]).
-define(METAFLOW_UPDATE_TYPES, []).

-define(METAFLOW_UPDATE_ROUTING_KEY(CallId)
        ,<<"metaflow.", (amqp_util:encode(CallId))/binary, ".update">>
       ).

%%--------------------------------------------------------------------
%% @doc Request metaflow - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req(kz_json:object() | kz_proplist()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?METAFLOW_REQ_HEADERS, ?OPTIONAL_METAFLOW_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for metaflow_req"}
    end;
req(JObj) -> req(kz_json:to_proplist(JObj)).

-spec req_v(kz_json:object() | kz_proplist()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?METAFLOW_REQ_HEADERS, ?METAFLOW_REQ_VALUES, ?METAFLOW_REQ_TYPES);
req_v(JObj) -> req_v(kz_json:to_proplist(JObj)).

-spec update(wh_json:object() | wh_proplist()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
update(Prop) when is_list(Prop) ->
    case update_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?METAFLOW_UPDATE_HEADERS, ?OPTIONAL_METAFLOW_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for metaflow_update"}
    end;
update(JObj) -> update(wh_json:to_proplist(JObj)).

-spec update_v(wh_json:object() | wh_proplist()) -> boolean().
update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?METAFLOW_UPDATE_HEADERS, ?METAFLOW_UPDATE_VALUES, ?METAFLOW_UPDATE_TYPES);
update_v(JObj) -> update_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props),
    Action = props:get_value('action', Props, <<"*">>),
    amqp_util:bind_q_to_whapps(Queue, ?METAFLOW_REQ_ROUTING_KEY(CallId, Action)),
    amqp_util:bind_q_to_whapps(Queue, ?METAFLOW_UPDATE_ROUTING_KEY(CallId)).

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props),
    Action = props:get_value('action', Props, <<"*">>),
    amqp_util:unbind_q_from_whapps(Queue, ?METAFLOW_REQ_ROUTING_KEY(CallId, Action)),
    amqp_util:unbind_q_from_whapps(Queue, ?METAFLOW_UPDATE_ROUTING_KEY(CallId)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:kapps_exchange().

-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?METAFLOW_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:kapps_publish(?METAFLOW_REQ_ROUTING_KEY(call_id(Req), action(Req)), Payload, ContentType).

-spec publish_update(api_terms()) -> 'ok'.
-spec publish_update(api_terms(), ne_binary()) -> 'ok'.
publish_update(JObj) ->
    publish_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_update(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?METAFLOW_UPDATE_VALUES, fun ?MODULE:update/1),
    amqp_util:whapps_publish(?METAFLOW_UPDATE_ROUTING_KEY(call_id(Req)), Payload, ContentType).

action([_|_]=API) ->
    props:get_value(<<"Action">>, API);
action(JObj) ->
    kz_json:get_value(<<"Action">>, JObj).

call_id([_|_]=API) ->
    props:get_value(<<"Call-ID">>, API);
call_id(JObj) ->
    kz_json:get_value(<<"Call-ID">>, JObj).
