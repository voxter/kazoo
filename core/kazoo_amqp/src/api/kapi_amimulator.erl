%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(kapi_amimulator).

-export([control_queue_req/1, control_queue_req_v/1
         ,control_queue_resp/1, control_queue_resp_v/1
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_control_queue_req/1, publish_control_queue_req/2
         ,publish_control_queue_resp/2, publish_control_queue_resp/3
        ]).

-include_lib("amqp_util.hrl").
-include_lib("kazoo_amqp/include/kz_api.hrl").

-define(CONTROL_QUEUE_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CONTROL_QUEUE_REQ_HEADERS, []).
-define(CONTROL_QUEUE_REQ_VALUES, [{<<"Event-Category">>, <<"amimulator">>}
                              	   ,{<<"Event-Name">>, <<"control_queue_req">>}
                                  ]).
-define(CONTROL_QUEUE_REQ_TYPES, []).

-define(CONTROL_QUEUE_RESP_HEADERS, [<<"Call-ID">>, <<"Control-Queue">>]).
-define(OPTIONAL_CONTROL_QUEUE_RESP_HEADERS, []).
-define(CONTROL_QUEUE_RESP_VALUES, [{<<"Event-Category">>, <<"amimulator">>}
									,{<<"Event-Name">>, <<"control_queue_resp">>}
								   ]).
-define(CONTROL_QUEUE_RESP_TYPES, []).

-define(CONTROL_QUEUE_ROUTING_KEY(CallId)
        ,<<"amimulator.control_queue.", (amqp_util:encode(CallId))/binary>>
       ).

%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec control_queue_req(kz_json:object() | kz_proplist()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
control_queue_req(Prop) when is_list(Prop) ->
    case control_queue_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CONTROL_QUEUE_REQ_HEADERS, ?OPTIONAL_CONTROL_QUEUE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for control_queue_req"}
    end;
control_queue_req(JObj) -> control_queue_req(kz_json:to_proplist(JObj)).

-spec control_queue_req_v(kz_json:object() | kz_proplist()) -> boolean().
control_queue_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CONTROL_QUEUE_REQ_HEADERS, ?CONTROL_QUEUE_REQ_VALUES, ?CONTROL_QUEUE_REQ_TYPES);
control_queue_req_v(JObj) -> control_queue_req_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec control_queue_resp(kz_json:object() | kz_proplist()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
control_queue_resp(Prop) when is_list(Prop) ->
    case control_queue_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CONTROL_QUEUE_RESP_HEADERS, ?OPTIONAL_CONTROL_QUEUE_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for control_queue_resp"}
    end;
control_queue_resp(JObj) -> control_queue_resp(kz_json:to_proplist(JObj)).

-spec control_queue_resp_v(kz_json:object() | kz_proplist()) -> boolean().
control_queue_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CONTROL_QUEUE_RESP_HEADERS, ?CONTROL_QUEUE_RESP_VALUES, ?CONTROL_QUEUE_RESP_TYPES);
control_queue_resp_v(JObj) -> control_queue_resp_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props),
    amqp_util:bind_q_to_kapps(Queue, ?CONTROL_QUEUE_ROUTING_KEY(CallId)).

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props),
    amqp_util:unbind_q_from_kapps(Queue, ?CONTROL_QUEUE_ROUTING_KEY(CallId)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:kapps_exchange().

%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec publish_control_queue_req(api_terms()) -> 'ok'.
-spec publish_control_queue_req(api_terms(), ne_binary()) -> 'ok'.
publish_control_queue_req(JObj) ->
    publish_control_queue_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_control_queue_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?CONTROL_QUEUE_REQ_VALUES, fun ?MODULE:control_queue_req/1),
    amqp_util:kapps_publish(?CONTROL_QUEUE_ROUTING_KEY(call_id(Req)), Payload, ContentType).

call_id([_|_]=API) ->
    props:get_value(<<"Call-ID">>, API);
call_id(JObj) ->
    kz_json:get_value(<<"Call-ID">>, JObj).

%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec publish_control_queue_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_control_queue_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_control_queue_resp(Q, JObj) ->
    publish_control_queue_resp(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_control_queue_resp(Q, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?CONTROL_QUEUE_RESP_VALUES, fun ?MODULE:control_queue_resp/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).
