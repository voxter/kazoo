-ifndef(AMIMULATOR_CALL_HRL).

-define(AMIMULATOR_CALL_HRL, 'true').

-record(call, {call_id :: api_binary() | '_'
              ,other_leg_call_id :: api_binary() | '_'
              ,channel :: api_binary() | '_'
              ,other_channel :: api_binary() | '_'
              ,account_id :: api_binary() | '_'
              ,authorizing_id :: api_binary() | '_'
              ,authorizing_type :: api_binary() | '_'
              ,custom_channel_vars :: kz_json:object() | '_'
              ,control_q :: api_binary() | '_'
              ,acdc_queue_id :: api_binary() | '_'
              ,agent_id :: api_binary | '_'
              ,conference_id :: api_binary() | '_'
              ,username :: api_binary() | '_'
              ,to = <<"nouser@norealm">> :: ne_binary() | '_'
              ,from = <<"nouser@norealm">> :: ne_binary() | '_'
              ,direction :: api_binary() | '_'
              ,answered :: api_boolean() | '_'
              ,timestamp :: api_integer() | '_'
              ,caller_id_name :: api_binary() | '_'
              ,caller_id_number :: api_binary() | '_'
              ,callee_id_name :: api_binary() | '_'
              ,callee_id_number :: api_binary() | '_'
              }).
-type amimulator_call() :: #call{}.

-endif.
