-ifndef(AMIMULATOR_CALL_HRL).

-define(AMIMULATOR_CALL_HRL, 'true').

-record(call, {call_id :: kz_term:api_binary() | '_'
              ,other_leg_call_id :: kz_term:api_binary() | '_'
              ,channel :: kz_term:api_binary() | '_'
              ,other_channel :: kz_term:api_binary() | '_'
              ,account_id :: kz_term:api_binary() | '_'
              ,authorizing_id :: kz_term:api_binary() | '_'
              ,authorizing_type :: kz_term:api_binary() | '_'
              ,custom_channel_vars :: kz_json:object() | '_'
              ,control_q :: kz_term:api_binary() | '_'
              ,acdc_queue_id :: kz_term:api_binary() | '_'
              ,agent_id :: kz_term:api_binary() | '_'
              ,conference_id :: kz_term:api_binary() | '_'
              ,username :: kz_term:api_binary() | '_'
              ,to = <<"nouser@norealm">> :: kz_term:ne_binary() | '_'
              ,from = <<"nouser@norealm">> :: kz_term:ne_binary() | '_'
              ,direction :: kz_term:api_binary() | '_'
              ,answered :: kz_term:api_boolean() | '_'
              ,timestamp :: kz_term:api_integer() | '_'
              ,caller_id_name :: kz_term:api_binary() | '_'
              ,caller_id_number :: kz_term:api_binary() | '_'
              ,callee_id_name :: kz_term:api_binary() | '_'
              ,callee_id_number :: kz_term:api_binary() | '_'
              }).
-type amimulator_call() :: #call{}.

-endif.
