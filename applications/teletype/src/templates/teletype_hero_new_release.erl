%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Ooma Inc
%%% @doc
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_hero_new_release).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"hero_new_release">>).

-define(TEMPLATE_SUBJECT, <<"New Release of {{hero.app_name}} is Available">>).
-define(TEMPLATE_CATEGORY, <<"hero">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

-define(RELEASE_NOTIFICATION_EMAIL_KEY, <<"release_notification_email">>).

%%------------------------------------------------------------------------------
%% @doc Define the teletype template and bind to requests for sending emails
%% built from the template.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'subject', ?TEMPLATE_SUBJECT}
                                          ,{'category', ?TEMPLATE_CATEGORY}
                                          ,{'to', ?TEMPLATE_TO}
                                          ,{'from', ?TEMPLATE_FROM}
                                          ,{'cc', ?TEMPLATE_CC}
                                          ,{'bcc', ?TEMPLATE_BCC}
                                          ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]),
    teletype_bindings:bind(<<"hero_new_release">>, ?MODULE, 'handle_req').

%%------------------------------------------------------------------------------
%% @doc Handle requests for sending emails built from the template, provided the
%% template is not disabled.
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:hero_new_release_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:notification_failed(?TEMPLATE_ID, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kapi_notifications:account_id(DataJObj),
    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' ->
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            WhitelabelResult = kz_datamgr:open_cache_doc(AccountDb, <<"whitelabel">>),
            process_req(DataJObj, WhitelabelResult)
    end.

-spec process_req(kz_json:object(), {'ok', kz_json:object()} | kz_datamgr:data_error()) -> template_response().
process_req(DataJObj, {'ok', WhitelabelJObj}) ->
    HeroWhitelabelJObj = kz_json:get_json_value(<<"hero">>, WhitelabelJObj),
    process_req2(DataJObj, HeroWhitelabelJObj);
process_req(_, {'error', E}) ->
    teletype_util:notification_failed(?TEMPLATE_ID, E).

%%------------------------------------------------------------------------------
%% @doc Process the template request provided there is the required Hero
%% whitelabel configuration for the recipient.
%% @end
%%------------------------------------------------------------------------------
-spec process_req2(kz_json:object(), kz_term:api_object()) -> template_response().
process_req2(_, 'undefined') ->
    teletype_util:notification_failed(?TEMPLATE_ID, <<"missing whitelabel app configuration">>);
process_req2(DataJObj, HeroWhitelabelJObj) ->
    Emails = case kz_json:get_ne_binary_value(?RELEASE_NOTIFICATION_EMAIL_KEY, HeroWhitelabelJObj) of
                 'undefined' -> [];
                 Email -> [Email]
             end,
    maybe_send_new_release_email(DataJObj, HeroWhitelabelJObj, Emails).

%%------------------------------------------------------------------------------
%% @doc Build macros, template metadata, subject, and supplementary recipient
%% emails, then send a custom email to each primary recipient.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_send_new_release_email(kz_json:object(), kz_json:object(), kz_term:api_ne_binaries()) -> template_response().
maybe_send_new_release_email(_, _, []) ->
    teletype_util:notification_ignored(?TEMPLATE_ID);
maybe_send_new_release_email(DataJObj, HeroWhitelabelJObj, ToEmails) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ]
        ++ build_macro_data(DataJObj, HeroWhitelabelJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID, kapi_notifications:account_id(DataJObj)),

    Subject = teletype_util:render_subject(kz_json:get_ne_binary_value(<<"subject">>, TemplateMetaJObj), Macros),
    Emails = teletype_util:find_addresses(
               kz_json:new()
              ,TemplateMetaJObj
              ,?TEMPLATE_ID
              ),

    Results = lists:map(fun(ToEmail) ->
                                send_new_release_email(Macros, DataJObj, ToEmail, Emails, Subject)
                        end, ToEmails),
    find_at_least_one_successful(Results).

%%------------------------------------------------------------------------------
%% @doc Generate and send template email to a recipient plus CC/BCCs.
%% @end
%%------------------------------------------------------------------------------
-spec send_new_release_email(macros(), kz_json:object(), kz_term:ne_binary(), email_map(), binary()) -> template_response().
send_new_release_email(Macros, DataJObj, ToEmail, BaseEmails, Subject) ->
    HeroMacros = props:get_value(<<"hero">>, Macros),
    Macros1 = props:set_value(<<"hero">>, props:set_value(?RELEASE_NOTIFICATION_EMAIL_KEY, ToEmail, HeroMacros), Macros),
    Emails = props:set_value(<<"to">>, [ToEmail], BaseEmails),
    RenderedTemplate = teletype_templates:render(?TEMPLATE_ID, Macros1, DataJObj),

    case teletype_util:send_email(Emails, Subject, RenderedTemplate) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} -> teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.

%%------------------------------------------------------------------------------
%% @doc If there was at least one successful email sent, consider the
%% notification as a success as teletype_bindings is expecting one result.
%% @end
%%------------------------------------------------------------------------------
-spec find_at_least_one_successful(template_responses()) -> template_response().
find_at_least_one_successful(Results) ->
    case lists:keyfind('completed', 1, Results) of
        'false' -> lists:keyfind('failed', 1, Results);
        Completed -> Completed
    end.

%%------------------------------------------------------------------------------
%% @doc Build Hero new release notification-specific macros from Hero whitelabel
%% data.
%% @end
%%------------------------------------------------------------------------------
-spec build_macro_data(kz_json:object(), kz_json:object()) -> kz_term:proplist().
build_macro_data(DataJObj, HeroWhitelabelJObj) ->
    [{<<"hero">>
     ,[{<<"app_name">>, kz_json:get_ne_binary_value(<<"name">>, HeroWhitelabelJObj)}
      ,{<<"asset_url">>, kz_json:get_ne_binary_value(<<"asset_url">>, DataJObj)}
      ,{<<"version">>, kz_json:get_ne_binary_value(<<"hero_version">>, DataJObj)}
      ]
     }].
