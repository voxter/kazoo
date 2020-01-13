%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_hero_preferences).

-export([new/0]).
-export([custom_hero_groups/1, custom_hero_groups/2, set_custom_hero_groups/2]).
-export([locale/1, locale/2, set_locale/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"hero_preferences">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec custom_hero_groups(doc()) -> kz_term:api_list().
custom_hero_groups(Doc) ->
    custom_hero_groups(Doc, 'undefined').

-spec custom_hero_groups(doc(), Default) -> list() | Default.
custom_hero_groups(Doc, Default) ->
    kz_json:get_list_value([<<"custom_hero_groups">>], Doc, Default).

-spec set_custom_hero_groups(doc(), list()) -> doc().
set_custom_hero_groups(Doc, CustomHeroGroups) ->
    kz_json:set_value([<<"custom_hero_groups">>], CustomHeroGroups, Doc).

-spec locale(doc()) -> kz_term:api_binary().
locale(Doc) ->
    locale(Doc, 'undefined').

-spec locale(doc(), Default) -> binary() | Default.
locale(Doc, Default) ->
    kz_json:get_binary_value([<<"locale">>], Doc, Default).

-spec set_locale(doc(), binary()) -> doc().
set_locale(Doc, Locale) ->
    kz_json:set_value([<<"locale">>], Locale, Doc).
