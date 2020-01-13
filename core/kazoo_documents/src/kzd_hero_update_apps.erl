%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Accessors for `hero_update_apps' document.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_hero_update_apps).

-export([new/0]).
-export([apps/1, apps/2, set_apps/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"hero_update_apps">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec apps(doc()) -> kz_term:api_list().
apps(Doc) ->
    apps(Doc, 'undefined').

-spec apps(doc(), Default) -> list() | Default.
apps(Doc, Default) ->
    kz_json:get_list_value([<<"apps">>], Doc, Default).

-spec set_apps(doc(), list()) -> doc().
set_apps(Doc, Apps) ->
    kz_json:set_value([<<"apps">>], Apps, Doc).
