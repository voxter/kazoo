%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc PostgreSQL kazoo storage driver
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kazoo_postgresql).
-behaviour(kz_data).

%% Driver callbacks
-export([new_connection/1
        ,format_error/1
        ]).

%% _ConnPool callbacks
-export([server_url/1
        ,get_db/2
        ,db_url/2
        ,server_info/1
        ]).

%% DB operations
-export([db_create/3
        ,db_delete/2
        ,db_view_cleanup/2
        ,db_info/1, db_info/2
        ,db_exists/2
        ,db_archive/3
        ,db_list/2
        ]).

%% Document operations
-export([open_doc/4
        ,lookup_doc_rev/3
        ,save_doc/4
        ,save_docs/4
        ,del_doc/4
        ,del_docs/4
        ,ensure_saved/4
        ]).

%% Attachment-related
-export([fetch_attachment/4
        ,stream_attachment/5
        ,put_attachment/6
        ,delete_attachment/5
        ,attachment_url/5
        ]).

%% View-related
-export([design_info/3
        ,all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,all_docs/3
        ]).

-include("kz_postgresql.hrl").

%% _ConnPool operations
-spec new_connection(map()) -> {'ok', kz_postgresql:connection_pool()} | kz_data:data_error().
new_connection(ConnPoolSettingsMap) ->
    kz_postgresql_connection:new_connection(ConnPoolSettingsMap).

-spec format_error(epgsql:error_reply()) -> kz_data:data_errors().
format_error(Error) ->
    kz_postgresql_response:format_error(Error).

%% ConnPool operations
-spec server_url(kz_postgresql:connection_pool()) -> kz_term:ne_binary().
server_url(_ConnPool) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec get_db(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()) -> any().
get_db(_ConnPool, _KazooDBName) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    'undefined'.

-spec db_url(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()) -> kz_term:ne_binary().
db_url(_ConnPool, _KazooDBName) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    'undefined'.

-spec server_info(kz_postgresql:connection_pool()) -> {'ok', kz_postgresql:connection_pool()} | kz_data:data_error().
server_info(ConnPool) ->
    kz_postgresql_util:server_info(ConnPool).


%% DB operations
-spec db_create(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:options()) -> boolean().
db_create(_ConnPool, _KazooDBName, _Options) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    false.

-spec db_delete(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()) -> boolean().
db_delete(ConnPool, KazooDBName) ->
    kz_postgresql_db:db_delete(ConnPool, KazooDBName).

-spec db_view_cleanup(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()) -> boolean().
db_view_cleanup(_ConnPool, _KazooDBName) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    false.

-spec db_info(kz_postgresql:connection_pool()) -> {'ok', kz_json:object()} | kz_data:data_error().
db_info(_ConnPool) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec db_info(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()) -> {'ok', kz_json:object()} | kz_data:data_error().
db_info(_ConnPool, _KazooDBName) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.


-spec db_exists(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name()) -> boolean().
db_exists(ConnPool, KazooDBName) ->
    kz_postgresql_db:db_exists(ConnPool, KazooDBName).

-spec db_archive(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary()) -> 'ok' | kz_data:data_error().
db_archive(_ConnPool, _KazooDBName, _Filename) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec db_list(kz_postgresql:connection_pool(), kz_data:options()) -> {'ok', kz_term:ne_binaries()} | kz_data:data_error().
db_list(ConnPool, Options) ->
    kz_postgresql_db:db_list(ConnPool, Options).


%% Document operations
-spec open_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_data:options()) ->
                      {'ok', kz_json:object()} | kz_data:data_error().
open_doc(ConnPool, KazooDBName, DocId, Options) ->
    kz_postgresql_doc:open_doc(ConnPool, KazooDBName, DocId, Options).

-spec lookup_doc_rev(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binary()} | kz_data:data_error().
lookup_doc_rev(ConnPool, KazooDBName, DocId) ->
    kz_postgresql_doc:lookup_doc_rev(ConnPool, KazooDBName, DocId).

-spec save_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document(), kz_data:options()) ->
                      {'ok', kz_json:object()} | kz_data:data_error().
save_doc(ConnPool, KazooDBName, Doc, Options) ->
    kz_postgresql_doc:save_doc(ConnPool, KazooDBName, Doc, Options).

-spec save_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:documents(), kz_data:options()) ->
                       {'ok', kz_json:objects()} | kz_data:data_error().
save_docs(ConnPool, KazooDBName, Docs, Options) ->
    kz_postgresql_doc:save_docs(ConnPool, KazooDBName, Docs, Options).


-spec del_doc(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document(), kz_data:options()) ->
                     {'ok', kz_json:object()} | kz_data:data_error().
del_doc(ConnPool, KazooDBName, Doc, Options) ->
    kz_postgresql_doc:del_doc(ConnPool, KazooDBName, Doc, Options).

-spec del_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:documents(), kz_data:options()) ->
                      {'ok', kz_json:objects()} | kz_data:data_error().
del_docs(ConnPool, KazooDBName, Docs, Options) ->
    kz_postgresql_doc:del_docs(ConnPool, KazooDBName, Docs, Options).

-spec ensure_saved(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:document(), kz_data:options()) ->
                          {'ok', kz_json:object()} | kz_data:data_error().
ensure_saved(ConnPool, KazooDBName, Doc, Options) ->
    kz_postgresql_doc:save_doc(ConnPool, KazooDBName, Doc, Options).


%% Attachment-related
-spec fetch_attachment(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
fetch_attachment(_ConnPool, _KazooDBName, _DocId, _AName) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec stream_attachment(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) ->
                               {'ok', kz_data:document()} | {'error', any()}.
stream_attachment(_ConnPool, _KazooDBName, _DocId, _AName, _Caller) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec put_attachment(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
                    ,kz_data:options()) ->
                            any().
put_attachment(_ConnPool, _KazooDBName, _DocId, _AName, _Contents, _Options) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec delete_attachment(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
delete_attachment(_ConnPool, _KazooDBName, _DocId, _AName, _Options) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec attachment_url(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
attachment_url(_ConnPool, _KazooDBName, _DocId, _AName, _Options) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

%% View-related
-spec design_info(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary()) ->
                         {'ok', kz_json:object()} | kz_data:data_error().
design_info(_ConnPool, _KazooDBName, _Design) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec all_design_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:options()) ->
                             {'ok', kz_json:objects()} | kz_data:data_error().
all_design_docs(_ConnPool, ?NE_BINARY = _KazooDBName, _Options) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec get_results(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_data:options()) ->
                         {'ok', kz_json:objects() | kz_json:path()} | kz_data:data_error().
get_results(ConnPool, KazooDBName, DesignDoc, Options) ->
    kz_postgresql_view:get_results(ConnPool, KazooDBName, DesignDoc, Options).

-spec get_results_count(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_term:ne_binary(), kz_data:options()) ->
                               {'ok', integer()} | kz_data:data_error().
get_results_count(_ConnPool, _KazooDBName, _DesignDoc, _ViewOptions) ->
    lager:error("~p/~p, function not implemented", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    {'error', 'function_not_implemented'}.

-spec all_docs(kz_postgresql:connection_pool(), kz_postgresql:kazoo_db_name(), kz_data:options()) ->
                      {'ok', kz_json:objects()} | kz_data:data_error().
all_docs(ConnPool, KazooDBName, Options) ->
    kz_postgresql_view:all_docs(ConnPool, KazooDBName, Options).
