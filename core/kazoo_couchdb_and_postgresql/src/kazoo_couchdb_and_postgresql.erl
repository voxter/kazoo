%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc CouchDB and PostgreSQL kazoo storage driver
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kazoo_couchdb_and_postgresql).
-behaviour(kz_data).

%% Driver callbacks
-export([new_connection/1
        ,format_error/1
        ]).

%% Connections callbacks
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

%%%% View-related
-export([design_info/3
        ,all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,all_docs/3
        ]).


%% Connections operations
-spec new_connection(map()) -> kz_data:connection() | {'error', term()}.
new_connection(#{'settings' := #{'couchdb' := CouchDBMap
                                ,'postgresql' := PostgreSQLMap}
                }=_SettingsMap) ->
    lager:debug("couchdb and postgresql new connection called"),
    lager:debug("attempting new couchdb and postgresql connection with settings ~p", [_SettingsMap]),
    case {kazoo_couch:new_connection(CouchDBMap), kazoo_postgresql:new_connection(PostgreSQLMap)} of
        {{'ok', CouchConn}, {'ok', PGConn}} ->
            lager:debug("couch and postgresql connected successfully, Couch Conn: ~p, PostgreSQL Conn: ~p", [CouchConn, PGConn]),
            {'ok', #{'couchdb' => CouchConn, 'postgresql' => PGConn}};
        {{'error', CouchError} = Error, {'error', PGError}} ->
            lager:error("couch and postgresql failed to connect, CouchError: ~p, PostgreSQL Error: ~p", [CouchError, PGError]),
            Error;
        {{'error', CouchError} = Error, _} ->
            %% TODO Taredown PG connection
            lager:error("couch failed to connect, Couch Error: ~p", [CouchError]),
            Error;
        {_, {'error', PGError} = Error} ->
            %% TODO Taredown Couch connection
            lager:error("postgresql failed to connect, PostgreSQL Error: ~p", [PGError]),
            Error
    end.


-spec format_error(any()) -> any().
format_error(Error) ->
    Error.

-spec server_url(kz_data:connection()) -> kz_term:ne_binary().
server_url(Connections) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, []),
    CouchResp.

-spec get_db(kz_data:connection(), kz_term:ne_binary()) -> any().
get_db(Connections, DBName) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName]),
    CouchResp.

-spec db_url(kz_data:connection(), kz_term:ne_binary()) -> kz_term:ne_binary().
db_url(Connections, DBName) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName]),
    CouchResp.

-spec server_info(kz_data:connection()) -> any().
server_info(Connections) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [], 'false'),
    CouchResp.

%% DB operations
-spec db_create(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) -> boolean().
db_create(Connections, DBName, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Options]),
    PGResp.

-spec db_delete(kz_data:connection(), kz_term:ne_binary()) -> boolean().
db_delete(Connections, DBName) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName]),
    PGResp.

-spec db_view_cleanup(kz_data:connection(), kz_term:ne_binary()) -> boolean().
db_view_cleanup(Connections, DBName) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName]),
    PGResp.

-spec db_info(kz_data:connection()) -> {'ok', kz_json:object()} | kz_data:data_error().
db_info(Connections) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, []),
    PGResp.

-spec db_info(kz_data:connection(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | kz_data:data_error().
db_info(Connections, DBName) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName]),
    PGResp.

-spec db_exists(kz_data:connection(), kz_term:ne_binary()) -> boolean().
db_exists(Connections, DBName) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName]),
    PGResp.

-spec db_archive(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | kz_data:data_error().
db_archive(Connections, DBName, Filename) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Filename]),
    PGResp.

-spec db_list(kz_data:connection(), kz_data:options()) -> {'ok', kz_term:ne_binaries()} | kz_data:data_error().
db_list(Connections, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [Options]),
    PGResp.


%% Document operations
-spec open_doc(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
                      {'ok', kz_json:object()} |
                      kz_data:data_error().
open_doc(Connections, DBName, DocId, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DocId, Options]),
    PGResp.

-spec lookup_doc_rev(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binary()} |
                            kz_data:data_error().
lookup_doc_rev(Connections, DBName, DocId) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DocId]),
    PGResp.

-spec save_doc(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
                      {'ok', kz_json:object()} |
                      kz_data:data_error().
save_doc(Connections, DBName, Doc, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Doc, Options]),
    PGResp.

-spec save_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) ->
                       {'ok', kz_json:objects()} |
                       kz_data:data_error().
save_docs(Connections, DBName, Docs, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Docs, Options]),
    PGResp.


-spec del_doc(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
                     {'ok', kz_json:object()} |
                     kz_data:data_error().
del_doc(Connections, DBName, Doc, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Doc, Options]),
    PGResp.

-spec del_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) ->
                      {'ok', kz_json:objects()} |
                      kz_data:data_error().
del_docs(Connections, DBName, Docs, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Docs, Options]),
    PGResp.

-spec ensure_saved(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
                          {'ok', kz_json:object()} |
                          kz_data:data_error().
ensure_saved(Connections, DBName, Doc, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Doc, Options]),
    PGResp.


%% Attachment-related
-spec fetch_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
fetch_attachment(Connections, DBName, DocId, AName) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DocId, AName]),
    CouchResp.

-spec stream_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) ->
                               {'ok', kz_data:document()} |
                               {'error', any()}.
stream_attachment(Connections, DBName, DocId, AName, Caller) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DocId, AName, Caller]),
    CouchResp.

-spec put_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
put_attachment(Connections, DBName, DocId, AName, Contents, Options) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DocId, AName, Contents, Options]),
    CouchResp.

-spec delete_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
delete_attachment(Connections, DBName, DocId, AName, Options) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DocId, AName, Options]),
    CouchResp.

-spec attachment_url(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
attachment_url(Connections, DBName, DocId, AName, Options) ->
    {CouchResp, _PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DocId, AName, Options]),
    CouchResp.

%% View-related
-spec design_info(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | kz_data:data_error().
design_info(Connections, DBName, Design) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Design]),
    PGResp.

-spec all_design_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) ->
                             {'ok', kz_json:objects()} |
                             kz_data:data_error().
all_design_docs(Connections, DBName, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Options]),
    PGResp.

-spec get_results(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
                         {'ok', kz_json:objects() | kz_json:path()} |
                         kz_data:data_error().
get_results(Connections, DBName, DesignDoc, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DesignDoc, Options]),
    PGResp.

-spec get_results_count(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
                               {'ok', integer()} |
                               kz_data:data_error().
get_results_count(Connections, DBName, DesignDoc, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, DesignDoc, Options]),
    PGResp.

-spec all_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) ->
                      {'ok', kz_json:objects()} |
                      kz_data:data_error().
all_docs(Connections, DBName, Options) ->
    {_CouchResp, PGResp} = call_couch_and_pg_funs(Connections, ?FUNCTION_NAME, [DBName, Options]),
    PGResp.


%% Private functions
-spec call_couch_and_pg_funs(kz_data:server(), atom(), list()) ->
                                    {{'ok', kz_json:object()} | kz_data:data_error()
                                    ,{'ok', kz_json:object()} | kz_data:data_error()}.
call_couch_and_pg_funs(Conn, Fun, Args) ->
    call_couch_and_pg_funs(Conn, Fun, Args, 'true').

-spec call_couch_and_pg_funs(kz_data:server(), atom(), list(), boolean()) ->
                                    {{'ok', kz_json:object()} | kz_data:data_error()
                                    ,{'ok', kz_json:object()} | kz_data:data_error()}.
call_couch_and_pg_funs(Conn, Fun, Args, 'true') ->
    Arity = length(Args),
    lager:debug("~p/~p called", [Fun, Arity]),
    {CouchResp, PGResp, CouchTime, PGTime} = do_call_couch_and_pg_funs(Conn, Fun, Args),
    ResponseMatch = kz_couch_pg_resp_compare:compare_resp(CouchResp, PGResp),
    lager:debug("Function execute times, Fun: ~p/~p, Couch: ~p, PG: ~p (microseconds), Response match: ~p", [Fun, Arity, CouchTime, PGTime, ResponseMatch]),
    {CouchResp, PGResp};
call_couch_and_pg_funs(Conn, Fun, Args, 'false') ->
    {CouchResp, PGResp, _CouchTime, _PGTime} = do_call_couch_and_pg_funs(Conn, Fun, Args),
    {CouchResp, PGResp}.

-spec do_call_couch_and_pg_funs(kz_data:server(), atom(), list()) ->
                                       {{'ok', kz_json:object()} | kz_data:data_error()
                                       ,{'ok', kz_json:object()} | kz_data:data_error()
                                       ,pos_integer()
                                       ,pos_integer()}.
do_call_couch_and_pg_funs(#{'couchdb' := CouchConn, 'postgresql' := PGConn}, Fun, Args) ->
    {CouchTime, CouchResp} = timer:tc(kazoo_couch, Fun, [CouchConn | Args]),
    {PGTime, PGResp} = timer:tc(kazoo_postgresql, Fun, [PGConn | Args]),
    {CouchResp, PGResp, CouchTime, PGTime}.
