%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communication Inc
%%% @doc CouchDB and PostgreSQL response comparson
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_couch_pg_resp_compare).

-export([compare_resp/2
        ]).

%%------------------------------------------------------------------------------
%% @doc Compare Couch and PG responses and log the diff if any
%% @end
%%------------------------------------------------------------------------------
-spec compare_resp({'ok', kz_json:object()} | kz_data:data_error(), {'ok', kz_json:object()} | kz_data:data_error()) -> boolean().
compare_resp({ErrorOrOk, Resp}, {ErrorOrOk, Resp}) ->
    lager:debug("couch and pg both returned an ~p response", [ErrorOrOk]),
    log_response_same(),
    'true';
compare_resp({ErrorOrOk, CouchBody}=CouchResp, {ErrorOrOk, PGBody}=PGResp) ->
    lager:debug("couch and pg both returned an ~p response", [ErrorOrOk]),
    lager:debug("comparing response body"),
    case compare_resp_body(CouchBody, PGBody) of
        'true' ->
            log_response_same(),
            'true';
        'false' ->
            log_response_diff(CouchResp, PGResp),
            'false'
    end;
compare_resp(CouchResp, PGResp) ->
    log_response_diff(CouchResp, PGResp),
    'false'.

%%------------------------------------------------------------------------------
%% @doc Return true if the response bodys are the same, else return false
%% @end
%%------------------------------------------------------------------------------
-spec compare_resp_body(kz_json:object() | any(), kz_json:object() | any()) -> boolean().
compare_resp_body(RespBody, RespBody) ->
    'true';
compare_resp_body(CouchBody, PGBody) ->
    case {is_doc_or_docs(CouchBody), is_doc_or_docs(PGBody)} of
        {'true', 'true'} ->
            compare_docs(CouchBody, PGBody);
        _Else ->
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Return true if the arg is a doc or list of docs, else return false
%% @end
%%------------------------------------------------------------------------------
-spec is_doc_or_docs(any()) -> boolean().
is_doc_or_docs(Docs) when is_list(Docs) ->
    kz_json:are_json_objects(Docs);
is_doc_or_docs(Doc) ->
    kz_json:is_json_object(Doc).

%%------------------------------------------------------------------------------
%% @doc Compare Couch and PG Doc
%% @end
%%------------------------------------------------------------------------------
-spec compare_docs(kz_json:object() | kz_json:objects(), kz_json:object() | kz_json:objects()) -> boolean().
compare_docs([], []) ->
    lager:debug("couch and pg doc response are the same (Both returned 0 docs)"),
    'true';
compare_docs(CouchDoc, PGDoc) when not is_list(CouchDoc), not is_list(PGDoc) ->
    compare_docs_are_equal(CouchDoc, PGDoc);
compare_docs(CouchDocs, PGDoc) when is_list(CouchDocs), not is_list(PGDoc) ->
    lager:error("couch and pg response differ, Couch returned a list (length: ~p), PG returned a single doc", [length(CouchDocs)]),
    'false';
compare_docs(CouchDoc, PGDocs) when not is_list(CouchDoc), is_list(PGDocs) ->
    lager:error("couch and pg response differ, Couch returned a single doc, PG returned a list (length: ~p)", [length(PGDocs)]),
    'false';
compare_docs(CouchDocs, PGDocs) when is_list(CouchDocs), is_list(PGDocs) ->
    try lists:zip(CouchDocs, PGDocs) of
        DocsZiped ->
            lager:debug("couch and pg response contain the same number of docs"),
            lists:foldl(fun({CouchDoc, PGDoc}, 'false') -> compare_docs(CouchDoc, PGDoc), 'false';
                           ({CouchDoc, PGDoc}, _) -> compare_docs(CouchDoc, PGDoc) end
                       ,'true'
                       ,DocsZiped)
    catch
        error:_Error ->
            lager:error("couch and pg response contain diferent number of docs (Couch ~p, PG ~p)", [length(CouchDocs), length(PGDocs)]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Compare 2 kz_json objects
%% @end
%%------------------------------------------------------------------------------
-spec compare_docs_are_equal(kz_json:object(), kz_json:object()) -> boolean().
compare_docs_are_equal(CouchDoc, PGDoc) ->
    lager:debug("comparing couch and pg doc....."),
    CouchDocClean = clean_doc(CouchDoc),
    PGDocClean = clean_doc(PGDoc),
    case kz_json:are_equal(CouchDocClean, PGDocClean) of
        'true' ->
            lager:debug("docs are the same"),
            'true';
        'false' ->
            lager:error("docs are NOT the same"),
            log_doc_diff(CouchDoc, PGDoc),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Remove all rev's from doc
%% @end
%%------------------------------------------------------------------------------
-spec clean_doc(kz_json:object()) -> kz_json:object().
clean_doc(Doc) ->
    DocFiltered = kz_json:filter(fun filter_doc/1, Doc),
    kz_json:delete_key([<<"doc">>, <<"_rev">>], DocFiltered).

-spec filter_doc({kz_type:ne_binary(),kz_type:ne_binary()}) -> boolean().
filter_doc({<<"rev">>, _}) -> 'false';
filter_doc({<<"_rev">>, _}) -> 'false';
filter_doc(_) -> 'true'.

-spec log_response_same() -> 'ok'.
log_response_same() ->
    lager:debug("couch and pg doc response are the same").

-spec log_response_diff(any(), any()) -> 'ok'.
log_response_diff(CouchResp, PGResp) ->
    lager:error("couch and pg doc response are NOT the same"),
    lager:error("couch resp: ~p", [CouchResp]),
    lager:error("pg resp: ~p", [PGResp]).

%%------------------------------------------------------------------------------
%% @doc Log any diferences in 2 docs to the lager
%% @end
%%------------------------------------------------------------------------------
-spec log_doc_diff(kz_json:object(), kz_json:object()) -> 'ok'.
log_doc_diff(CouchDoc, PGDoc) ->
    CouchDocClean = clean_doc(CouchDoc),
    PGDocClean = clean_doc(PGDoc),
    CouchDocDiff = kz_json:diff(CouchDocClean, PGDocClean),
    PGDocDiff = kz_json:diff(PGDocClean, CouchDocClean),
    lager:error("doc diff: in couch and not in pg: ~p", [CouchDocDiff]),
    lager:error("doc diff: in pg and not in couch: ~p", [PGDocDiff]).
