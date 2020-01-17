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
-spec compare_resp({'ok', kz_json:object()} | boolean() | kz_data:data_error(), {'ok', kz_json:object()} | boolean() | kz_data:data_error()) ->
                          boolean().
compare_resp({ErrorOrOk, Resp}, {ErrorOrOk, Resp}) ->
    lager:debug("couch and pg returned the same response, Response result: ~p", [ErrorOrOk]),
    'true';
compare_resp({ErrorOrOk, CouchBody}=CouchResp, {ErrorOrOk, PGBody}=PGResp) ->
    lager:debug("couch and pg returned a response result: ~p, comparing response doc(s)", [ErrorOrOk]),
    case compare_resp_body(CouchBody, PGBody) of
        'true' ->
            lager:debug("couch and pg returned the same response, Response result: ~p", [ErrorOrOk]),
            'true';
        'false' ->
            lager:error("couch and pg returned different responses, Response results are the same (~p) but doc contents differ", [ErrorOrOk]),
            log_responses(CouchResp, PGResp),
            'false'
    end;
compare_resp({CouchErrorOrOk, _}=CouchResp, {PGErrorOrOk, _}=PGResp) ->
    lager:error("couch and pg returned different responses, Different response types (Couch: ~p, PG: ~p)", [CouchErrorOrOk, PGErrorOrOk]),
    log_responses(CouchResp, PGResp),
    'false';
compare_resp(Resp, Resp) ->
    lager:debug("couch and pg returned the same response, No response result returned"),
    'true';
compare_resp(CouchResp, PGResp) ->
    lager:error("couch and pg returned different responses, Unknown difference"),
    log_responses(CouchResp, PGResp),
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
    'true';
compare_docs(CouchDoc, PGDoc) when not is_list(CouchDoc), not is_list(PGDoc) ->
    compare_doc(CouchDoc, PGDoc);
compare_docs(CouchDocs, PGDoc) when is_list(CouchDocs), not is_list(PGDoc) ->
    lager:error("couch and pg docs differ, Couch returned a list of size ~p, PG returned single doc", [length(CouchDocs)]),
    'false';
compare_docs(CouchDoc, PGDocs) when not is_list(CouchDoc), is_list(PGDocs) ->
    lager:error("couch and pg docs differ, Couch returned a single doc, PG returned a list of size ~p", [length(PGDocs)]),
    'false';
compare_docs(CouchDocs, PGDocs) when is_list(CouchDocs), is_list(PGDocs) ->
    try lists:zip(CouchDocs, PGDocs) of
        DocsZiped ->
            lager:debug("couch and pg response contain the same number of docs: ~p", [length(CouchDocs)]),
            lists:foldl(fun({CouchDoc, PGDoc}, 'false') -> compare_docs(CouchDoc, PGDoc), 'false';
                           ({CouchDoc, PGDoc}, _) -> compare_docs(CouchDoc, PGDoc) end
                       ,'true'
                       ,DocsZiped)
    catch
        error:_Error ->
            lager:error("couch and pg docs differ, Couch returned a list of size ~p, PG returned a list of size ~p", [length(CouchDocs), length(PGDocs)]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Compare 2 kz_json objects
%% First verify the doc ids are equal, then check the rest of the attributes
%% @end
%%------------------------------------------------------------------------------
-spec compare_doc(kz_json:object(), kz_json:object()) -> boolean().
compare_doc(CouchDoc, PGDoc) ->
    case kz_doc:id(CouchDoc) == kz_doc:id(PGDoc) of
        'true' -> compare_docs_are_equal(CouchDoc, PGDoc);
        'false' ->
            lager:error("found issue when comparing couch and PG doc, doc ids are different (Couch doc id: ~p, PG doc id: ~p)", [kz_doc:id(CouchDoc), kz_doc:id(PGDoc)]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Compare 2 kz_json objects
%% @end
%%------------------------------------------------------------------------------
-spec compare_docs_are_equal(kz_json:object(), kz_json:object()) -> boolean().
compare_docs_are_equal(CouchDoc, PGDoc) ->
    CouchDocClean = clean_doc(CouchDoc),
    PGDocClean = clean_doc(PGDoc),
    case kz_json:are_equal(CouchDocClean, PGDocClean) of
        'true' ->
            'true';
        'false' ->
            lager:error("found issue when comparing couch and PG doc, docs are NOT the same"),
            log_doc_diff(CouchDoc, PGDoc),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Remove all rev's from doc
%% Remove rev at root level and inside doc or value if present
%% @end
%%------------------------------------------------------------------------------
-spec clean_doc(kz_json:object()) -> kz_json:object().
clean_doc(Doc) ->
    DocFiltered = kz_json:filter(fun filter_doc/1, Doc),
    DocFiltered1 = clean_doc(DocFiltered, <<"doc">>),
    clean_doc(DocFiltered1, <<"value">>).

-spec clean_doc(kz_json:object(), kz_json:path()) -> kz_json:object().
clean_doc(Doc, Path) ->
    case kz_json:get_value(Path, Doc) of
        'undefined' -> Doc;
        'null' -> Doc;
        _Exists -> kz_json:filter(fun filter_doc/1, Doc, Path)
    end.

-spec filter_doc({kz_type:ne_binary(),kz_type:ne_binary()}) -> boolean().
filter_doc({<<"pvt_type">>, _}) -> 'false';
filter_doc({<<"rev">>, _}) -> 'false';
filter_doc({<<"_rev">>, _}) -> 'false';
filter_doc(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Log both responses
%% @end
%%------------------------------------------------------------------------------
-spec log_responses(any(), any()) -> 'ok'.
log_responses(CouchResp, PGResp) ->
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
    lager:error("doc diff: Data in couch and not in pg: ~p", [CouchDocDiff]),
    lager:error("doc diff: Data in pg and not in couch: ~p", [PGDocDiff]).
