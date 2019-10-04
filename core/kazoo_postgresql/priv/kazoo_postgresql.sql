--
-- Kazoo PostgreSQL Database Schema
-- Copyright (C) 2019-, Voxter Communication Inc
-- Author Ben Bradford bsc.benbradford@gmail.com
--

----------------------------------------------------------------------------------
-- PG tables prerequisites
----------------------------------------------------------------------------------
-- Table Names:
--   All PG data table names and JSON doc type must be mapped in kz_postgresql_schema:get_pgsql_table_name/2
--   If there is no table name mapping, the PG table 'other' will be used
--   See table schema section for archive table naming convention
--
-- Column Names:
--   All column names matching a JSON doc key will be populated with the corresponding value
--   Custom column names can be defined and must be mapped in kz_postgresql_schema:get_key_path/2 to a JSON key
--
-- Table Schema:
--   For ever data table there must contain an corresponding archive table
--   Archive table name must be defined to the corresponding data table name with trailing '_archive'
--   EG. cdr data table must have an archive table named cdr_archive

--   All data tables must contain a minimum defined set of columns and a PKEY as defined below:
--    _id character varying(255) NOT NULL,
--    _rev integer NOT NULL,
--    pvt_account_db character varying(255) NOT NULL,
--    pvt_account_db character varying(255),
--    pvt_created bigint,
--    pvt_modified bigint,
--    pvt_node character varying(255),
--    pvt_type character varying(255),
--    other_json jsonb,
--    PRIMARY KEY(_id, _rev)

--   All archive tables must contain the same columns as the corresponding data table with the following leading columns in the order:
--    _deleted boolean NOT NULL
--    _changed_on TIMESTAMP(6) NOT NULL,
--
--   All data tables must have an associated table trigger as defined below:
--    CREATE TRIGGER <<DATA-TABLE-NAME>>_changes_trigger
--      BEFORE INSERT OR UPDATE OR DELETE
--      ON <<DATA-TABLE-NAME>>
--      FOR EACH ROW
--      EXECUTE PROCEDURE data_table_changes();
--
--
----------------------------------------------------------------------------------
-- PG View prerequisites
----------------------------------------------------------------------------------
-- Couch converted views must follow the following rules to work
-- The view must be named DESIGN_NAME~VIEW_NAME as it would be defined in couchdb
-- Its assumed that DESIGN_NAME is the plural of the associated Table Name, unless override defined in kz_postgresql_schema:view_name_to_table_name/1
-- The view must return a column "_view_id" that will be placed in the "id" part of the JSON response
-- The view must return a column "_view_rev" that will be used by include_docs inner join to return the correct doc rev row from the table
-- The view must return '_view_db_name' column that is used to filter results for a particular database
-- The view must return at least one column beginning with _view_key_<<0..n>>  that that will be placed in the "key" part of the JSON response
-- The view can return columns beginning with _view_value_<<COLUMN_NAME>>  that that will be placed in the "value" map part of the JSON response
-- The view can alternatively return one column _view_value that will be the only value placed in the "value" map part of the JSON response
-- All other columns will be considered to be part of the JSON doc and will be added to the "doc" key of the response, (include_docs defined)
----------------------------------------------------------------------------------



----------------------------------------------------------------------------------
-- Tables
-- See notes on views at the top of this file for table prerequisites
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- status_stat
-- Table is used to store all kazoo docs of type status_stat
----------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS public.status_stat (
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 _deleted boolean NOT NULL,
 agent_id character varying(255),
 "timestamp" bigint,
 status character varying(255),
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_node character varying(255),
 pvt_type character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.status_stat OWNER TO kazoo;

CREATE TABLE IF NOT EXISTS public.status_stat_archive (
 _deleted boolean NOT NULL,
 _changed_on TIMESTAMP(6) NOT NULL,
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 _deleted boolean NOT NULL,
 agent_id character varying(255),
 "timestamp" bigint,
 status character varying(255),
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_node character varying(255),
 pvt_type character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.status_stat_archive OWNER TO kazoo;

----------------------------------------------------------------------------------
-- Auth
-- Table to store all Auth docs
----------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS public.auth (
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 auth_type character varying(255),
 auth_module character varying(255),
 status character varying(255),
 message character varying(255),
 client_ip character varying(255),
 timestamp bigint,
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_node character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.auth OWNER TO kazoo;

CREATE TABLE IF NOT EXISTS public.auth_archive (
 _deleted boolean NOT NULL,
 _changed_on TIMESTAMP(6) NOT NULL,
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 auth_type character varying(255),
 auth_module character varying(255),
 status character varying(255),
 message character varying(255),
 client_ip character varying(255),
 timestamp bigint,
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_node character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.auth_archive OWNER TO kazoo;

----------------------------------------------------------------------------------
-- call_stat
-- Table is used to store all kazoo docs of type call_stat
----------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS public.call_stat (
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 agent_id character varying(255),
 queue_id character varying(255),
 entered_timestamp bigint,
 handled_timestamp bigint,
 processed_timestamp bigint,
 hung_up_by character varying(255),
 entered_position bigint,
 is_callback boolean,
 misses jsonb,
 status character varying(255),
 caller_id_name character varying(255),
 caller_id_number character varying(255),
 required_skills jsonb,
 wait_time bigint,
 talk_time bigint,
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_node character varying(255),
 pvt_type character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.call_stat OWNER TO kazoo;

CREATE TABLE IF NOT EXISTS public.call_stat_archive (
 _deleted boolean NOT NULL,
 _changed_on TIMESTAMP(6) NOT NULL,
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 agent_id character varying(255),
 queue_id character varying(255),
 entered_timestamp bigint,
 handled_timestamp bigint,
 processed_timestamp bigint,
 hung_up_by character varying(255),
 entered_position bigint,
 is_callback boolean,
 misses jsonb,
 status character varying(255),
 caller_id_name character varying(255),
 caller_id_number character varying(255),
 required_skills jsonb,
 wait_time bigint,
 talk_time bigint,
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_node character varying(255),
 pvt_type character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.call_stat_archive OWNER TO kazoo;

----------------------------------------------------------------------------------
-- cdr
-- Table to store all CDR docs
----------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS public.cdr (
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 custom_channel_vars jsonb,
 to_uri character varying(255),
 to_tag character varying(255),
 "to" character varying(255),
 from_uri character varying(255),
 from_tag character varying(255),
 "from" character varying(255),
 e164_destination character varying(80),
 e164_origination character varying(80),
 "timestamp" bigint,
 ringing_seconds bigint,
 request character varying(255),
 presence_id character varying(255),
 media_server character varying(255),
 hangup_code character varying(255),
 hangup_cause character varying(255),
 endpoint_disposition character varying(255),
 duration_seconds bigint,
 disposition character varying(255),
 channel_created_time bigint,
 callee_id_number character varying(255),
 callee_id_name character varying(255),
 caller_id_number character varying(255),
 caller_id_name character varying(255),
 call_direction character varying(255),
 billing_seconds bigint,
 call_id character varying(255),
 other_leg_call_id character varying(255),
 node character varying(255),
 msg_id character varying(255),
 event_name character varying(255),
 event_category character varying(255),
 app_name character varying(255),
 is_conference boolean,
 interaction_id character varying(255),
 interaction_key character varying(255),
 interaction_time bigint,
 did_classifier character varying(255),
 pvt_vsn integer,
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_type character varying(255),
 pvt_node character varying(255),
 pvt_document_hash character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.cdr OWNER TO kazoo;

CREATE TABLE IF NOT EXISTS public.cdr_archive (
 _deleted boolean NOT NULL,
 _changed_on TIMESTAMP(6) NOT NULL,
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 custom_channel_vars jsonb,
 to_uri character varying(255),
 to_tag character varying(255),
 "to" character varying(255),
 from_uri character varying(255),
 from_tag character varying(255),
 "from" character varying(255),
 e164_destination character varying(80),
 e164_origination character varying(80),
 "timestamp" bigint,
 ringing_seconds bigint,
 request character varying(255),
 presence_id character varying(255),
 media_server character varying(255),
 hangup_code character varying(255),
 hangup_cause character varying(255),
 endpoint_disposition character varying(255),
 duration_seconds bigint,
 disposition character varying(255),
 channel_created_time bigint,
 callee_id_number character varying(255),
 callee_id_name character varying(255),
 caller_id_number character varying(255),
 caller_id_name character varying(255),
 call_direction character varying(255),
 billing_seconds bigint,
 call_id character varying(255),
 other_leg_call_id character varying(255),
 node character varying(255),
 msg_id character varying(255),
 event_name character varying(255),
 event_category character varying(255),
 app_name character varying(255),
 is_conference boolean,
 interaction_id character varying(255),
 interaction_key character varying(255),
 interaction_time bigint,
 did_classifier character varying(255),
 pvt_vsn integer,
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_type character varying(255),
 pvt_node character varying(255),
 pvt_document_hash character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.cdr_archive OWNER TO kazoo;

----------------------------------------------------------------------------------
-- lookup
-- Table is used to keep a reference of all docs by account name and doc id and there corresponding PG table
----------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS public.lookup (
 doc_id character varying(255) NOT NULL,
 db_name character varying(255) NOT NULL,
 table_name character varying(63) NOT NULL,
 PRIMARY KEY(doc_id, db_name)
);
ALTER TABLE public.lookup OWNER TO kazoo;

----------------------------------------------------------------------------------
-- Other
-- Table is used to store all kazoo docs that do not have a defined PG table
----------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS public.other (
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_node character varying(255),
 pvt_type character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.other OWNER TO kazoo;

CREATE TABLE IF NOT EXISTS public.other_archive (
 _deleted boolean NOT NULL,
 _changed_on TIMESTAMP(6) NOT NULL,
 _id character varying(255) NOT NULL,
 _rev integer NOT NULL,
 pvt_account_id character varying(255),
 pvt_account_db character varying(255) NOT NULL,
 pvt_created bigint,
 pvt_modified bigint,
 pvt_node character varying(255),
 pvt_type character varying(255),
 other_json jsonb,
 PRIMARY KEY(_id, _rev)
);
ALTER TABLE public.other_archive OWNER TO kazoo;

----------------------------------------------------------------------------------
-- Couch PG Views
-- See notes on views at the top of this file for view prerequisites
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Agent_stats Views
----------------------------------------------------------------------------------
CREATE OR REPLACE VIEW public."agent_stats~most_recent_by_agent" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  agent_id AS _view_key_0,
  timestamp AS _view_key_1
 FROM public.status_stat
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."agent_stats~most_recent_by_agent" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."agent_stats~most_recent_by_timestamp" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  timestamp AS _view_key_0,
  agent_id AS _view_key_1
 FROM public.status_stat
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."agent_stats~most_recent_by_timestamp" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."agent_stats~status_log" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  agent_id AS _view_key_0,
  timestamp AS _view_key_1,
  status AS _view_value
 FROM public.status_stat
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."agent_stats~status_log" OWNER TO kazoo;

----------------------------------------------------------------------------------
-- Auth Views
----------------------------------------------------------------------------------
CREATE OR REPLACE VIEW public."auth~login_attempt_by_auth_module" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  auth_module AS _view_key_0,
  pvt_created AS _view_key_1,
  _id AS _view_value_id,
  auth_type AS _view_value_auth_type,
  auth_module AS _view_value_auth_module,
  status AS _view_value_status,
  message AS _view_value_message,
  timestamp AS _view_value_timestamp,
  client_ip AS _view_value_client_ip
 FROM public.auth
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."auth~login_attempt_by_auth_module" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."auth~login_attempt_by_auth_type" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  auth_type AS _view_key_0,
  pvt_created AS _view_key_1,
  _id AS _view_value_id,
  auth_type AS _view_value_auth_type,
  auth_module AS _view_value_auth_module,
  status AS _view_value_status,
  message AS _view_value_message,
  timestamp AS _view_value_timestamp,
  client_ip AS _view_value_client_ip
 FROM public.auth
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."auth~login_attempt_by_auth_type" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."auth~login_attempt_by_status" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  status AS _view_key_0,
  pvt_created AS _view_key_1,
  _id AS _view_value_id,
  auth_type AS _view_value_auth_type,
  auth_module AS _view_value_auth_module,
  status AS _view_value_status,
  message AS _view_value_message,
  timestamp AS _view_value_timestamp,
  client_ip AS _view_value_client_ip
 FROM public.auth
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."auth~login_attempt_by_status" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."auth~login_attempt_by_time" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  pvt_created AS _view_key_0,
  _id AS _view_value_id,
  auth_type AS _view_value_auth_type,
  auth_module AS _view_value_auth_module,
  status AS _view_value_status,
  message AS _view_value_message,
  timestamp AS _view_value_timestamp,
  client_ip AS _view_value_client_ip
 FROM public.auth
 ORDER BY _view_key_0;
ALTER TABLE public."auth~login_attempt_by_time" OWNER TO kazoo;

----------------------------------------------------------------------------------
-- Call_stats Views
----------------------------------------------------------------------------------
CREATE OR REPLACE VIEW public."call_stats~call_log" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  queue_id AS _view_key_0,
  entered_timestamp AS _view_key_1
 FROM public.call_stat
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."call_stats~call_log" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."call_stats~crossbar_listing" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  entered_timestamp AS _view_key_0,
  _id AS _view_value_id,
  entered_timestamp AS _view_value_entered_timestamp,
  handled_timestamp AS _view_value_handled_timestamp,
  caller_id_number AS _view_value_caller_id_number,
  caller_id_name AS _view_value_caller_id_name,
  entered_position AS _view_value_entered_position,
  status AS _view_value_status,
  agent_id AS _view_value_agent_id,
  wait_time AS _view_value_wait_time,
  talk_time AS _view_value_talk_time,
  queue_id AS _view_value_queue_id
 FROM public.call_stat
 ORDER BY _view_key_0;
ALTER TABLE public."call_stats~crossbar_listing" OWNER TO kazoo;

----------------------------------------------------------------------------------
-- CDR Views
----------------------------------------------------------------------------------
CREATE OR REPLACE VIEW public."cdrs~crossbar_listing" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  pvt_created AS _view_key_0,
  _id AS _view_key_1,
  _id AS _view_value_id,
  call_id AS _view_value_call_id,
  caller_id_name AS _view_value_caller_id_name,
  caller_id_number AS _view_value_caller_id_number,
  callee_id_name AS _view_value_callee_id_name,
  callee_id_number AS _view_value_callee_id_number,
  duration_seconds AS _view_value_duration_seconds,
  interaction_id AS _view_value_interaction_id,
  billing_seconds AS _view_value_billing_seconds,
  "timestamp" AS _view_value_timestamp,
  hangup_cause AS _view_value_hangup_cause,
  other_leg_call_id AS _view_value_other_leg_call_id,
  custom_channel_vars->>'owner_id' AS _view_value_owner_id,
  to_uri AS _view_value_to,
  from_uri AS _view_value_from,
  custom_channel_vars->>'inception' AS _view_value_inception,
  call_direction AS _view_value_direction,
  request AS _view_value_request,
  custom_channel_vars->>'authorizing_id' AS _view_value_authorizing_id,
  custom_channel_vars->>'media_recordings' AS _view_value_media_recordings
 FROM public.cdr
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."cdrs~crossbar_listing" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."cdrs~interaction_listing" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  interaction_time AS _view_key_0,
  interaction_key AS _view_key_1,
  COALESCE(channel_created_time, ("timestamp" - duration_seconds)) AS _view_key_2,
  _id AS _view_value_id,
  COALESCE(channel_created_time, ("timestamp" - duration_seconds)) AS _view_value_channel_time,
  COALESCE((other_json ->> 'channel_loopback_leg'::text), '_'::text) AS _view_value_leg
 FROM public.cdr
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."cdrs~interaction_listing" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."cdrs~interaction_listing_by_id" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  interaction_id AS _view_key_0,
  channel_created_time AS _view_key_1
 FROM public.cdr
 ORDER BY _view_key_0, _view_key_1;
 ALTER TABLE public."cdrs~interaction_listing_by_id" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."cdrs~interaction_listing_by_owner" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  custom_channel_vars->>'owner_id' AS _view_key_0,
  interaction_time AS _view_key_1,
  interaction_key AS _view_key_2,
  channel_created_time AS _view_key_3,
  _id AS _view_value_id,
  channel_created_time AS _view_value_channel_time,
  COALESCE((other_json ->> 'channel_loopback_leg'::text), '_'::text) AS _view_value_leg
 FROM public.cdr
 WHERE custom_channel_vars->>'owner_id' IS NOT NULL
  AND custom_channel_vars IS NOT NULL
 ORDER BY _view_key_0, _view_key_1, _view_key_2, _view_key_3;
ALTER TABLE public."cdrs~interaction_listing_by_owner" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."cdrs~listing_by_owner" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  pvt_account_db AS _view_db_name,
  custom_channel_vars->>'owner_id' AS _view_key_0,
  pvt_created AS _view_key_1,
  _id AS _view_key_2,
  _id AS _view_value_id,
  call_id AS _view_value_call_id,
  caller_id_name AS _view_value_caller_id_name,
  caller_id_number AS _view_value_caller_id_number,
  callee_id_name AS _view_value_callee_id_name,
  callee_id_number AS _view_value_callee_id_number,
  duration_seconds AS _view_value_duration_seconds,
  billing_seconds AS _view_value_billing_seconds,
  "timestamp" AS _view_value_timestamp,
  hangup_cause AS _view_value_hangup_cause,
  other_leg_call_id AS _view_value_other_leg_call_id,
  call_direction AS _view_value_call_direction,
  to_uri AS _view_value_to,
  from_uri AS _view_value_from,
  custom_channel_vars->>'inception' AS _view_value_inception
 FROM public.cdr
 WHERE custom_channel_vars->>'owner_id' IS NOT NULL
  AND custom_channel_vars IS NOT NULL
 ORDER BY _view_key_0, _view_key_1, _view_key_2;
ALTER TABLE public."cdrs~listing_by_owner" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."cdrs~offnet-calls" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  lookup.db_name AS _view_db_name,
  call_id AS _view_key_0,
  pvt_created AS _view_key_1,
  _id AS _view_value_id,
  call_id AS _view_value_call_id,
  caller_id_name AS _view_value_caller_id_name,
  caller_id_number AS _view_value_caller_id_number,
  callee_id_name AS _view_value_callee_id_name,
  callee_id_number AS _view_value_callee_id_number,
  duration_seconds AS _view_value_duration_seconds,
  "timestamp" AS _view_value_timestamp,
  hangup_cause AS _view_value_hangup_cause,
  other_leg_call_id AS _view_value_other_leg_call_id,
  call_direction AS _view_value_call_direction,
  to_uri AS _view_value_to,
  from_uri AS _view_value_from,
  custom_channel_vars->>'inception' AS _view_value_inception
 FROM (public.cdr
  JOIN public.lookup ON ((((cdr._id)::text = (lookup.doc_id)::text) AND ((lookup.table_name)::text = 'cdr'::text))))
 WHERE custom_channel_vars IS NOT NULL
  AND custom_channel_vars->>'global_resource' IS NOT NULL
  AND (custom_channel_vars->>'global_resource')::boolean = true
  AND custom_channel_vars->>'channel_authorized' IS NOT NULL
  AND (custom_channel_vars->>'channel_authorized')::boolean = true
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."cdrs~offnet-calls" OWNER TO kazoo;

CREATE OR REPLACE VIEW public."cdrs~summarize_cdrs" AS
 SELECT
  _id AS _view_id,
  _rev AS _view_rev,
  lookup.db_name as _view_db_name,
  pvt_created AS _view_key_0,
  _id AS _view_key_1,
  call_direction AS _view_value_direction,
  billing_seconds::integer AS _view_value_billable_seconds,
  CASE
WHEN call_direction = 'inbound'
 THEN SUBSTRING(cdr."from", POSITION('@' IN cdr."from")+1)
ELSE SUBSTRING(cdr."to", POSITION('@' IN cdr."to")+1)
  END AS _view_value_domain,
  CASE
WHEN "to" ~ '^\+1((?:800|888|877|866|855)\d{7})\@'
 THEN 'tollfree_us'
WHEN "to" ~ '^\+1(900\d{7})\@'
 THEN 'toll_us'
WHEN "to" ~ '^(911|933)\@'
 THEN 'emergency'
WHEN "to" ~ '^\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\d{7})\@'
 THEN 'caribbean'
WHEN "to" ~ '^\+?1?([2-9][0-9]{2}[2-9][0-9]{6})\@'
 THEN 'did_us'
WHEN "to" ~ '^\+[2-9]\d{7,}\@'
 THEN 'international'
ELSE 'unknown'
  END AS _view_value_classifier,
  CASE
WHEN custom_channel_vars->>'rate_name' IS NOT NULL
 THEN custom_channel_vars->>'rate_name'
ELSE 'null'
  END AS _view_value_rate_name
 FROM (public.cdr
  JOIN public.lookup ON ((((cdr._id)::text = (lookup.doc_id)::text) AND ((lookup.table_name)::text = 'cdr'::text))))
 WHERE custom_channel_vars IS NOT NULL
  AND other_json->>'channel_name' !~ '(\d*|\w*)*loopback'
  AND NOT(cdr.call_direction = 'inbound' AND cdr.custom_channel_vars->>'authorizing_id' IS NOT NULL)
  AND NOT(cdr.call_direction = 'outbound' AND cdr.custom_channel_vars->>'resource_id' IS NULL)
 ORDER BY _view_key_0, _view_key_1;
ALTER TABLE public."cdrs~summarize_cdrs" OWNER TO kazoo;


----------------------------------------------------------------------------------
-- PG Functions
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-- Handle INSERT, UPDATE or DELETE on data tables
-- This trigger function should be called for all INSERT, UPDATE and DELETE calls
-- to non archive tables
-- This will take care of row/document revision and keeping copies of data in archive
-- tables when data is altered
----------------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION data_table_changes()
  RETURNS trigger AS $BODY$
  DECLARE
    archive_table_name VARCHAR := TG_TABLE_NAME || '_archive';
  BEGIN
    -- Verify _id, pvt_account_db are set for INSERT and UPDATES
    IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
      IF NEW._id IS NULL THEN
        RAISE EXCEPTION '_id cannot be null';
      END IF;
      IF NEW.pvt_account_db IS NULL THEN
        RAISE EXCEPTION 'pvt_account_db cannot be null';
      END IF;
    END IF;

    -- Handle INSERT to table
    IF TG_OP = 'INSERT' THEN
      -- Add entry in to 'lookup' table
      -- Calculate next rev for the doc/row and set it on NEW
      -- Return NEW so it is added to the table
      INSERT INTO lookup(doc_id, db_name, table_name)
          VALUES(NEW._id, NEW.pvt_account_db, TG_TABLE_NAME);
      EXECUTE format('SELECT coalesce(MAX(_rev) + 1, 1) FROM %I.%I WHERE _id = $1', TG_TABLE_SCHEMA, archive_table_name)
          INTO NEW._rev
          USING NEW._id;
      --NEW._rev := revision
      RETURN NEW;

    -- Handle UPDATE to table
    ELSIF TG_OP = 'UPDATE' THEN
      -- Copy old row to archive with _delete set to false and increment NEW rev by 1
      -- Return NEW so it is added to the table
      EXECUTE format('INSERT INTO %I.%I SELECT false, now(), $1.*', TG_TABLE_SCHEMA, archive_table_name)
        USING OLD;
      NEW._rev = OLD._rev + 1;
      RETURN NEW;

    -- Handle DELETE to table
    ELSIF TG_OP = 'DELETE' THEN
      -- Copy OLD row to archive with _delete set to true and delete lookup table row
      -- Return OLD so it is deleted from the table
      EXECUTE format('INSERT INTO %I.%I SELECT true, now(), $1.*', TG_TABLE_SCHEMA, archive_table_name)
        USING OLD;
      DELETE FROM lookup WHERE doc_id = OLD._id AND db_name = OLD.pvt_account_db;
      RETURN OLD;
    END IF;
  END;
  $BODY$ LANGUAGE plpgsql;


----------------------------------------------------------------------------------
-- PG Triggers
-- See notes on table schema at the top of this file
-- Every data table must have a trigger for INSERT, DELETE and UPDATE
----------------------------------------------------------------------------------
CREATE TRIGGER status_stat_changes_trigger
  BEFORE INSERT OR UPDATE OR DELETE
  ON status_stat
  FOR EACH ROW
  EXECUTE PROCEDURE data_table_changes();

CREATE TRIGGER auth_changes_trigger
  BEFORE INSERT OR UPDATE OR DELETE
  ON auth
  FOR EACH ROW
  EXECUTE PROCEDURE data_table_changes();

CREATE TRIGGER call_stat_changes_trigger
  BEFORE INSERT OR UPDATE OR DELETE
  ON call_stat
  FOR EACH ROW
  EXECUTE PROCEDURE data_table_changes();

CREATE TRIGGER cdr_changes_trigger
  BEFORE INSERT OR UPDATE OR DELETE
  ON cdr
  FOR EACH ROW
  EXECUTE PROCEDURE data_table_changes();



--
-- Kazoo PostgreSQL database schema complete
--
