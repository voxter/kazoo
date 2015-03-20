-module(amimulator_hook_map).
-behaviour(gen_server).

-export([start_link/0, register_all/3, handle_event/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("amimulator.hrl").

-record(state, {
    mappings = []
}).

%%
%% Public functions
%%
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_all(Responders, AccountId, Pid) ->
	gen_server:cast(?MODULE, {register_all, Responders, AccountId, Pid}).

handle_event(EventJObj, _Props) ->
	%lager:debug("amimulator_hook_map handled event with props ~p", [Props]),
	gen_server:cast(?MODULE, {handle, EventJObj}).

%%
%% gen_server callbacks
%%
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
    
handle_call(_Request, _From, State) ->
    {noreply, State}.
    
handle_cast({register_all, Responders, AccountId, Pid}, State) ->
	{noreply, register_all(Responders, AccountId, Pid, State)};
handle_cast({handle, EventJObj}, #state{mappings=Mappings}=State) ->
	pvt_handle_event(EventJObj, Mappings),
	{noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(shutdown, #state{}) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Private functions
%%
pvt_handle_event(EventJObj, Mappings) ->
	case maybe_find_event_account_id(EventJObj) of
		undefined ->
			ok;
		AccountId ->
			maybe_send_to_handler(EventJObj, find_mapping(AccountId,
				wh_json:get_value(<<"Event-Category">>, EventJObj),
				wh_json:get_value(<<"Event-Name">>, EventJObj),
				Mappings
			))
	end.

maybe_find_event_account_id(EventJObj) ->
	case wh_json:get_value(<<"Account-ID">>, EventJObj) of
		undefined ->
			Realm = wh_json:get_value(<<"Realm">>, EventJObj),
			case couch_mgr:get_results(<<"accounts">>, <<"accounts/listing_by_realm">>, [{key, Realm}]) of
				{ok, [Result]} ->
					wh_json:get_value([<<"value">>, <<"account_id">>], Result);
				_ ->
					undefined
			end;
		AccountId ->
			AccountId
	end.

maybe_send_to_handler(_EventJObj, undefined) ->
	ok;
maybe_send_to_handler(EventJObj, {Srv, Mod, Fun}) ->
	gen_listener:cast(Srv, {handle, Mod, Fun, EventJObj}).

find_mapping(_AccountId, _EventCategory, _EventName, []) ->
	undefined;
find_mapping(AccountId, EventCategory, EventName, [{AccountId, {Srv, {Mod, Fun},
	[{EventCategory, EventName}|_Events]}}|_Mappings]) ->
		{Srv, Mod, Fun};
find_mapping(AccountId, EventCategory, EventName, [_|Mappings]) ->
	find_mapping(AccountId, EventCategory, EventName, Mappings).

register_all([], _AccountId, _Pid, State) ->
	State;
register_all([{Srv, {Mod, Fun}, Events}|Responders], AccountId, Pid, #state{mappings=Mappings}=State) ->
	gen_listener:add_responder(
		Srv, {?MODULE, handle_event}, Events
	),
	register_all(Responders, AccountId, Pid, State#state{mappings=[{AccountId, {Pid, {Mod, Fun}, Events}}] ++ Mappings}).
