%%%-------------------------------------------------------------------
%%% @copyright (C) 2017 Voxter Communications
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(kz_service_seats).

-export([reconcile/1]).
% -export([reconcile/2]).

-include("kazoo_services.hrl").

-define(SERVICE_CATEGORY, <<"seats">>).

-record(state, {account_id     = 'undefined' :: api_binary()
               ,account_db     = 'undefined' :: api_binary()
               ,basic_seats    = 0           :: non_neg_integer()
               ,standard_seats = 0           :: non_neg_integer()
               ,premium_seats  = 0           :: non_neg_integer()
               ,users_todo     = []          :: kz_json:objects()
               ,users_next     = []          :: kz_json:objects() %% Users that may be basic or standard, resolve later
               ,users_done     = []          :: kz_json:objects()
               ,devices_todo   = []          :: kz_json:objects()
               ,devices_done   = []          :: kz_json:objects()
               ,owned_devices  = []          :: kz_json:objects()
               ,vmboxes_todo   = []          :: kz_json:objects()
               ,callflows_todo = []          :: kz_json:objects()
               }).
-type state() :: #state{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    {State, _} = reconcile_seats(Services),
    kz_services:update(?SERVICE_CATEGORY, <<"basic">>, State#state.basic_seats
                      ,kz_services:update(?SERVICE_CATEGORY, <<"standard">>, State#state.standard_seats
                                         ,kz_services:update(?SERVICE_CATEGORY, <<"premium">>, State#state.premium_seats, Services))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Start the process of reconciling seats.
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_seats(kz_services:services()) -> {state(), kz_services:services()}.
reconcile_seats(Services) ->
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Users = fetch_users(AccountDb),
    OwnedDevices = fetch_owned_devices(AccountDb),
    reconcile_user_seats(#state{account_id=AccountId
                               ,account_db=AccountDb
                               ,users_todo=Users
                               ,owned_devices=OwnedDevices
                               }
                        ,Services).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle users that should be considered premium seats (user owns
%% more than one device).
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_user_seats(state(), kz_services:services()) ->
                                  {state(), kz_services:services()}.
reconcile_user_seats(#state{account_db=AccountDb
                           ,users_todo=[]
                           }=State, Services) ->
    Vmboxes = fetch_vmboxes(AccountDb),
    reconcile_vmbox_seats(State#state{vmboxes_todo=Vmboxes}, Services);
reconcile_user_seats(#state{premium_seats=PremiumSeats
                           ,users_todo=[User|Users]
                           ,users_next=UsersNext
                           ,users_done=UsersDone
                           ,devices_done=DevicesDone
                           ,owned_devices=OwnedDevices
                           }=State, Services) ->
    UserId = kz_json:get_ne_binary_value(<<"id">>, User),
    {UserDevices, RemainingDevices} = partition_owned_devices(UserId, OwnedDevices),
    lager:debug("check for premium in user ~s", [UserId]),

    %% Premium users have more than one owned device
    State1 = case length(UserDevices) > 1 of
                 'true' ->
                     lager:debug("user ~s is premium", [UserId]),
                     State#state{premium_seats=PremiumSeats + 1
                                ,users_done=[User|UsersDone]
                                ,devices_done=UserDevices ++ DevicesDone
                                ,owned_devices=RemainingDevices
                                };
                 'false' ->
                     State#state{users_next=[User|UsersNext]}
             end,
    reconcile_user_seats(State1#state{users_todo=Users}, Services).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle users that have a voicemail box and should therefore be
%% considered standard seats.
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_vmbox_seats(state(), kz_services:services()) ->
                                   {state(), kz_services:services()}.
reconcile_vmbox_seats(#state{account_db=AccountDb
                            ,vmboxes_todo=[]
                            }=State
                     ,Services) ->
    Callflows = fetch_callflows(AccountDb),
    reconcile_standard_seats_due_to_dids(State#state{callflows_todo=Callflows}, Services);
reconcile_vmbox_seats(#state{standard_seats=StandardSeats
                            ,users_next=UsersNext
                            ,users_done=UsersDone
                            ,devices_done=DevicesDone
                            ,owned_devices=OwnedDevices
                            ,vmboxes_todo=[Vmbox|Vmboxes]
                            }=State, Services) ->
    lager:debug("check for standard due to vm ~s", [kz_json:get_ne_binary_value(<<"id">>, Vmbox)]),
    OwnerId = kz_json:get_ne_binary_value([<<"value">>, <<"owner_id">>], Vmbox),
    %% Only consider a user to be standard due to VM if they exist, and have
    %% not already been included in the counts
    case OwnerId =:= 'undefined'
            orelse exists(OwnerId, UsersDone)
            orelse not exists(OwnerId, UsersNext)
    of
        'true' ->
            reconcile_vmbox_seats(State#state{vmboxes_todo=Vmboxes}, Services);
        'false' ->
            lager:debug("user ~s is standard due to vm", [OwnerId]),
            UsersNext1 = filter_out_user_id(OwnerId, UsersNext),
            UsersDone1 = [kz_json:set_value(<<"id">>, OwnerId, kz_json:new())
                          | UsersDone
                         ],
            {UserDevices, RemainingDevices} = partition_owned_devices(OwnerId, OwnedDevices),
            reconcile_vmbox_seats(State#state{standard_seats=StandardSeats + 1
                                             ,users_next=UsersNext1
                                             ,users_done=UsersDone1
                                             ,devices_done=UserDevices ++ DevicesDone
                                             ,owned_devices=RemainingDevices
                                             ,vmboxes_todo=Vmboxes
                                             }, Services)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle users or devices that appear in callflows. If they are in a
%% callflow that has a DID, they are considered standard seats.
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_standard_seats_due_to_dids(state(), kz_services:services()) ->
                                                  {state(), kz_services:services()}.
reconcile_standard_seats_due_to_dids(#state{account_db=AccountDb
                                           ,callflows_todo=[]
                                           }=State
                                    ,Services) ->
    Devices = fetch_devices(AccountDb),
    reconcile_basic_seats(State#state{devices_todo=Devices}, Services);
reconcile_standard_seats_due_to_dids(#state{callflows_todo=[Callflow|Callflows]}=State
                                    ,Services) ->
    %% Having a DID will make a user or device a standard seat
    case callflow_has_did(Callflow) of
        'true' ->
            CallflowId = kz_json:get_ne_binary_value(<<"id">>, Callflow),
            lager:debug("check for standard due to dids in callflow ~s", [CallflowId]),
            check_standard_seat_in_callflow(State, Services);
        'false' ->
            reconcile_standard_seats_due_to_dids(State#state{callflows_todo=Callflows}
                                                ,Services)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle all remaining seats as basic. Remaining users are done first
%% in order to filter out their device. Then remaining devices are
%% counted.
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_basic_seats(state(), kz_services:services()) ->
                                   {state(), kz_services:services()}.
reconcile_basic_seats(#state{users_next=[]
                            ,devices_todo=[]
                            }=State, Services) ->
    {State, Services};
reconcile_basic_seats(#state{basic_seats=BasicSeats
                            ,users_next=[]
                            ,devices_todo=[Device|Devices]
                            ,devices_done=DevicesDone
                            }=State, Services) ->
    DeviceId = kz_json:get_ne_binary_value(<<"id">>, Device),
    lager:debug("fall back to basic for device ~s", [DeviceId]),

    State1 = State#state{devices_todo=Devices},
    %% Only devices that have not been counted up to this point are basic
    case exists(DeviceId, DevicesDone) of
        'true' -> reconcile_basic_seats(State1, Services);
        'false' ->
            lager:debug("device ~s is basic", [DeviceId]),
            reconcile_basic_seats(State1#state{basic_seats=BasicSeats + 1
                                              ,devices_todo=Devices
                                              ,devices_done=[Device|DevicesDone]
                                              }, Services)
    end;
reconcile_basic_seats(#state{basic_seats=BasicSeats
                            ,users_next=[User|Users]
                            ,devices_done=DevicesDone
                            ,owned_devices=OwnedDevices
                            }=State, Services) ->
    UserId = kz_json:get_ne_binary_value(<<"id">>, User),
    lager:debug("user ~s is basic", [UserId]),
    {UserDevices, RemainingDevices} = partition_owned_devices(UserId, OwnedDevices),
    reconcile_basic_seats(State#state{basic_seats=BasicSeats + 1
                                     ,users_next=Users
                                     ,devices_done=UserDevices ++ DevicesDone
                                     ,owned_devices=RemainingDevices
                                     }, Services).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return true if a given callflow JSON object has a DID assigned.
%%
%% @end
%%--------------------------------------------------------------------
-spec callflow_has_did(kz_json:object()) -> boolean().
callflow_has_did(Callflow) ->
    callflow_has_did2(kz_json:get_list_value([<<"value">>, <<"numbers">>], Callflow)).

-spec callflow_has_did2(api_binaries()) -> boolean().
callflow_has_did2('undefined') -> 'false';
callflow_has_did2([]) -> 'false';
callflow_has_did2([Number|Numbers]) ->
    case knm_converters:is_reconcilable(Number) of
        'true' -> 'true';
        'false' -> callflow_has_did2(Numbers)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% For callflows that have a DID, determine if there is a user or
%% device in the flow that should be considered as standard.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_standard_seat_in_callflow(state(), kz_services:services()) ->
                                             {state(), kz_services:services()}.
check_standard_seat_in_callflow(#state{account_db=AccountDb
                                      ,standard_seats=StandardSeats
                                      ,users_next=UsersNext
                                      ,users_done=UsersDone
                                      ,devices_done=DevicesDone
                                      ,owned_devices=OwnedDevices
                                      ,callflows_todo=[Callflow|Callflows]
                                      }=State
                               ,Services) ->
    CallflowId = kz_json:get_ne_binary_value(<<"id">>, Callflow),
    {'ok', CallflowJObj} = kz_datamgr:open_cache_doc(AccountDb, CallflowId),

    State1 = case basic_seat_in_callflow(CallflowJObj, UsersNext, UsersDone, DevicesDone) of
                  'false' -> State;
                  {'device', Device} -> device_detected_standard(Device, State);
                  {'user', User} ->
                      UserId = kz_json:get_ne_binary_value(<<"id">>, User),
                      lager:debug("user ~s is standard due to did", [UserId]),
                      UsersNext1 = filter_out_user_id(UserId, UsersNext),
                      {UserDevices, RemainingDevices} = partition_owned_devices(UserId, OwnedDevices),
                      State#state{standard_seats=StandardSeats + 1
                                 ,users_next=UsersNext1
                                 ,users_done=[User|UsersDone]
                                 ,devices_done=UserDevices ++ DevicesDone
                                 ,owned_devices=RemainingDevices
                                 }
             end,
    reconcile_standard_seats_due_to_dids(State1#state{callflows_todo=Callflows}, Services).

-spec basic_seat_in_callflow(kz_json:object(), kz_json:objects(), kz_json:objects(), kz_json:objects()) ->
                                    {'device', kz_json:object()} |
                                    {'user', kz_json:object()} |
                                    'false'.
basic_seat_in_callflow(CallflowJObj, UsersNext, UsersDone, DevicesDone) ->
    Flow = kz_json:get_json_value(<<"flow">>, CallflowJObj, kz_json:new()),
    basic_seat_in_callflow2([Flow], UsersNext, UsersDone, DevicesDone).

-spec basic_seat_in_callflow2(kz_json:objects(), kz_json:objects(), kz_json:objects(), kz_json:objects()) ->
                                     {'device', kz_json:object()} |
                                     {'user', kz_json:object()} |
                                     'false'.
basic_seat_in_callflow2([], _, _, _) -> 'false';
basic_seat_in_callflow2([Node|Nodes], UsersNext, UsersDone, DevicesDone) ->
    case unmatched_basic_seat(Node, UsersDone, DevicesDone) of
        'false' ->
            Children = kz_json:values(<<"children">>, Node),
            basic_seat_in_callflow2(Nodes ++ Children, UsersNext, UsersDone, DevicesDone);
        %% Special case - callflows can reference deleted users
        %% Only consider this a match if the user exists and has not been matched
        {'user', User}=Match ->
            UserId = kz_json:get_ne_binary_value(<<"id">>, User),
            case exists(UserId, UsersNext) of
                'true' -> Match;
                'false' ->
                    Children = kz_json:values(<<"children">>, Node),
                    basic_seat_in_callflow2(Nodes ++ Children, UsersNext, UsersDone, DevicesDone)
            end;
        Match -> Match
    end.

-spec unmatched_basic_seat(kz_json:object(), kz_json:objects(), kz_json:objects()) ->
                                  {'device', kz_json:object()} |
                                  {'user', kz_json:object()} |
                                  'false'.
unmatched_basic_seat(Node, UsersDone, DevicesDone) ->
    Module = kz_json:get_ne_binary_value(<<"module">>, Node),
    case Module of
        <<"device">> ->
            DeviceId = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], Node),
            unmatched_basic_seat_device(DeviceId, DevicesDone);
        <<"user">> ->
            UserId = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], Node),
            unmatched_basic_seat_user(UserId, UsersDone);
        _ -> 'false'
    end.

-spec unmatched_basic_seat_device(ne_binary(), kz_json:objects()) ->
                                         {'device', kz_json:object()} | 'false'.
unmatched_basic_seat_device(DeviceId, []) ->
    Device = kz_json:set_value(<<"id">>, DeviceId, kz_json:new()),
    {'device', Device};
unmatched_basic_seat_device(DeviceId, [Device|Devices]) ->
    case kz_json:get_ne_binary_value(<<"id">>, Device) of
        DeviceId -> 'false';
        _ -> unmatched_basic_seat_device(DeviceId, Devices)
    end.

-spec unmatched_basic_seat_user(ne_binary(), kz_json:objects()) ->
                                       {'user', kz_json:object()} | 'false'.
unmatched_basic_seat_user(UserId, []) ->
    User = kz_json:set_value(<<"id">>, UserId, kz_json:new()),
    {'user', User};
unmatched_basic_seat_user(UserId, [User|Users]) ->
    case kz_json:get_ne_binary_value(<<"id">>, User) of
        UserId -> 'false';
        _ -> unmatched_basic_seat_user(UserId, Users)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% When a device is detected as a standard seat (due to DID), check if
%% it is an owned device, and then associate the user with the seat.
%%
%% @end
%%--------------------------------------------------------------------
-spec device_detected_standard(kz_json:object(), state()) -> state().
-spec device_detected_standard(kz_json:object(), kz_json:objects(), state()) ->
                                      state().
device_detected_standard(Device, #state{owned_devices=OwnedDevices}=State) ->
    device_detected_standard(Device, OwnedDevices, State).

device_detected_standard(Device, [], #state{standard_seats=StandardSeats
                                           ,devices_done=DevicesDone
                                           }=State) ->
    DeviceId = kz_json:get_ne_binary_value(<<"id">>, Device),
    lager:debug("device ~s is standard due to did", [DeviceId]),
    State#state{standard_seats=StandardSeats + 1
               ,devices_done=[Device|DevicesDone]
               };
device_detected_standard(Device, [OwnedDevice|Devices], #state{standard_seats=StandardSeats
                                                              ,users_next=UsersNext
                                                              ,users_done=UsersDone
                                                              ,devices_done=DevicesDone
                                                              ,owned_devices=OwnedDevices
                                                              }=State) ->
    DeviceId = kz_json:get_ne_binary_value(<<"id">>, Device),
    case kz_json:get_ne_binary_value(<<"id">>, OwnedDevice) of
        %% The device is an owned device
        DeviceId ->
            OwnerId = kz_json:get_ne_binary_value(<<"key">>, OwnedDevice),
            %% Special case - device owner_id can reference deleted users
            %% Only bundle this device with the owner if the owner is not deleted
            case exists(OwnerId, UsersNext) of
                'true' ->
                    lager:debug("user ~s is standard due to did with a device they own", [OwnerId]),
                    UsersNext1 = filter_out_user_id(OwnerId, UsersNext),
                    UsersDone1 = [kz_json:set_value(<<"id">>, OwnerId, kz_json:new())
                                  | UsersDone
                                 ],
                    {UserDevices, RemainingDevices} = partition_owned_devices(OwnerId, OwnedDevices),
                    State#state{standard_seats=StandardSeats + 1
                               ,users_next=UsersNext1
                               ,users_done=UsersDone1
                               ,devices_done=UserDevices ++ DevicesDone
                               ,owned_devices=RemainingDevices
                               };
                'false' ->
                    device_detected_standard(Device, Devices, State)
            end;
        _ ->
            device_detected_standard(Device, Devices, State)
    end.

-spec fetch_users(ne_binary()) -> kz_json:objects().
fetch_users(AccountDb) ->
    case kz_datamgr:get_results(AccountDb, <<"users/crossbar_listing">>) of
        {'ok', JObjs} -> JObjs;
        {'error', _R} ->
            lager:error("unable to get users: ~p", [_R]),
            []
    end.

-spec fetch_devices(ne_binary()) -> kz_json:objects().
fetch_devices(AccountDb) ->
    case kz_datamgr:get_results(AccountDb, <<"devices/crossbar_listing">>) of
        {'ok', JObjs} -> JObjs;
        {'error', _R} ->
            lager:error("unable to get devices: ~p", [_R]),
            []
    end.

-spec fetch_owned_devices(ne_binary()) -> kz_json:objects().
fetch_owned_devices(AccountDb) ->
    case kz_datamgr:get_results(AccountDb, <<"devices/listing_by_owner">>) of
        {'ok', JObjs} -> JObjs;
        {'error', _R} ->
            lager:error("unable to get owned devices: ~p", [_R]),
            []
    end.

-spec fetch_vmboxes(ne_binary()) -> kz_json:objects().
fetch_vmboxes(AccountDb) ->
    case kz_datamgr:get_results(AccountDb, <<"vmboxes/crossbar_listing">>) of
        {'ok', JObjs} -> JObjs;
        {'error', _R} ->
            lager:error("unable to get vmboxes: ~p", [_R]),
            []
    end.

-spec fetch_callflows(ne_binary()) -> kz_json:objects().
fetch_callflows(AccountDb) ->
    case kz_datamgr:get_results(AccountDb, <<"callflows/crossbar_listing">>) of
        {'ok', JObjs} -> JObjs;
        {'error', _R} ->
            lager:error("unable to get callflows: ~p", [_R]),
            []
    end.

-spec filter_out_user_id(ne_binary(), kz_json:objects()) -> kz_json:objects().
filter_out_user_id(UserId, Users) ->
    lists:filter(fun(User) ->
                     case kz_json:get_ne_binary_value(<<"id">>, User) of
                         UserId -> 'false';
                         _ -> 'true'
                     end
                 end, Users).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Partition a list of devices by an owning user Id.
%%
%% @end
%%--------------------------------------------------------------------
-spec partition_owned_devices(ne_binary(), kz_json:objects()) ->
                                     {kz_json:objects(), kz_json:objects()}.
partition_owned_devices(UserId, Devices) ->
    lists:partition(fun(Device) ->
                        case kz_json:get_ne_binary_value(<<"key">>, Device) of
                            UserId -> 'true';
                            _ -> 'false'
                        end
                    end, Devices).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return true if the given Id is the Id of at least one of the list
%% of JSON objects.
%%
%% @end
%%--------------------------------------------------------------------
-spec exists(ne_binary(), kz_json:objects()) -> boolean().
exists(_, []) -> 'false';
exists(Id, [JObj|JObjs]) ->
    case kz_json:get_ne_binary_value(<<"id">>, JObj) of
        Id -> 'true';
        _ -> exists(Id, JObjs)
    end.
