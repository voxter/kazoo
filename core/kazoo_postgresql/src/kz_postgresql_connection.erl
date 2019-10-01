%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, Voxter Communication Inc
%%% @doc Postgresql connection functions
%%% @end
%%% @author Ben Bradford <bsc.benbradford@gmail.com>
%%%-----------------------------------------------------------------------------
-module(kz_postgresql_connection).
-include("kz_postgresql.hrl").

-export([new_connection/1
        ]).

%%------------------------------------------------------------------------------
%% @doc Open a new connection with a PostgreSQL DB
%% @end
%%------------------------------------------------------------------------------
-spec new_connection(postgresql_connection() | map()) -> {'ok', kz_postgresql:connection_pool()} | {'error', term()}.
new_connection(#{}=Map) ->
    lager:debug("opening new postgresql connection to db with map: ~p", [Map]),
    %% Extract the conection info from the supplied map structure and connect to the DB
    connect(maps:fold(fun connection_parse/3, #kz_postgresql_connection{}, Map)).

%%------------------------------------------------------------------------------
%% @doc Extract the conection info from the supplied map structure
%% @end
%%------------------------------------------------------------------------------
-spec connection_parse(atom(), any(), postgresql_connection()) -> postgresql_connection().
connection_parse('settings', Map, Conn)
  when is_map(Map) ->
    maps:fold(fun connection_parse/3, Conn, Map);
connection_parse('credentials', #{'username' := Username, 'password' := Password}, Conn) ->
    Conn#kz_postgresql_connection{'username'=binary_to_list(Username), 'password'=binary_to_list(Password)};
connection_parse('ip', V, Conn) ->
    Conn#kz_postgresql_connection{'host'=binary_to_list(V)};
connection_parse('host', V, Conn) ->
    Conn#kz_postgresql_connection{'host'=binary_to_list(V)};
connection_parse('port', V, Conn) ->
    Conn#kz_postgresql_connection{'port'=kz_term:to_integer(V)};
connection_parse('timeout', V, Conn) ->
    Conn#kz_postgresql_connection{'timeout'=kz_term:to_integer(V)};
connection_parse('connection_pool_size', V, Conn) ->
    Conn#kz_postgresql_connection{'conn_pool_size'=kz_term:to_integer(V)};
connection_parse('connection_pool_max_overflow', V, Conn) ->
    Conn#kz_postgresql_connection{'conn_pool_max_overflow'=kz_term:to_integer(V)};
connection_parse('username', V, Conn) ->
    Conn#kz_postgresql_connection{'username'=binary_to_list(V)};
connection_parse('password', V, Conn) ->
    Conn#kz_postgresql_connection{'password'=binary_to_list(V)};
connection_parse('database', V, Conn) ->
    Conn#kz_postgresql_connection{'database'=binary_to_list(V)};
connection_parse(_, _V, Conn) ->
    Conn.

%%------------------------------------------------------------------------------
%% @doc Connect to the postgresql DB
%% @end
%%------------------------------------------------------------------------------
-spec connect(postgresql_connection()) -> {'ok', kz_postgresql:connection_pool()} | kz_data:data_error().
connect(#kz_postgresql_connection{id=Id
                                 ,host=Host
                                 ,port=Port
                                 ,username=User
                                 ,password=Pass
                                 ,database=Database
                                 ,timeout=Timeout
                                 ,conn_pool_size=ConnPoolSize
                                 ,conn_pool_max_overflow=ConnPoolMaxOverflow
                                 }) ->
    IdBinary = list_to_binary(integer_to_list(Id)),
    ConnPoolName = binary_to_atom(<<"postgresql_", IdBinary/binary, "_pool">>, 'latin1'),
    lager:debug("attempting new postgresql connection to host ~p:~p, Connection pool name: ~p", [Host, Port, ConnPoolName]),
    application:ensure_all_started(pgapp),
    {'ok', _Pid} = pgapp:connect(ConnPoolName,[{'size', ConnPoolSize}
                                              ,{'max_overflow', ConnPoolMaxOverflow}
                                              ,{'host', Host}
                                              ,{'port', Port}
                                              ,{'database', Database}
                                              ,{'username', User}
                                              ,{'password', Pass}
                                              ,{'timeout', Timeout}
                                              ]),
    %% Verify the connection pool is connected to the PG DB
    case kazoo_postgresql:server_info(ConnPoolName) of
        {'ok', _} ->
            lager:debug("postgresql connection successful, connection pool: ~p", [ConnPoolName]),
            {'ok', ConnPoolName};
        {'error', _} = Error ->
            lager:error("postgresql failed to connect to ~p, connection error: ~p", [ConnPoolName, Error]),
            %% Kill the child PG connection pool as its not connected successfuly.
            supervisor:terminate_child('pgapp_sup', ConnPoolName),
            Error
    end.
