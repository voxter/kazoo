%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, Voxter Communications
%%% @doc
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_pro_util).

-export([listen_on_other_leg/2
        ,send_hangup_req/1
        ,send_break_req/1
        ,check_license/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-ignore_xref([{'konami_pro_hack', 'listen_on_other_leg', 2}
             ,{'konami_pro_hack', 'send_hangup_req', 1}
             ,{'konami_pro_hack', 'send_break_req', 1}
             ]).

-spec listen_on_other_leg(any(), any()) -> any().
listen_on_other_leg(Arg1, Arg2) ->
    lager:debug("args: ~p", [[Arg1, Arg2]]),
    call_hack_module('listen_on_other_leg', [Arg1, Arg2]).

-spec send_hangup_req(any()) -> any().
send_hangup_req(Arg1) ->
    lager:debug("args: ~p", [[Arg1]]),
    call_hack_module('send_hangup_req', [Arg1]).

-spec send_break_req(any()) -> any().
send_break_req(Arg1) ->
    lager:debug("args: ~p", [[Arg1]]),
    call_hack_module('send_break_req', [Arg1]).

%%------------------------------------------------------------------------------
%% @doc TODO: do something better to suppress xref of 'konami_pro_hack'
%% detecting undefined_functions
%% @end
%%------------------------------------------------------------------------------
-spec call_hack_module(fun(([any()]) -> any()), [any()]) -> any().
call_hack_module(Fun, Args) ->
    Module = 'konami_pro_hack',
    erlang:apply(Module, Fun, Args).

-spec check_license(any()) -> boolean().
check_license(Module) ->
    lager:debug("bypassing license check for module ~s", [Module]),
    'true'.
