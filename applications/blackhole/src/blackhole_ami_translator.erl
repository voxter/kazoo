-module(blackhole_ami_translator).

-export([init/0]).

-define(HANDLERS, [
    {<<"call_event">>, {blackhole_ami_call}},
    {<<"queue">>, {blackhole_ami_acdc}}
]).

init() ->
    lists:foreach(fun({_HandlerEventType, {Module}}) ->
        Module:init_bindings() end, ?HANDLERS).