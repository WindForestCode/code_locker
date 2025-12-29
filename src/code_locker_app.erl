%%%-------------------------------------------------------------------
%% @doc code_locker public API
%% @end
%%%-------------------------------------------------------------------

-module(code_locker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    code_locker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
