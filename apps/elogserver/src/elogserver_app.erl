-module(elogserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start(bigwig),
    start(lager),
    elogserver_sup:start_link().

stop(_State) ->
    ok.

start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) ->
    ok;
start_ok(_App, {error, {already_started, _App}}) ->
    ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(_App, {error, Reason}) ->
    erlang:error({app_start_failed, Reason}).
