-module(poirot_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

% Standalone start
start() ->
  ok = inets:start(),
  ok = application:start(poirot).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    poirot_sup:start_link().

stop(_State) ->
    ok.
