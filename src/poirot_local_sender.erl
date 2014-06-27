-module(poirot_local_sender).
-behaviour(poirot_sender).
-export([init/1, send_event/2, terminate/1]).
-include("poirot.hrl").

init(_) ->
  gen_event:start_link({local, poirot_local}),
  {ok, undefined}.

send_event(Event, _) ->
  gen_event:notify(poirot_local, Event),
  ok.

terminate(_) -> ok.
