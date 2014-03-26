-module(poirot_inproc_sender).
-behaviour(poirot_sender).
-export([init/1, send_event/2, terminate/1]).

init(_Args) -> {ok, undefined}.

send_event(Event, _) -> poirot_index:index_event(Event).

terminate(_) -> ok.
