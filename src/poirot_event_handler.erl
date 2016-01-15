-module(poirot_event_handler).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init(Callback) ->
  {ok, Callback}.

handle_event(Event, Callback) ->
  Callback(Event),
  {ok, Callback}.

handle_call(_, State) ->
  {ok, {error, unknown_call}, State}.

handle_info(_, State) ->
  {ok, State}.

terminate(_Reason, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
