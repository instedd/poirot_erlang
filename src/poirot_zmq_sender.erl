-module(poirot_zmq_sender).
-behaviour(poirot_sender).
-export([init/1, send_event/2, terminate/1]).

-record(state, {zmq_context, zmq_socket}).

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, pub),
  ok = erlzmq:setsockopt(Socket, sndhwm, 50),
  ok = erlzmq:connect(Socket, "tcp://localhost:2120"),

  {ok, #state{zmq_context = Context, zmq_socket = Socket}}.

send_event(Event, #state{zmq_socket = Socket}) ->
  Data = poirot_event:dump(Event),
  ok = erlzmq:send(Socket, iolist_to_binary(Data)).

terminate(#state{zmq_context = Context, zmq_socket = Socket}) ->
  erlzmq:close(Socket, 1000),
  erlzmq:term(Context, 1000),
  ok.
