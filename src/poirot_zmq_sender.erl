-module(poirot_zmq_sender).
-behaviour(poirot_sender).
-export([init/1, send_event/2, terminate/1]).

-record(state, {zmq_context, zmq_socket}).

-define(DEFAULT_URL, "tcp://localhost:2120").
-define(DEFAULT_HWM, 50).

init(Args) ->
  process_flag(trap_exit, true),

  Url = proplists:get_value(url, Args, ?DEFAULT_URL),
  Hwm = proplists:get_value(hwm, Args, ?DEFAULT_HWM),

  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, pub),
  ok = erlzmq:setsockopt(Socket, sndhwm, Hwm),
  ok = erlzmq:connect(Socket, Url),

  {ok, #state{zmq_context = Context, zmq_socket = Socket}}.

send_event(Event, #state{zmq_socket = Socket}) ->
  Data = poirot_event:dump(Event),
  ok = erlzmq:send(Socket, iolist_to_binary(Data)).

terminate(#state{zmq_context = Context, zmq_socket = Socket}) ->
  erlzmq:close(Socket, 1000),
  erlzmq:term(Context, 1000),
  ok.
