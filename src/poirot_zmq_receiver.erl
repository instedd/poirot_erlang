-module(poirot_zmq_receiver).
-export([start_link/0, start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {zmq_context, zmq_socket}).

-define(DEFAULT_BIND, "tcp://*:2120").

start_link() ->
  start_link([]).

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(Args) ->
  process_flag(trap_exit, true),
  Bind = proplists:get_value(bind, Args, ?DEFAULT_BIND),

  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, sub),
  ok = erlzmq:setsockopt(Socket, subscribe, ""),
  ok = erlzmq:bind(Socket, Bind),

  erlang:spawn_link(fun() -> receive_loop(Socket) end),

  {ok, #state{zmq_context = Context, zmq_socket = Socket}}.

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({event, Data}, State) ->
  Event = poirot_event:parse(Data),
  poirot_event:broadcast(Event),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{zmq_context = Context, zmq_socket = Socket}) ->
  erlzmq:close(Socket, 1000),
  erlzmq:term(Context, 1000),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

receive_loop(Socket) ->
  case erlzmq:recv(Socket) of
    {ok, Data} ->
      ?MODULE ! {event, Data},
      receive_loop(Socket);
    {error, Error} ->
      error(Error)
  end.
