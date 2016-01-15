-module(poirot_zmq_sender).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {zmq_context, zmq_socket}).

-define(DEFAULT_URL, "tcp://localhost:2120").
-define(DEFAULT_HWM, 50).

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

init(Options) ->
  process_flag(trap_exit, true),
  poirot_event:subscribe(fun(Event) -> gen_server:call(?MODULE, {event, Event}) end),

  Url = proplists:get_value(url, Options, ?DEFAULT_URL),
  Hwm = proplists:get_value(hwm, Options, ?DEFAULT_HWM),

  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, pub),
  ok = erlzmq:setsockopt(Socket, sndhwm, Hwm),
  ok = erlzmq:connect(Socket, Url),

  {ok, #state{zmq_context = Context, zmq_socket = Socket}}.

handle_call({event, Event}, _From, State = #state{zmq_socket = Socket}) ->
  Data = poirot_event:dump(Event),
  ok = erlzmq:send(Socket, iolist_to_binary(Data)),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{zmq_context = Context, zmq_socket = Socket}) ->
  erlzmq:close(Socket, 1000),
  erlzmq:term(Context, 1000),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
