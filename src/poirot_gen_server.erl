-module(poirot_gen_server).

-export([start_link/3, start_link/4, start/3, start/4, call/2, call/3,
    multi_call/2, multi_call/3, multi_call/4, cast/2, abcast/2, abcast/3,
    reply/2, enter_loop/3, enter_loop/4, enter_loop/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([behaviour_info/1]).

-behaviour(gen_server).

-record(state, {module, data}).

behaviour_info(callbacks) ->
  [{init,1}, {handle_call,3}, {handle_cast,2}, {handle_info,2}, {terminate,2}, {code_change,3}];
behaviour_info(_Other) ->
  undefined.


%% Public interface

start(Module, Args, Options) ->
  gen_server:start(?MODULE, {Module, Args}, Options).
start(ServerName, Module, Args, Options) ->
  gen_server:start(ServerName, ?MODULE, {Module, Args}, Options).
start_link(Module, Args, Options) ->
  gen_server:start_link(?MODULE, {Module, Args}, Options).
start_link(ServerName, Module, Args, Options) ->
  gen_server:start_link(ServerName, ?MODULE, {Module, Args}, Options).
call(ServerRef, Request) ->
  gen_server:call(ServerRef, {poirot:current(), Request}).
call(ServerRef, Request, Timeout) ->
  gen_server:call(ServerRef, {poirot:current(), Request}, Timeout).
multi_call(Name, Request) ->
  gen_server:multi_call(Name, Request).
multi_call(Nodes, Name, Request) ->
  gen_server:multi_call(Nodes, Name, Request).
multi_call(Nodes, Name, Request, Timeout) ->
  gen_server:multi_call(Nodes, Name, Request, Timeout).
cast(ServerRef, Request) ->
  gen_server:cast(ServerRef, {poirot:current(), Request}).
abcast(Name, Request) ->
  gen_server:abcast(Name, Request).
abcast(Nodes, Name, Request) ->
  gen_server:abcast(Nodes, Name, Request).
reply(Client, Reply) ->
  gen_server:reply(Client, Reply).
enter_loop(Module, Options, State) ->
  gen_server:enter_loop(Module, Options, State).
enter_loop(Module, Options, State, ServerName) ->
  gen_server:enter_loop(Module, Options, State, ServerName).
enter_loop(Module, Options, State, ServerName, Timeout) ->
  gen_server:enter_loop(Module, Options, State, ServerName, Timeout).

%% Behaviour implementation

init({Mod, Args}) ->
  case Mod:init(Args) of
    {ok, ModState} ->
      {ok, #state{module = Mod, data = ModState}};
    {ok, ModState, Timeout} ->
      {ok, #state{module = Mod, data = ModState}, Timeout};
    {stop, Reason} ->
      {stop, Reason}
  end.

%% @private
handle_call({Activity, Message}, From, State = #state{module = Mod, data = ModState}) ->
  poirot:inside(Activity, fun() ->
        case Mod:handle_call(Message, From, ModState) of
          {reply, Reply, NewModState} ->
            {reply, Reply, State#state{data = NewModState}};
          {reply, Reply, NewModState, Timeout} ->
            {reply, Reply, State#state{data = NewModState}, Timeout};
          {noreply, NewModState} ->
            {noreply, State#state{data = NewModState}};
          {noreply, NewModState, Timeout} ->
            {noreply, State#state{data = NewModState}, Timeout};
          {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#state{data = NewModState}};
          {stop, Reason, NewModState} ->
            {stop, Reason, State#state{data = NewModState}}
        end
    end).

%% @private
handle_cast({Activity, Message}, State = #state{module = Mod, data = ModState}) ->
  Description = io_lib:format("~w:cast(~p)", [Mod, Message]),
  poirot:new_inside(Activity, Description, async, fun() ->
        case Mod:handle_cast(Message, ModState) of
          {noreply, NewModState} ->
            {noreply, State#state{data = NewModState}};
          {noreply, NewModState, Timeout} ->
            {noreply, State#state{data = NewModState}, Timeout};
          {stop, Reason, NewModState} ->
            {stop, Reason, State#state{data = NewModState}}
        end
    end).

%% @private
handle_info(Info, State = #state{module = Mod, data = ModState}) ->
  case Mod:handle_info(Info, ModState) of
    {noreply, NewModState} ->
      {noreply, State#state{data = NewModState}};
    {noreply, NewModState, Timeout} ->
      {noreply, State#state{data = NewModState}, Timeout};
    {stop, Reason, NewModState} ->
      {stop, Reason, State#state{data = NewModState}}
  end.

%% @private
terminate(Reason, #state{module = Mod, data = ModState}) ->
  Mod:terminate(Reason, ModState).

%% @private
code_change(OldVsn, State = #state{module = Mod, data = ModState}, Extra) ->
  case Mod:code_change(OldVsn, ModState, Extra) of
    {ok, NewModState} ->
      {ok, State#state{data = NewModState}};
    {error, Reason} ->
      {error, Reason}
  end.

