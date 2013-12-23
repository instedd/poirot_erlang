-module(lager_zeromq_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("lager/include/lager.hrl").

-record(state, {level, formatter, format_config, context, socket}).

%% @private
init(Config) when is_list(Config) ->
  Level = proplists:get_value(level, Config, info),
  Formatter = proplists:get_value(formatter, Config, lager_poirot_formatter),
  FormatterConfig = proplists:get_value(formatter_config, Config, []),
  Hwm = proplists:get_value(send_hwm, Config, 50),
  Url = proplists:get_value(url, Config, "tcp://localhost:2120"),

  Levels = lager_util:config_to_mask(Level),

  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, pub),
  ok = erlzmq:setsockopt(Socket, sndhwm, Hwm),
  ok = erlzmq:connect(Socket, Url),

  {ok, #state{level = Levels, formatter = Formatter, format_config = FormatterConfig, socket = Socket, context = Context}}.

%% @private
handle_call(get_loglevel, #state{level = Level} = State) ->
  {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
  try lager_util:config_to_mask(Level) of
    Levels ->
      {ok, ok, State#state{level = Levels}}
  catch
    _:_ ->
      {ok, {error, bad_log_level}, State}
  end;
handle_call(_Request, State) ->
  {ok, ok, State}.

%% @private
handle_event({log, Message}, #state{level = L, formatter = Formatter, format_config = FormatConfig, socket = Socket} = State) ->
  case lager_util:is_loggable(Message, L, ?MODULE) of
    true ->
      FormattedMessage = Formatter:format(Message, FormatConfig),
      ok = erlzmq:send(Socket, iolist_to_binary(FormattedMessage)),
      {ok, State};
    false ->
      {ok, State}
  end;
handle_event(_Event, State) ->
  {ok, State}.

%% @private
handle_info(_Info, State) ->
  {ok, State}.

%% @private
terminate(_Reason, #state{socket = Socket, context = Context}) ->
  ok = erlzmq:close(Socket),
  ok = erlzmq:term(Context),
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

