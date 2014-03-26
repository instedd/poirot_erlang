-module(lager_poirot_backend).
-behaviour(gen_event).
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).
-include("poirot.hrl").
-include_lib("lager/include/lager.hrl").

-record(state, {level}).

%% @private
init(Config) when is_list(Config) ->
  Level = proplists:get_value(level, Config, info),
  Levels = lager_util:config_to_mask(Level),
  {ok, #state{level = Levels}}.

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
handle_event({log, Message}, State = #state{level = L}) ->
  case lager_util:is_loggable(Message, L, ?MODULE) of
    true ->
      Metadata = lager_msg:metadata(Message),
      LogEntry = #logentry{
        level = lager_msg:severity(Message),
        timestamp = lager_msg:timestamp(Message),
        message = lager_msg:message(Message),
        activity = poirot_sender:make_printable(proplists:get_value(activity, Metadata, null)),
        pid = poirot_sender:make_printable(proplists:get_value(pid, Metadata, null)),
        metadata = filtered_metadata(Metadata),
        tags = proplists:get_value(tags, Metadata, [])
      },
      poirot_sender:logentry(LogEntry),
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
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

filtered_metadata(Metadata) ->
  {struct, lists:foldr(fun add_metadata/2, [], Metadata)}.

add_metadata({activity, _}, Acc) ->
  Acc;
add_metadata({parent_activity, _}, Acc) ->
  Acc;
add_metadata({short_activity, _}, Acc) ->
  Acc;
add_metadata({pid, _}, Acc) ->
  Acc;
add_metadata({max_severity, _}, Acc) ->
  Acc;
add_metadata({type, _}, Acc) ->
  Acc;
add_metadata({Key, Value}, Acc) ->
  [{Key, poirot_sender:make_printable(Value)} | Acc].
