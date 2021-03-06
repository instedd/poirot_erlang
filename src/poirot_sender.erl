-module(poirot_sender).
-export([start_link/1, send_event/1, begin_activity/1, end_activity/1, logentry/1, make_printable/1, proxied_log/2]).
-include("poirot.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% poirot_sender behaviour definition:
-callback init(Args :: list(term())) -> {ok, State :: term()}.
-callback send_event(Event :: #event{}, State :: term()) -> term().
-callback terminate(State :: term()) -> term().

-record(state, {source}).

start_link(Source) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Source, []).

send_event(Event) ->
  gen_server:cast(?MODULE, {event, Event}).

begin_activity(Activity) ->
  gen_server:cast(?MODULE, {begin_activity, erlang:timestamp(), Activity}).

end_activity(Activity) ->
  gen_server:cast(?MODULE, {end_activity, erlang:timestamp(), Activity}).

logentry(LogEntry) ->
  gen_server:cast(?MODULE, {logentry, LogEntry}).

init(Source) ->
  {ok, #state{source = Source}}.

proxied_log(Source, LogEntry) ->
  Body = [
      {<<"@level">>, proplists:get_value(level, LogEntry)},
      {<<"@message">>, proplists:get_value(message, LogEntry)},
      {<<"@timestamp">>, format_unix_timestamp(proplists:get_value(timestamp, LogEntry))},
      {<<"@tags">>, []},
      {<<"@activity">>, null},
      {<<"@pid">>, proplists:get_value(pid, LogEntry, null)},
      {<<"@fields">>, proplists:get_value(metadata, LogEntry, null)}
    ],
  gen_server:cast(?MODULE, {logentry, Body, Source}).

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast({begin_activity, Timestamp, Activity}, State) ->
  Event = #event{
    type = begin_activity,
    id = Activity#activity.id,
    body = [
      {<<"@start">>, format_timestamp(Timestamp)}
      | activity_body(Activity)
    ]
  },
  send_event(Event, State),
  {noreply, State};

handle_cast({end_activity, Timestamp, Activity}, State) ->
  Event = #event{
    type = end_activity,
    id = Activity#activity.id,
    body = [
      {<<"@end">>, format_timestamp(Timestamp)}
      | activity_body(Activity)
    ]
  },
  send_event(Event, State),
  {noreply, State};

handle_cast({logentry, LogEntry}, State) ->
  Event = #event{
    type = logentry,
    body = logentry_body(LogEntry)
  },
  send_event(Event, State),
  {noreply, State};

handle_cast({logentry, Body, Source}, State) ->
  Event = #event{
    type = logentry,
    body = Body
  },
  send_event(Event, State#state{source = Source}),
  {noreply, State};

handle_cast({event, Event}, State) ->
  send_event(Event, State),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send_event(Event = #event{body = Body}, #state{source = Source}) ->
  Event2 = Event#event{body = [{<<"@source">>, Source} | Body]},
  poirot_event:broadcast(Event2).

activity_body(#activity{description = Description, metadata = Metadata, parent = Parent, async = Async}) ->
  ParentId = case Parent of
    undefined -> undefined;
    #activity{id = ParId} -> ParId
  end,
  [
    {<<"@timestamp">>, format_timestamp(erlang:timestamp())},
    {<<"@parent">>, make_printable(ParentId)},
    {<<"@description">>, Description},
    {<<"@fields">>, {struct, Metadata}},
    {<<"@tags">>, []},
    {<<"@async">>, Async},
    {<<"@pid">>, make_printable(self())}
  ].

logentry_body(#logentry{level = Level, timestamp = Timestamp, message = Message, tags = Tags, activity = Activity, pid = Pid, metadata = Metadata}) ->
  [
    {<<"@level">>, Level},
    {<<"@message">>, make_printable(Message)},
    {<<"@timestamp">>, format_timestamp(Timestamp)},
    {<<"@tags">>, Tags},
    {<<"@activity">>, make_printable(Activity)},
    {<<"@pid">>, make_printable(Pid)},
    {<<"@fields">>, Metadata}
  ].

format_timestamp({_, _, Microsecs} = Timestamp) ->
  {{Y, Mo, D}, {H, Mn, S}} = calendar:now_to_datetime(Timestamp),
  S1 = S + Microsecs / 1000000,
  format_datetime({{Y, Mo, D}, {H, Mn, S1}}).

format_unix_timestamp(Timestamp) when is_integer(Timestamp) ->
  BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  Seconds       = BaseDate + (Timestamp div 1000),
  {{Y,Mo,D}, {H,Mn,S}} = calendar:gregorian_seconds_to_datetime(Seconds),
  Milliseconds = Timestamp rem 1000,
  S1 = S + Milliseconds/1000,
  format_datetime({{Y,Mo,D}, {H,Mn,S1}}).

format_datetime({{Y, Mo, D}, {H, Mn, S}}) ->
  FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~9.6.0fZ",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
  list_to_binary(IsoStr).

make_printable(undefined) -> null;
make_printable(A) when is_atom(A) orelse is_binary(A) orelse is_number(A) -> A;
make_printable(P) when is_pid(P) -> iolist_to_binary(pid_to_list(P));
make_printable(L) when is_list(L) -> unicode:characters_to_binary(L);
make_printable(Other) -> unicode:characters_to_binary(io_lib:format("~p", [Other])).
