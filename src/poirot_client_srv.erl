-module(poirot_client_srv).

-export([start_link/0]).
-export([begin_activity/1, end_activity/1, lager_entry/1, proxied_log/2, set_source/1, get_source/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("poirot.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-record(state, {context, socket, source}).

-define(DEFAULT_HWM, 50).
-define(DEFAULT_URL, "tcp://localhost:2120").
-define(DEFAULT_SOURCE, <<"unknown">>).


start_link() ->
  Url = application:get_env(poirot_erlang, url, ?DEFAULT_URL),
  Hwm = application:get_env(poirot_erlang, hwm, ?DEFAULT_HWM),
  Source = application:get_env(poirot_erlang, source, ?DEFAULT_SOURCE),
  Options = [{source, Source}, {url, Url}, {hwm, Hwm}],
  gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

get_source() ->
  gen_server:call(?SERVER, {get_source}).

set_source(Source) ->
  gen_server:call(?SERVER, {set_source, Source}).

begin_activity(Activity = #activity{id = Id}) ->
  Body = activity_body(Activity),
  Body1 = [
    {<<"@start">>, format_timestamp(erlang:now())}
    | Body],
  gen_server:cast(?SERVER, {write, Id, <<"begin_activity">>, Body1}).

end_activity(Activity = #activity{id = Id}) ->
  Body = activity_body(Activity),
  Body1 = [
    {<<"@end">>, format_timestamp(erlang:now())}
    | Body],
  gen_server:cast(?SERVER, {write, Id, <<"end_activity">>, Body1}).

activity_body(#activity{id = Id, description = Description, metadata = Metadata, parent = Parent, async = Async}) ->
  ParentId = case Parent of
    undefined -> undefined;
    #activity{id = ParId} -> ParId
  end,
  [
    {<<"_type">>, <<"activity">>},
    {<<"_id">>, make_printable(Id)},
    {<<"@timestamp">>, format_timestamp(erlang:now())},
    {<<"@parent">>, make_printable(ParentId)},
    {<<"@description">>, Description},
    {<<"@fields">>, {struct, Metadata}},
    {<<"@tags">>, []},
    {<<"@async">>, Async},
    {<<"@pid">>, make_printable(self())}
  ].


lager_entry(LagerMsg) ->
  Timestamp = lager_msg:timestamp(LagerMsg),
  Severity = lager_msg:severity(LagerMsg),
  Metadata = lager_msg:metadata(LagerMsg),
  Message = lager_msg:message(LagerMsg),

  Pid = proplists:get_value(pid, Metadata, null),
  Activity = proplists:get_value(activity, Metadata, null),
  Tags = proplists:get_value(tags, Metadata, []),

  Body = [
    {<<"_type">>, <<"logentry">>},
    {<<"@level">>, Severity},
    {<<"@message">>, make_printable(Message)},
    {<<"@timestamp">>, format_timestamp(Timestamp)},
    {<<"@tags">>, Tags},
    {<<"@activity">>, make_printable(Activity)},
    {<<"@pid">>, make_printable(Pid)},
    {<<"@fields">>, filtered_metadata(Metadata)}
  ],

  gen_server:cast(?SERVER, {write, null, <<"logentry">>, Body}).


proxied_log(Source, LogEntry) ->
  Body = [
      {<<"_type">>, <<"logentry">>},
      {<<"@level">>, proplists:get_value(level, LogEntry)},
      {<<"@message">>, proplists:get_value(message, LogEntry)},
      {<<"@timestamp">>, format_unix_timestamp(proplists:get_value(timestamp, LogEntry))},
      {<<"@tags">>, proplists:get_value(level, LogEntry, [])},
      {<<"@activity">>, null},
      {<<"@pid">>, proplists:get_value(pid, LogEntry, null)},
      {<<"@fields">>, proplists:get_value(metadata, LogEntry, null)}
    ],
  gen_server:cast(?SERVER, {write, null, <<"logentry">>, Body, Source}).

format_timestamp({_,_,Microsecs} = Timestamp) ->
  {{Y,Mo,D}, {H,Mn,S}} = calendar:now_to_datetime(Timestamp),
  S1 = S + Microsecs/1000000,
  format_datetime({{Y,Mo,D}, {H,Mn,S1}}).

format_unix_timestamp(Timestamp) when is_integer(Timestamp) ->
  BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  Seconds       = BaseDate + (Timestamp div 1000),
  {{Y,Mo,D}, {H,Mn,S}} = calendar:gregorian_seconds_to_datetime(Seconds),
  Milliseconds = Timestamp rem 1000,
  S1 = S + Milliseconds/1000,
  format_datetime({{Y,Mo,D}, {H,Mn,S1}}).

format_datetime({{Y,Mo,D}, {H,Mn,S}}) ->
  FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~9.6.0fZ",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
  list_to_binary(IsoStr).

make_printable(undefined) -> null;
make_printable(A) when is_atom(A) orelse is_binary(A) orelse is_number(A) -> A;
make_printable(P) when is_pid(P) -> iolist_to_binary(pid_to_list(P));
make_printable(L) when is_list(L) -> iolist_to_binary(L);
make_printable(Other) -> iolist_to_binary(io_lib:format("~p",[Other])).

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
  [{Key, make_printable(Value)} | Acc].


%% @private
init(Config) when is_list(Config) ->
  Source = proplists:get_value(source, Config, ?DEFAULT_SOURCE),
  Hwm = proplists:get_value(send_hwm, Config, ?DEFAULT_HWM),
  Url = proplists:get_value(url, Config, ?DEFAULT_URL),

  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, pub),
  ok = erlzmq:setsockopt(Socket, sndhwm, Hwm),
  ok = erlzmq:connect(Socket, Url),

  {ok, #state{socket = Socket, context = Context, source = Source}}.

%% @private
handle_call({get_source}, _From, State = #state{source = Source}) ->
  {reply, Source, State};
handle_call({set_source, Source}, _From, State) ->
  {reply, ok, State#state{source = Source}}.

%% @private
handle_cast({write, DocId, Type, Body}, State = #state{source = Source}) ->
  write(DocId, Type, Body, Source, State);

handle_cast({write, DocId, Type, Body, Source}, State) ->
  write(DocId, Type, Body, Source, State).

write(DocId, Type, Body, Source, State = #state{socket = Socket}) ->
  Body1 = [{<<"@source">>, Source} | Body],
  Data = mochijson2:encode({struct, [
        {id, make_printable(DocId)},
        {type, Type},
        {body, {struct, Body1}}
      ]}) ++ "\n",
  ok = erlzmq:send(Socket, iolist_to_binary(Data)),
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, #state{context = Context, socket = Socket}) ->
  ok = erlzmq:close(Socket),
  ok = erlzmq:term(Context),
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

