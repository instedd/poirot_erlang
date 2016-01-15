-module(poirot_index).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("poirot.hrl").
-record(state, {prefix, elasticsearch_url, events = [], event_count = 0}).

-define(MAX_QUEUED_ITEMS, 300).
-define(QUEUE_TIMEOUT, 3000).
-define(DEFAULT_PREFIX, <<"poirot">>).
-define(DEFAULT_ES_URL, "http://localhost:9200/").

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

index_event(Event) ->
  gen_server:cast(?MODULE, {event, Event}).

init(Options) ->
  poirot_event:subscribe(fun(Event) -> index_event(Event) end),

  Prefix = proplists:get_value(prefix, Options, ?DEFAULT_PREFIX),
  EsUrl = case os:getenv("ELASTICSEARCH_URL") of
    false ->
       case proplists:get_value(elasticsearch_url, Options) of
        undefined -> ?DEFAULT_ES_URL;
        EnvUrl -> EnvUrl
      end;
    OptionsUrl -> OptionsUrl
  end,

  {ok, Template} = file:read_file(filename:join(code:priv_dir(poirot), "elasticsearch-template.json")),
  MetadataTemplate = binary:replace(Template, <<"{{prefix}}">>, Prefix),
  {ok, {200, _}} = api_call(EsUrl, put, "_template/poirot_template", MetadataTemplate),
  {ok, #state{prefix = Prefix, elasticsearch_url = EsUrl}}.

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast({event, Event}, State = #state{events = Events, event_count = Count}) ->
  NewState = State#state{events = [Event | Events], event_count = Count + 1},
  if
    NewState#state.event_count >= ?MAX_QUEUED_ITEMS ->
      {noreply, flush(NewState)};
    true ->
      {noreply, NewState, ?QUEUE_TIMEOUT}
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  {noreply, flush(State)};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

flush(State = #state{events = Events}) ->
  bulk_send(State, Events),
  State#state{events = [], event_count = 0}.

bulk_send(State, Events) ->
  Bulk = iolist_to_binary(prepare_bulk_body(State#state.prefix, Events)),
  api_call(State#state.elasticsearch_url, post, "_bulk", Bulk).

prepare_bulk_body(Prefix, Events) -> prepare_bulk_body(Prefix, Events, []).
prepare_bulk_body(_, [], Bulk) -> Bulk;
prepare_bulk_body(Prefix, [Event | T], Bulk) ->
  {Y, M, D} = poirot_event:timestamp_date(Event),
  Index = <<Prefix/binary, $-, Y/binary, $., M/binary, $., D/binary>>,
  BulkCommand = case Event#event.type of
    begin_activity -> index_bulk_command(Index, <<"activity">>, Event#event.id, Event#event.body);
    end_activity -> index_update_command(Index, <<"activity">>, Event#event.id, Event#event.body);
    logentry -> index_bulk_command(Index, <<"logentry">>, undefined, Event#event.body)
  end,
  prepare_bulk_body(Prefix, T, [BulkCommand | Bulk]).

index_bulk_command(Index, Type, Id, Body) ->
  [
    mochijson2:encode({struct, [{<<"index">>, {struct, [
      {<<"_index">>, Index},
      {<<"_type">>, Type} |
      case Id of undefined -> []; _ -> [{<<"_id">>, Id}] end
    ]}}]}),
    <<"\r\n">>,
    mochijson2:encode({struct, Body}),
    <<"\r\n">>
  ].

index_update_command(Index, Type, Id, Body) ->
  [
    mochijson2:encode({struct, [{<<"update">>, {struct, [
      {<<"_index">>, Index},
      {<<"_type">>, Type},
      {<<"_id">>, Id}
    ]}}]}),
    <<"\r\n">>,
    mochijson2:encode({struct, [{<<"doc">>, {struct, Body}}]}),
    <<"\r\n">>
  ].

api_call(BaseUrl, Method, Path, Body) ->
  httpc:request(Method, {BaseUrl ++ Path, [], "", Body}, [], [{body_format, binary}, {full_result, false}]).
