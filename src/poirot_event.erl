-module(poirot_event).
-export([parse/1, dump/1, timestamp_date/1]).
-export([start_link/0, add_handler/2, add_sup_handler/2, subscribe/1, broadcast/1]).

-include("poirot.hrl").

parse(Json) ->
  {struct, EventArgs} = mochijson2:decode(Json),
  Type = parse_type(proplists:get_value(<<"type">>, EventArgs)),
  Id = proplists:get_value(<<"id">>, EventArgs),
  {struct, Body} = proplists:get_value(<<"body">>, EventArgs),

  #event{type = Type, id = Id, body = Body}.

parse_type(<<"begin_activity">>) -> begin_activity;
parse_type(<<"end_activity">>) -> end_activity;
parse_type(<<"logentry">>) -> logentry.

dump(#event{id = Id, type = Type, body = Body}) ->
  mochijson2:encode({struct, [
    {id, poirot_sender:make_printable(Id)},
    {type, Type},
    {body, {struct, Body}}
  ]}).

timestamp_date(#event{body = Body}) ->
  case proplists:get_value(<<"@timestamp">>, Body) of
    <<Y:4/binary, $-, M:2/binary, $-, D:2/binary, _/binary>> -> {Y, M, D};
    _ ->
      {{Y, M, D}, _} = calendar:universal_time(),
      {int_to_bin(Y), int_to_bin(M), int_to_bin(D)}
  end.

int_to_bin(X) -> list_to_binary(integer_to_list(X)).

start_link() ->
  gen_event:start_link({local, poirot_event_bus}).

add_handler(Handler, Args) ->
  gen_event:add_handler(poirot_event_bus, Handler, Args).

add_sup_handler(Handler, Args) ->
  gen_event:add_sup_handler(poirot_event_bus, Handler, Args).

% Subscribe to the events with a callback function.
% This avoids the requirement of registering a new module to receive notifications.
subscribe(Fun) ->
  gen_event:add_sup_handler(poirot_event_bus, poirot_event_handler, Fun).

broadcast(Event) ->
  gen_event:notify(poirot_event_bus, Event).
