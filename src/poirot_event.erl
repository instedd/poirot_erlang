-module(poirot_event).
-export([parse/1, dump/1, timestamp_date/1]).

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
      {integer_to_binary(Y), integer_to_binary(M), integer_to_binary(D)}
  end.
