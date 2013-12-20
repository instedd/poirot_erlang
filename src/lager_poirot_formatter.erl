-module(lager_poirot_formatter).
-compile([{parse_transform, lager_transform}]).

-export([format/2, format/3]).

format(LagerMsg, Config, _Colors) ->
  format(LagerMsg, Config).

format(LagerMsg, _Config) ->
  try
    Timestamp = lager_msg:timestamp(LagerMsg),
    Severity = lager_msg:severity(LagerMsg),
    Metadata = lager_msg:metadata(LagerMsg),
    Message = lager_msg:message(LagerMsg),

    Pid = proplists:get_value(pid, Metadata, null),
    Activity = proplists:get_value(activity, Metadata, null),
    Type = proplists:get_value(type, Metadata, logentry),
    Tags = proplists:get_value(tags, Metadata, []),

    BaseHeaders = [
      {<<"@timestamp">>, iso8601:format(Timestamp)},
      {<<"@tags">>, Tags},
      {<<"@activity">>, make_printable(Activity)},
      {<<"@pid">>, make_printable(Pid)},
      {<<"@source">>, <<"cepheid-receiver">>},
      {<<"@fields">>, filtered_metadata(Metadata)}
    ],

    {ExtraHeaders, DocId}  = case Type of
      begin_activity ->
        ParentActivity = proplists:get_value(parent_activity, Metadata, null),
        {[
          {<<"_type">>, <<"activity">>},
          {<<"_id">>, make_printable(Activity)},
          {<<"@level">>, Severity},
          {<<"@parent">>, make_printable(ParentActivity)}
        ],
        Activity};
      end_activity ->
        MaxLevel = proplists:get_value(max_severity, Metadata, Severity),
        ParentActivity = proplists:get_value(parent_activity, Metadata, null),
        {[
          {<<"_type">>, <<"activity">>},
          {<<"_id">>, make_printable(Activity)},
          {<<"@level">>, MaxLevel},
          {<<"@parent">>, make_printable(ParentActivity)}
        ], Activity};
      logentry ->
        {[
          {<<"_type">>, <<"logentry">>},
          {<<"@level">>, Severity},
          {<<"@message">>, make_printable(Message)}
        ],
        null}
    end,

    Headers = BaseHeaders ++ ExtraHeaders,
    mochijson2:encode({struct, [
          {id, make_printable(DocId)},
          {type, Type},
          {body, {struct, Headers}}
        ]}) ++ "\n"
  catch
    Exc:Reason ->
      io:format("Error formatting lagger message: ~p, ~p, ~p~n", [Exc, Reason, erlang:get_stacktrace()])
  end.

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

