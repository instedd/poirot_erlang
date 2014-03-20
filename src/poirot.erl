-module(poirot).
-compile([{parse_transform, lager_transform}]).

-export([current/0, current_id/0, set_current/1]).
-export([new/2, new/3, new_inside/3, new_inside/4, inside/2, new_activity/1, new_activity/2, activity/1]).
-export([push/1, pop/0]).
-export([set_description/1, add_meta/1, clear_meta/0]).
-export([proxied_log/2]).

-include("poirot.hrl").

% Process dictionary key to keep the current activity id
-define(KEY, '__poirot_activity').

% Get the activity in the current process
current() ->
  get(?KEY).
current_id() ->
  (current())#activity.id.

% Encapsulate the execution of Fun() in a new activity. Previous activity (if
% any) is popped back when Fun() finishes.
new(Description, Fun) ->
  Activity = new_activity(Description),
  wrap(Activity, Fun).
new(Description, async, Fun) ->
  Activity = new_async_activity(Description),
  wrap(Activity, Fun);
new(Description, Options, Fun) ->
  Activity = case proplists:get_value(async, Options, false) of
    true -> new_async_activity(Description);
    _ -> new_activity(Description)
  end,
  Metadata = proplists:get_value(metadata, Options, []),
  wrap(Activity#activity{metadata = Metadata}, Options, Fun).

new_inside(Activity = #activity{}, Description, Fun) ->
  NewActivity = new_activity(Description),
  saving(Activity, fun() -> wrap(NewActivity, Fun) end);
new_inside(ActivityId, Description, Fun) ->
  new_inside(activity(ActivityId), Description, Fun).

new_inside(Activity = #activity{}, Description, async, Fun) ->
  NewActivity = new_async_activity(Description),
  saving(Activity, fun() -> wrap(NewActivity, Fun) end);
new_inside(ActivityId, Description, async, Fun) ->
  new_inside(activity(ActivityId), Description, async, Fun).

% Spread activity id to current process and execute Fun() inside it. Activity
% state is restored after execution finishes.
inside(Activity = #activity{}, Fun) ->
  saving(Activity, fun() -> wrap_fun_call(Fun) end);
inside(ActivityId, Fun) ->
  inside(activity(ActivityId), Fun).

wrap(Activity, Fun) -> wrap(Activity, [], Fun).
wrap(Activity, Options, Fun) ->
  push(Activity, Options),
  try
    wrap_fun_call(Fun)
  after
    pop(Options)
  end.

saving(Activity, Fun) ->
  Save = current(),
  set_current(Activity),
  try
    Fun()
  after
    set_current(Save)
  end.

wrap_fun_call(Fun) ->
  try
    Fun()
  catch
    error:Error ->
      lager:critical("A critical error occurred: ~p", [{error, Error, erlang:get_stacktrace()}]),
      erlang:error(Error);
    exit:Exit ->
      lager:critical("Process exit: ~p", [{exit, Exit, erlang:get_stacktrace()}]),
      erlang:exit(Exit);
    throw:Exception ->
      lager:error("An error occurred: ~p", [{throw, Exception, erlang:get_stacktrace()}]),
      throw(Exception)
  end.

push(Activity) -> push(Activity, []).
push(Activity = #activity{metadata = Meta}, Options) ->
  Current = current(),
  CurrentMeta = case Current of
    undefined -> [];
    #activity{metadata = M} -> M
  end,
  LinkedMeta = case proplists:get_value(inherit_meta, Options, false) of
    true -> merge_meta(Meta, CurrentMeta);
    _ -> Meta
  end,
  LinkedActivity = Activity#activity{parent = Current, metadata = LinkedMeta},
  set_current(LinkedActivity),
  poirot_client_srv:begin_activity(LinkedActivity),
  LinkedActivity.

pop() -> pop([]).
pop(Options) ->
  Activity = current(),
  Parent = Activity#activity.parent,
  case proplists:get_value(finalize, Options, true) of
    true -> poirot_client_srv:end_activity(Activity);
    _ -> ok
  end,
  set_current(Parent).

set_description(Description) when is_binary(Description) ->
  case current() of
    undefined -> undefined;
    Activity = #activity{} ->
      set_current(Activity#activity{description = Description})
  end;
set_description(Description) ->
  set_description(list_to_binary(Description)).

add_meta(Metadata) ->
  case current() of
    undefined -> undefined;
    Activity = #activity{metadata = CurrentMeta} ->
      set_current(Activity#activity{metadata = merge_meta(Metadata, CurrentMeta)})
  end.

clear_meta() ->
  case current() of
    undefined -> undefined;
    Activity = #activity{} ->
      set_current(Activity#activity{metadata = []})
  end.

merge_meta(NewMeta, OldMeta) ->
  NewMetaDict = orddict:from_list(NewMeta),
  orddict:merge(fun(_, V, _) -> V end, NewMetaDict, OldMeta).

%%
%% @doc Adds a single log entry as if it was sent by another source.
%% This is useful for cases in which the source of the events is not
%% able to connect directly with the poirot receiver.
%%
%% LogEntry is a proplist with the following keys:
%%
%%  level
%%  message
%%  timestamp
%%
%%  [optional] pid
%%  [optional] tags
%%  [optional] fields  (additional metadata)
%%
proxied_log(Source, LogEntry) ->
  poirot_client_srv:proxied_log(Source, LogEntry).

%%
%% Private implementation
%%

new_id() ->
  uuid:to_string(uuid:v4()).

new_activity(Description) when is_list(Description) ->
  new_activity(list_to_binary(Description));
new_activity(Description) ->
  #activity{id = new_id(), description = Description}.

new_activity(Format, Args) ->
  new_activity(io_lib:format(Format, Args)).

new_async_activity(Description) when is_list(Description) ->
  new_async_activity(list_to_binary(Description));
new_async_activity(Description) ->
  #activity{id = new_id(), async = true, description = Description}.

activity(Id) when is_binary(Id) ->
  activity(binary_to_list(Id));
activity(Id) ->
  #activity{id = Id}.

short_id(undefined) ->
  undefined;
short_id(<<>>) ->
  undefined;
short_id([]) ->
  undefined;
short_id(Id) ->
  <<Short:32, _:96>> = uuid:to_binary(Id),
  lists:flatten(io_lib:format("~8.16.0b", [Short])).

%@ private
set_current(Activity) ->
  Id = case Activity of
    undefined -> undefined;
    #activity{id = ActivityId} -> ActivityId
  end,
  put(?KEY, Activity),
  LagerMd = proplists:delete(short_activity, proplists:delete(activity, lager:md())),
  lager:md([{activity,Id},{short_activity,short_id(Id)}|LagerMd]),
  Activity.

