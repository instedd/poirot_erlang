-module(poirot).
-compile([{parse_transform, lager_transform}]).

-export([current/0]).
-export([start/1, stop/0, stop/1, suspend/0, suspend/1, resume/1]).
-export([wrap/2, spread_wrap/2]).

% Process dictionary key to keep the current activity id
-define(KEY, activity_id).

% Get the activity in the current process
current() ->
  get(?KEY).

% Encapsulate the execution of Fun() in a new activity. Previous activity (if
% any) is popped back when Fun() finishes.
wrap(Description, Fun) ->
  Save = start(Description),
  try
    Fun()
  after
    stop(Save)
  end.

% Spread activity id to current process and execute Fun() inside it. Activity
% state is restored after execution finishes.
spread_wrap(Id, Fun) ->
  Save = set_current(Id),
  try
    Fun()
  after
    set_current(Save)
  end.

% Start a new activity and transfer control to it. Returns the id of the
% previous activity.
start(Description) ->
  NewId = new_id(),
  Save = set_current(NewId),
  lager:log(info, [
      {type, begin_activity}, 
      {description, Description}, 
      {pid, self()}, 
      {activity, NewId},
      {short_activity, short_id(NewId)},
      {parent_activity, Save}
    ], activity_started),
  Save.

% Stop the current activity.
stop() ->
  stop(undefined).

% Stop the current activity and transfer back control to another activity.
stop(RestoreId) ->
  Save = set_current(RestoreId),
  lager:log(info, [
      {type, end_activity}, 
      {pid, self()}, 
      {activity, Save},
      {short_activity, short_id(Save)},
      {parent_activity, RestoreId}
    ], activity_stopped),
  Save.

% Suspends the current activity and returns the activity id.
suspend() ->
  set_current(undefined).  

% Suspends the current activity and sets current to the given. Returns the
% suspended activity id.
suspend(RestoreId) ->
  set_current(RestoreId).

% Resumes an activity and returns the previous active activity.
resume(Id) ->
  set_current(Id).


%%
%% Private implementation
%%

new_id() ->
  uuid:to_string(uuid:v4()).

short_id(undefined) ->
  undefined;
short_id(Id) ->
  <<Short:32, _:96>> = uuid:to_binary(Id),
  lists:flatten(io_lib:format("~8.16.0b", [Short])).

set_current(Id) ->
  OldId = put(?KEY, Id),
  LagerMd = proplists:delete(short_activity, proplists:delete(activity, lager:md())),
  lager:md([{activity,Id},{short_activity,short_id(Id)}|LagerMd]),
  OldId.

