-module(poirot_bert).
-compile([{parse_transform, lager_transform}]).

-export([execute_call/4, execute_cast/4]).

execute_call(ActivityId, Module, Function, Args) ->
  poirot:spread_wrap(binary_to_list(ActivityId), fun() -> 
        lager:debug("Executing call ~w:~w(~p)", [Module, Function, Args]),
        forward_call(Module, Function, Args)
    end).

execute_cast(ActivityId, Module, Function, Args) ->
  poirot:spawn_wrap(binary_to_list(ActivityId), fun() -> 
        lager:debug("Executing cast ~w:~w(~p)", [Module, Function, Args]),
        forward_call(Module, Function, Args)
    end).

forward_call(Module, Function, Args) ->
  try
    erlang:apply(Module, Function, Args) 
  catch
    error:Error ->
      lager:warning("Error executing ~w:~w(~p): ~p", 
        [Module, Function, Args, {error, Error, erlang:get_backtrace()}]),
      erlang:error(Error);
    exit:Exit ->
      lager:warning("Exit error executing ~w:~w(~p): ~p", 
        [Module, Function, Args, {exit, Exit, erlang:get_backtrace()}]),
      erlang:exit(Exit);
    throw:Exception ->
      lager:warning("Exception thrown executing ~w:~w(~p): ~p", 
        [Module, Function, Args, {throw, Exception, erlang:get_backtrace()}]),
      throw(Exception)
  end.

