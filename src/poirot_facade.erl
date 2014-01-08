-module(poirot_facade).
-compile([{parse_transform, lager_transform}]).

-export([execute/4]).

execute(ActivityId, Module, Function, Args) ->
  poirot:spread_wrap(binary_to_list(ActivityId), fun() -> 
        try
          lager:debug("Executing ~w:~w(~p)", [Module, Function, Args]),
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
        end
    end).

