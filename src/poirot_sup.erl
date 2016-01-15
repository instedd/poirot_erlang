-module(poirot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type), ?CHILD(I, Type, [])).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10}, config()} }.

config() ->
  Source = application:get_env(poirot, source, <<"unknown">>),
  Modules = application:get_env(poirot, modules, []),
  [
    ?CHILD(poirot_event, worker),
    ?CHILD(poirot_sender, worker, [Source])
    | module_configs(Modules)
  ].

module_configs([]) -> [];
module_configs([ConfigModule | T]) when is_atom(ConfigModule) ->
  module_configs([{ConfigModule, []} | T]);
module_configs([{ConfigModule, Config} | T]) ->
  case map_module(ConfigModule) of
    undefined ->
      error_logger:error_msg("Unknown Poirot module ~p~n", [ConfigModule]),
      module_configs(T);
    Module ->
      [?CHILD(Module, worker, [Config]) | module_configs(T)]
  end.

map_module(zmq_receiver) -> poirot_zmq_receiver;
map_module(zmq_sender) -> poirot_zmq_sender;
map_module(indexer) -> poirot_index;
map_module(_) -> undefined.
