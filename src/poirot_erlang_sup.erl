-module(poirot_erlang_sup).

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
  PoirotClient = ?CHILD(poirot_client_srv, worker),
  PoirotIndex = ?CHILD(poirot_index, worker, [[]]),
  PoirotReceiver = ?CHILD(poirot_zmq_receiver, worker),
  {ok, { {one_for_one, 5, 10}, [PoirotClient, PoirotIndex, PoirotReceiver]} }.

