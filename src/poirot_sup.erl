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
  {SenderNeedsIndexer, SenderConfig} = sender_config(application:get_env(poirot, sender, undefined)),
  ReceiverConfig = receiver_config(application:get_env(poirot, receiver, undefined)),

  [
    ?CHILD(poirot_sender, worker, [Source | SenderConfig]) |
    case ReceiverConfig of
      undefined -> indexer_config(SenderNeedsIndexer);
      _ -> [
        ?CHILD(poirot_zmq_receiver, worker, [ReceiverConfig]) | indexer_config(true)
      ]
    end
  ].

sender_config(zmq) -> sender_config({zmq, []});
sender_config({zmq, Options}) -> {false, [poirot_zmq_sender, Options]};
sender_config(inproc) -> {true, [poirot_inproc_sender, []]};
sender_config(local) -> {true, [poirot_local_sender, []]};
sender_config(undefined) -> sender_config(inproc);
sender_config(Config) -> exit({unsuported_poirot_sender, Config}).

receiver_config(Options) when is_list(Options) ->
  case proplists:get_value(enabled, Options, true) of
    true -> Options;
    _ -> undefined
  end;
receiver_config(_) -> undefined.

indexer_config(false) -> [];
indexer_config(true) ->
  IndexerConfig = application:get_env(poirot, index, []),
  [?CHILD(poirot_index, worker, [IndexerConfig])].
