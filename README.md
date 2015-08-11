Poirot Erlang
=============

This is the Erlang sender API and client, and receiver module for Poirot.

It depends on ElasticSearch being running.

## Configuration options

Configuration has the following main sections:

#### Source

Binary string with the name of the source of the log entries to be reported. Use only if this application will send log entries.

#### Sender

How to send entries to the Poirot receiver. The possible values are:
* `inproc` to send direct erlang messages to an inproc receiver (see below)
* `local` to raise `poirot_local` events via `gen_event`; use this option to implement a custom receiver in your application
* `{zmq, [{url, "tcp://myserver:myport"}]}` to send messages to a remote receiver via zmq; if no `url` is set then `localhost:2120` is used as default.

#### Receiver

Set up the log entries Receiver to accept entries from one or more senders. Can be set to `{receiver, [{bind, "tcp://*:2120"}]}` to listen to zeromq messages in the selected interface; if the in-proc communication is to be used, then set to `{receiver, []}`.

#### Index

Configures the location of the ElasticSearch indices to be used by the Receiver to index the log entries. Will index to `localhost:9200` by default. Can be set as `{index, [{elasticsearch_url, "http://localhost:9200/"}, {prefix, <<"poirot">>}]}` to configure both the location of ElasticSearch and the prefix to be used for the Poirot indices. The indexer will be automatically set if the sender is set to `inproc`.

## Scenarios

### Usage with the embedded in-proc receiver

This option is recommended for small or standalone Erlang applications.

- Add the following line to `rebar.config`

```erlang
{poirot, ".*", "https://github.com/instedd/poirot_erlang.git", "master"}
```

- Add to your `myapp.config` configuration file

```erlang
{poirot, [
  {source, <<"myapp">>},
  {receiver, []},
  {index, [
    {elasticsearch_url, "http://localhost:9200/"},
    {prefix, <<"poirot">>}
  ]},
]}
```

### Usage with an external receiver

This option must be used together with a standalone receiver, and is recommended for large applications.

- Add the following line to `rebar.config`

```erlang
{poirot, ".*", "https://github.com/instedd/poirot_erlang.git", "master"}
```

- Add to your `myapp.config` configuration file

```erlang
{poirot, [
  {source, <<"myapp">>},
  {sender, {zmq, [{url, "tcp://localhost:2120"}]}},
  {receiver, undefined}
]}
```


### Usage as a standalone receiver

This option spawns a receiver on itself, without being embedded in a larger Erlang app.

- Checkout a copy of this repository to a directory named `poirot`:

```bash
$ git clone https://github.com/instedd/poirot_erlang.git poirot
```

The name of the checkout directory is important, otherwise Poirot will not find
the necessary runtime files.

- Set up the configuration file `poirot.config` with the following contents:

```erlang
{poirot, [
  {receiver, [
    {bind, "tcp://*:2120"}
  ]}},
  {index, [
    {elasticsearch_url, "http://localhost:9200/"},
    {prefix, <<"poirot">>}
  ]},
]}
```

- Run the receiver with `make run`


### Configure Lager backend to send log entries to Poirot

- Optionally, you can add to your `myapp.config` configuration file in the `lager` section:

```erlang
{lager, [
  {handlers, [
    % ... other lager backends ...
    {lager_poirot_backend, [{level, info}]}
  ]},
  % ... the rest of the lager configuration
]}
```

