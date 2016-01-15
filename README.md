Poirot Erlang
=============

This is the Erlang sender API and client, and receiver module for Poirot.

## Configuration options

Configuration has the following main sections:

### Source

Binary string with the name of the source of the log entries to be reported. Use only if this application will send log entries.

### Modules

Different modules can be enabled to compose Poirot functionality. If just the name of the module is specified, it will use the default configuration.

#### zmq_sender

Deliver Poirot events through ZMQ.
Parameters:
  * `url`: The target address. Defaults to `tcp://localhost:2120`.
  * `hwm`: ZQM high watermark (max number of messages before dropping). Defaults to `50`.

#### zmq_receiver

Receives Poirot events through ZMQ.
Parameters:
  * `bind`: The address to bind the listening port. Defaults to `tcp://*:2120`.

#### indexer

Indexes the events in ElasticSearch. The events are inserted in separate indexes for each day. The name of each index has the form: `{prefix}-{year}.{month}.{day}`.
Parameters:
  * `prefix`: Index name prefix. Defaults to `poirot`.
  * `elasticsearch_url`: Base URL of the ElasticSearch server. Defaults to `http://localhost:9200/`.


## Scenarios

### Just index to Elasticsearch

This option is recommended for small or standalone Erlang applications.

- Add the following line to `rebar.config`

```erlang
{poirot, ".*", "https://github.com/instedd/poirot_erlang.git", "master"}
```

- Add to your `myapp.config` configuration file

```erlang
{poirot, [
  {source, <<"myapp">>},
  {modules, [
    indexer
  ]}
]}
```

Or if you need to customize the indexer parameters:

```erlang
{poirot, [
  {source, <<"myapp">>},
  {modules, [
    {indexer, [
      {elasticsearch_url, "http://localhost:9200/"},
      {prefix, <<"poirot">>}
    }
  ]}
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
  {modules, [
    {zmq_sender, [{url, "tcp://localhost:2120"}]}
  ]}
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
  {modules, [
    {zmq_receiver, [
      {bind, "tcp://*:2120"}
    ]},
    {indexer, [
      {elasticsearch_url, "http://localhost:9200/"},
      {prefix, <<"poirot">>}
    ]}
  ]}
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

