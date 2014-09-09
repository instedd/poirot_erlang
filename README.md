Poirot Erlang
=============

This is the Erlang sender API and client, and receiver module for Poirot.

It depends on ElasticSearch being running.


Usage with the embedded in-proc receiver
----------------------------------------

- Add the following line to `rebar.config`

```erlang
{poirot, ".*", "https://github.com/instedd/poirot_erlang.git", "master"}
```

- Add to your `myapp.config` configuration file

```erlang
{poirot, [
  {source, <<"myapp">>},
  {receiver, []}
]}
```

Usage with an external receiver
-------------------------------

- Add the following line to `rebar.config`

```erlang
{poirot, ".*", "https://github.com/instedd/poirot_erlang.git", "master"}
```

- Add to your `myapp.config` configuration file

```erlang
{poirot, [
  {source, <<"myapp">>},
  {sender, zmq},
  % optionaly, configure the receiver's URL
  % {sender, {zmq, [{url, "tcp://localhost:2120"}]}}
  {receiver, undefined}
]}
```


Usage as a standalone receiver
------------------------------

- Checkout a copy of this repository to a directory named `poirot`:

```bash
$ git clone https://github.com/instedd/poirot_erlang.git poirot
```

The name of the checkout directory is important, otherwise Poirot will not find
the necessary runtime files.

- Run the receiver with `make run`


Configure Lager backend to send log entries to Poirot
-----------------------------------------------------

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

