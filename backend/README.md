backend
=====

An OTP application

Build
-----

    $ rebar3 compile

Start API
---------

    $ rebar3 shell -sname node0

Start other nodes
-----------------

Start an erlang shell and define a `sname` different than `node0`:

    $ erl -sname node2

In the erlang shell, start a new node and connect it by passing the `Peer` in the second argument (do not forget to replace `<LOCATION>` by the correct value).

```erlang
Id = key:generate().  % Change the Id for other nodes.
NewNode = node:start(Id, {node0, 'node0@<LOCATION>'}).
```

After having started the nodes and stored some data via the frontend (for example), you can send a message to the node to see its storage:

```erlang
NewNode ! displayStore.
```
