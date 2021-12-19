-module(data).
-export([init/1, get/1, post/2]).

init([]) ->
    BaseNode = node:start(0),
    register(node0, BaseNode),
    ok.

get(Key) ->
    dht:locate(Key, node0).

post(Key, Value) ->
    dht:store(Key, Value, node0).
