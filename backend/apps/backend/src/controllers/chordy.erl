-module(chordy).

-export([get/0]).

get() ->
    dht:representation(node0).
