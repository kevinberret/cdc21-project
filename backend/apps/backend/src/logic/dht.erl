-module(dht).
-export([store/3, locate/2]).
-define(Timeout, 3000).

store(Key, Value, Peer) ->
  Qref = make_ref(),
  io:format("~w ~w ~w ~w ~n", [Peer, Key, Value, Qref]),
  Peer ! {add, Key, Value, Qref, self()},
  receive
    {Qref, Ans} ->
      {inserted, {Key, Value}, Ans}
  after
    ?Timeout ->
      io:format("Time out: no response~n", [])
  end.

locate(Key, Peer) ->
  Qref = make_ref(),
  Peer ! {lookup, Key, Qref, self()},
  receive
    {Qref, Node, Item} ->
      {Node, Item};
    {Qref, Item} ->
      Item
  after
    ?Timeout ->
      io:format("Time out: no response~n", []),
      timeout
  end.
