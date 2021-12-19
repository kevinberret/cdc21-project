-module(dht).
-export([store/3, locate/2, representation/1]).
-define(Timeout, 3000).

store(Key, Value, Peer) ->
  Qref = make_ref(),
  Peer ! {add, Key, Value, Qref, self()},
  receive
    {Qref, Ans} ->
      {inserted, {Key, Value}, Ans}
  after
    ?Timeout ->
      io:format("Time out: no response~n", []),
      timeout
  end.

locate(Key, Peer) ->
  Qref = make_ref(),
  Peer ! {lookup, Key, Qref, self()},
  receive
    {Qref, {Key, Value}} ->
      Value;
    {Qref, _, Item} ->
      Item;
    {Qref, Item} ->
      Item
  after
    ?Timeout ->
      io:format("Time out: no response~n", []),
      timeout
  end.

representation(Peer) ->
  Qref = make_ref(),
  Peer ! {initRepresentation, Qref, self()},
  receive
    {Qref, Representation} ->
      Representation
  after
    ?Timeout ->
      io:format("Time out: no response~n", []),
      timeout
  end.
