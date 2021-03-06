-module(storage).
-export([create/0, add/3, lookup/2, split/2, merge/2, to_json/1]).

create() ->
    [].

add(Key, Value, L) ->
    io:format("add ~w:~w ~n", [Key, Value]),
    lists:keystore(Key, 1, L, {Key, Value}).

lookup(Key, L) ->
    Res = lists:keyfind(Key, 1, L),
    Res.

split(Key, L) ->
    lists:partition(fun({K,_}) -> K =< Key end, L).

merge(L1, L2) ->
    lists:keymerge(1, L1, L2).

to_json(L)->
    F = fun({Key, Value}, Acc) -> [#{utils:ref_hash_to_string(Key) => Value} | Acc] end,
    lists:foldl(F, [], L).
