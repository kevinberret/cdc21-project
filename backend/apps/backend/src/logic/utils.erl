-module(utils).

-export([ref_hash_to_string/1]).

ref_hash_to_string(Ref) ->
    Hash = lists:concat([integer_to_list(X, 16) || X <- binary_to_list(Ref)]),
    list_to_binary(Hash).
