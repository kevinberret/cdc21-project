-module(key).
-export([generate/0, between/3, hash/1]).

generate() ->
    hash(node(self())).

between(_, From, To) when From == To ->
    % Let's define Key is between from and to for this case.
    true;
between(Key, From, To) when From > To ->
    % Since From is bigger than To, Key has to be smaller than To OR bigger than From to be between From & To.
    ((Key < To) or (Key > From));
between(Key, From, To) when From < To ->
    % Since From is smaller than To, Key has to be smaller than To AND bigger than From to be between From & To.
    ((Key < To) and (Key > From)).

hash(Atom) when is_atom(Atom) ->
    hash(atom_to_list(Atom));
hash(String) ->
    crypto:hash(sha, String).
