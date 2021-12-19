-module(node).
-export([start/0, start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 3000).

start() ->
    start(key:generate()).
start(Id) ->
    start(Id, nil).
start(Id, Peer) ->
    spawn(fun() -> init(Id, Peer) end).

% Schedule pointer refreshments.
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

init(Id, Peer) ->
    Predecessor = nil,
    Store = storage:create(),
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Store, nil).

connect(Id, nil) ->
    {ok, {Id, monitor(self()), self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, monitor(Peer), Peer}}
        after ?Timeout ->
            % Set the successor to ourself.
            io:format("Time out: no response~n",[]),
            {ok, {Id, monitor(self()), self()}}
    end.

node(Id, Predecessor, Successor, Store, Next) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);
        {notify, New} ->
            {Pred, Keep} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Keep, Next);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Store, Nxt);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {'DOWN', Ref, process, _, _} ->
            io:format("node with ref ~w is down!!~n", [Ref]),
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);
        probe ->
            create_probe(Id, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {add, Key, Value, Qref, Client} ->
            io:format("~w ~n", [Predecessor]),
            io:format("~w ~n", [Successor]),
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);
        displayStore ->
            io:format("Node ~w: Store data:~n", [Id]),
            io:format("~p~n", [Store]),
            node(Id, Predecessor, Successor, Store, Next);
        displayInfo ->
            io:format("Node ~w: my pred is ~w my succ is ~w my next is ~w ~n", [Id, Predecessor, Successor, Next]),
            node(Id, Predecessor, Successor, Store, Next);
        {initRepresentation, Qref, Client} ->
            {_, _, Spid} = Successor,
            Spid ! {representation, Qref, Client, Id, []},
            node(Id, Predecessor, Successor, Store, Next);
        {representation, Qref, Client, BaseId, Representation} ->
            representation(Qref, Client, BaseId, Id, Predecessor, Successor, Next, Store, Representation),
            node(Id, Predecessor, Successor, Store, Next)
    end.

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Next, Id, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            % If this is nil we should of course inform it about our existence
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Id, _} ->
            % If it is pointing back to us we don’t have to do anything.
            {Successor, Next};
        {Skey, _} ->
            % If it is pointing to itself we should of course notify it about our existence.
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Xkey, Xpid} ->
            % If it’s pointing to another node we need to be careful. The question is if we are to slide in between the two nodes or if we should place ourselves behind the predecessor.
            % If the key of the predecessor of our successor (Xkey) is between us and our successor we should of course adopt this node as our successor and run stabilization again.
            % If we should be in between the nodes we inform our successor of our existence.
            case key:between(Id, Xkey, Skey) of
                true ->
                    Spid ! {notify, {Id, self()}},
                    {Successor, Next};
                false ->
                    drop(Sref),
                    stabilize(Pred, Next, Id, {Xkey, monitor(Xpid), Xpid})
            end
    end.

request(Peer, Predecessor, {Skey, _, Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, _, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    % io:format("~w has been notified by ~w as a new predecessor~n", [Id, Predecessor]),
    case Predecessor of
        nil ->
            % io:format("currently no pred ~n"),
            Keep = handover(Store, Nkey, Npid),
            {{Nkey, monitor(Npid), Npid}, Keep};
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true -> 
                    Keep = handover(Store, Nkey, Npid),
                    drop(Pref),
                    {{Nkey, monitor(Npid), Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    HKey = key:hash(Key),
    io:format("~w ~w ~w ~n", [HKey, Value, Id]),
    case key:between(HKey, Pkey, Id) of
        true ->
            Client ! {Qref, ok} ,
            storage:add(HKey, Value, Store);            
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    HKey = key:hash(Key),
    io:format("lookup ~w in store ~w ~n", [HKey, Store]),
    case key:between(HKey, Pkey, Id) of
        true ->
            {_, Value} = storage:lookup(HKey, Store),
            Client ! {Qref, Value};
        false ->
            {_, _, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client} 
    end.

handover(Store, Nkey, Npid) ->
    {Rest, Keep} = storage:split(Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

create_probe(Id, Successor, Store) ->
    {_, _, Spid} = Successor,
    Spid ! {probe, Id, [{Id, self(), Store}], erlang:now()}.

remove_probe(T, Nodes) ->
    io:format("Probe : ~n",[]),
    lists:foreach(
      fun({Id, _}) -> io:format("    Node ~w ~n", [Id]) end,
      Nodes),
    io:format("Took ~w ms.~n",[timer:now_diff(erlang:now(), T)]).

forward_probe(Ref, T, Nodes, Id, Successor, Store) ->
    {_, _, Spid} = Successor,
    Spid ! {probe, Ref, [{Id, self(), Store}|Nodes], T}.

monitor(Pid) ->
    erlang:monitor(process, Pid).
drop(nil) ->
    ok;
drop(Pid) ->
    erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
    % We don't know our predecessor so define it to nil.
    {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    % Adopt next as successor and define next to nil.
    drop(Ref),
    Nref = monitor(Npid),
    {Predecessor, {Nkey, Nref, Npid}, nil}.

representation(Qref, Client, BaseRef, ThisRef, Predecessor, Successor, Next, Store, Representation) ->
    {_, _, Spid} = Successor,
    {Pref, _, _} = Predecessor,
    {Nref, _} = Next,
    ThisStore = storage:to_json(Store),
    ThisRepresentation = #{
        <<"hash">> => utils:ref_hash_to_string(ThisRef),
        <<"name">> => node(),
        <<"store">> => ThisStore,
        <<"predecessor">> => utils:ref_hash_to_string(Pref),
        <<"next">> => utils:ref_hash_to_string(Nref)
    },
    WholeRepresentation = [ThisRepresentation | Representation],
    case ThisRef == BaseRef of
        % The whole DHT has been traversed.
        true ->
            Client ! {Qref, WholeRepresentation};
        false ->
            Spid ! {representation, Qref, Client, BaseRef, WholeRepresentation}
    end.
