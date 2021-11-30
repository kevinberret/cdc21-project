-module(node).
-export([start/1, start/3]).

-define(Stabilize, 1000).
-define(Timeout, 3000).

start(Id) ->
    start(Id, nil, []).
start(Id, Peer, Store) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer, Store) end).

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

init(Id, Peer, Store) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Store).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey,Peer}}
        after ?Timeout ->
            io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Store);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {add, Key, Value, Qref, Client} ->
            Added = storage:add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            storage:lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged)
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            % If this is nil we should of course inform it about our existence
            Spid ! {notify, {Id, self()}},
            Successor;
        {_, _} ->
            % If it is pointing back to us we don’t have to do anything.
            Successor;
        {Skey, _} ->
            % If it is pointing to itself we should of course notify it about our existence.
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            % If it’s pointing to another node we need to be careful. The question is if we are to slide in between the two nodes or if we should place ourselves behind the predecessor. If the key of the predecessor of our successor (Xkey) is between us and our successor we should of course adopt this node as our successor and run stabilization again. If we should be in between the nodes we inform our successor of our existence.
            case key:between(Xkey, Id, Skey) of
                true ->
                    Spid ! {notify, {Id, self()}},
                    Successor;
                false ->
                    Xpid ! {request, self()},
                    Pred
            end
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey,  _} ->
            case key:between(Nkey, Pkey, Id) of
                true -> 
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    Predecessor
            end
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok} ,
            storage:add(Key, Value, Store);            
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client} 
    end.

handover(Id, Store, Nkey, Npid) ->
    {Rest, Keep} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

create_probe(Id, Successor) ->
    {Skey, Spid} = Successor,
    Spid ! {probe, Id, [{Skey, Spid}], erlang:now()}.

remove_probe(T, Nodes) ->
    io:format("Probe : ~n",[]),
    lists:foreach(
      fun({Id, _}) -> io:format("    Node ~w ~n", [Id]) end,
      Nodes),
    io:format("Took ~w ms.~n",[timer:now_diff(erlang:now(), T)]).

forward_probe(Ref, T, Nodes, _Id, Successor) ->
    {Skey, Spid} = Successor,
    Spid ! {probe, Ref, [{Skey, Spid}|Nodes], T}.
