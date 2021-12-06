-module(node).
-export([start/1, start/3]).

-define(Stabilize, 1000).
-define(FixFingers, 2000).
-define(Timeout, 3000).
-define(FTLength, 4).

start(Id) ->
    start(Id, nil, []).

start(Id, Peer, Store) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer, Store) end).

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

schedule_fixingFingers() ->
    timer:send_interval(?FixFingers, self(), fix_fingers).

init(Id, Peer, Store) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    FingerTable = [{X, Successor} || X <- lists:seq(1,?FTLength)],
    schedule_stabilize(),
    schedule_fixingFingers(),
    node(Id, Predecessor, Successor, Store, FingerTable).

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

node(Id, Predecessor, Successor, Store, FT) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, FT);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Store, FT);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store, FT);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store, FT);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, FT);
	% finger table related
	fix_fingers ->
	    FixedTable = fixFTable(Id, FT),
	    node(Id, Predecessor, Successor, Store, FixedTable);
	{looking4Finger, Key, Peer} ->
	    finger(Id, Key, Successor, Peer),
	    node(Id, Predecessor, Successor, Store, FT);
	{requested, Finger4Key} ->
	    FixedTable = fixFTable(Id,Finger4Key),
	    node(Id, Predecessor, Successor, Store, FixedTable);
	% probing related
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, FT);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, FT);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, FT);
	% storage related
        {add, Key, Value, Qref, Client} ->
            Added = storage:add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, FT);
        {lookup, Key, Qref, Client} ->
            storage:lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, FT);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, FT)
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            % If this is nil we should of course inform it about our existence
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
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

%===============STORE=========================
% adding key,value to store
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok} ,
            storage:add(Key, Value, Store);   
	% does not belong in self() 's store      
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_,Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
	% isn't in self() 's store, forward request 
        false ->
            Spid ! {lookup, Key, Qref, Client} 
    end.

% allows distribution of node's storage with another node
handover(Id, Store, Nkey, Npid) ->
    {Rest, Keep} = storage:split(Nkey, Store), %    {Rest, Keep} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

%===========FINGER TABLE==========
fixFTable(Id, FT) ->
	FingerTable = [self() ! {looking4Finger, Id+math:pow(2,K), self()}|| {K,_} <- FT],
	io:format("Finger table for ~p is ~p ~n",[Id, FingerTable]).

fixFinger(Id, {K, Node}) ->
	Node ! {looking4Finger, (Id+math:pow(2,K)), self()}.

%finger(Key, Successor, Peer)
finger(Id,Key, Successor, Peer) ->
  {_,SuccKey} = Successor,
  case key:between(Key, Id, SuccKey) of 
	true -> 
	  Peer ! {requested, Successor};
	false ->
	  Successor ! {looking4Finger,Key, Peer}
  end.

%===========PROBING=============

create_probe(Id, Successor) ->
    {Skey, Spid} = Successor,
    Spid ! {probe, Id, [{Skey, Spid}], erlang:monotonic_time()}.

remove_probe(T, Nodes) ->
    io:format("Probe : ~n",[]),
    lists:foreach(
      fun({Id, _}) -> io:format("Node ~w ~n", [Id]) end,
      Nodes),
    io:format("Took ~w ms.~n",[timer:monotonic_time()-T]).

forward_probe(Ref, T, Nodes, _Id, Successor) ->
    {Skey, Spid} = Successor,
    Spid ! {probe, Ref, [{Skey, Spid}|Nodes], T}.
