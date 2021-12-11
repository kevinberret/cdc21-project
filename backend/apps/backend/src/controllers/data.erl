-module(data).

-export([code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2, get/1, post/2, start/0]).

% REST 
start() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

init([]) ->
    BaseNode = node:start(0),
    register(node0, BaseNode),
    ok.

get(Key) ->
    dht:locate(Key, node0).

post(Key, Value) ->
    dht:store(Key, Value, node0).

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Process) -> {noreply, Process}.

handle_info(_Message, Process) -> {noreply, Process}.

terminate(_Reason, _Node) -> ok.

code_change(_OldVersion, Process, _Extra) ->
    {ok, Process}.
