-module(data).

-export([code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2, get/1, post/2, start/0]).

% REST 
start() ->
    io:format("START DATA~n"),
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

init([]) ->
    io:format("INIT DATA~n"),
    BaseNode = node:start(0),
    register(node0, BaseNode),
    ok.

% These are all wrappers for calls to the server
% get(ISBN) -> gen_server:call(?MODULE, {get, ISBN}).

% get() -> gen_server:call(?MODULE, {get}).

get(Key) ->
    dht:locate(Key, node0).

post(Key, Value) ->
    dht:store(Key, Value, node0).
    % {inserted, {_, _}, _} = dht:store(Key, Value, node0),
    % {reply, inserted}.
% post(Key, _) ->
%     {inserted, {_, _}, _} = dht:store(Key, value0, node0),
%     {reply, inserted}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Process) -> {noreply, Process}.

handle_info(_Message, Process) -> {noreply, Process}.

terminate(_Reason, _Node) -> ok.

code_change(_OldVersion, Process, _Extra) ->
    {ok, Process}.
