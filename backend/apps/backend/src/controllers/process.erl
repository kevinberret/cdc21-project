-module(process).

-export([code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2, post/1, start/0, get/0]).

% REST 
start() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

init([]) ->
    ok.

% These are all wrappers for calls to the server
% get(ISBN) -> gen_server:call(?MODULE, {get, ISBN}).

get() ->
    % Display node state.
    Res = node0 ! displayStore,
    io:format(Res),
    Res.

post(Id) ->
    % Create a new process
    node:start(Id, node0).

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Process) -> {noreply, Process}.

handle_info(_Message, Process) -> {noreply, Process}.

terminate(_Reason, _Node) -> ok.

code_change(_OldVersion, Process, _Extra) ->
    {ok, Process}.
