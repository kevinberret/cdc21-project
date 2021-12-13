%%%-------------------------------------------------------------------
%% @doc backend public API
%% @end
%%%-------------------------------------------------------------------

-module(backend_app).

-behaviour(application).

-export([start/2, stop/1]).

% start(_StartType, _StartArgs) ->
%     backend_sup:start_link().

% stop(_State) ->
%     ok.

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
        [
            %% {HostMatch, list({PathMatch, Handler, InitialState})}
            {'_', [
                {"/process", process_route, #{}},
                {"/data", data_route, #{}},
                {"/data/:key", data_route, #{}},
                {"/health", health_route, #{}}
            ]
            }
        ]
    ),
    Port = 8080,
    {ok, _} = cowboy:start_clear(http,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),
    io:format("===================================== ~n"),
    io:format("My name is ~w~n", [node()]),
    io:format("===================================== ~n"),
    io:format("~n"),
    io:format("~n"),
    io:format("~n"),
    io:format("~n"),
    io:format("Starting the backend node on ~w...~n", [Port]),
    BaseNode = node:start(key:generate()),
    io:format("Registering backend node's name...~n"),
    register(node0, BaseNode),
    io:format("Backend node started, ready...~n"),

    backend_sup:start_link().

stop(_State) -> ok = cowboy:stop_listener(http).

%% internal functions
