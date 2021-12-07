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
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),
    BaseNode = node:start(0),
    register(node0, BaseNode),

    backend_sup:start_link().

stop(_State) -> ok = cowboy:stop_listener(http).

%% internal functions
