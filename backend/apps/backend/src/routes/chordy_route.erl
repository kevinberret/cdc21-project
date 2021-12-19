-module(chordy_route).

-export([init/2, allowed_methods/2, content_types_provided/2, known_methods/2, get_dht_representation/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_dht_representation}],
     Req,
     State}.

get_dht_representation(Req0, State) ->
    Response = chordy:get(),
    case Response of
        timeout ->
            % Return timeout error.
            Req = cowboy_req:reply(
                408,
                Req0
            );
        _ ->
            Req = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"application/json">>},
                jiffy:encode(Response),
                Req0
            )
    end,
    {ok, Req, State}.

known_methods(Req, State) ->
    Result = [<<"GET">>],
    {Result, Req, State}.
