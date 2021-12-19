-module(data_route).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2, known_methods/2, get_data/2, post_data/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, post_data}],
     Req,
     State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_data}],
     Req,
     State}.

get_data(Req0, State) ->
    Key = cowboy_req:binding(key, Req0),
    Res = data:get(Key),
    case Res of
        timeout ->
            % Return timeout error.
            Req = cowboy_req:reply(
                408,
                Req0
            );
        false ->
            % Return timeout error.
            Req = cowboy_req:reply(
                408,
                Req0
            );
        _ ->
            % Return value found.
            Body = jiffy:encode(#{<<"value">> => Res}),
            Req = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"application/json">>},
                Body,
                Req0
            )
    end,
    {ok, Req, State}.

post_data(Req0, State) ->
    {ok, EncodedData, _} = cowboy_req:read_body(Req0),
    DecodedData = jiffy:decode(EncodedData),

    case DecodedData of
        {[{<<"key">>, Key}, {<<"value">>, Value}]} ->
            StoreRes = data:post(Key, Value),
            io:format("POST data add a key/value ~w/~w~n", [Key, Value])
    end,

    case StoreRes of
        timeout ->
            % Return timeout error.
            Code = 408;
        {inserted, {_, _}, _} ->
            Code = 201
    end,

    Req = cowboy_req:reply(
        Code,
        Req0
    ),
    {ok, Req, State}.

known_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.
