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

get_data(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    Res = data:get(Key),
    case Res of
        timeout ->
            % Return timeout error.
            cowboy_req:reply(
                404,
                Req
            );
        false ->
            % Return timeout error.
            cowboy_req:reply(
                404,
                Req
            );
        _ ->
            % Return value found.
            {jiffy:encode(#{<<"value">> => Res}), Req, State}
    end.

post_data(Req, _) ->
    {ok, EncodedData, _} = cowboy_req:read_body(Req),
    DecodedData = jiffy:decode(EncodedData),

    case DecodedData of
        {[{<<"key">>, Key}, {<<"value">>, Value}]} ->
            StoreRes = data:post(Key, Value),
            io:format("POST /process add a key/value ~w/~w~n", [Key, Value])
    end,

    case StoreRes of
        timeout ->
            % Return timeout error.
            cowboy_req:reply(
                404,
                Req
            );
        {inserted, {_, _}, _} ->
            cowboy_req:reply(
                201,
                Req
            )
    end.

known_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.
