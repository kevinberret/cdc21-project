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
    {ok, EncodedData, _} = cowboy_req:read_body(Req),
    DecodedData = jiffy:decode(EncodedData),

    case DecodedData of
        {[{<<"key">>, Key}]} ->
            Res = data:get(Key)
    end,
    io:format("found VALUE ~w ~n", [Res]),
    {jiffy:encode(#{<<"result">> => Res}), Req, State}.

    % cowboy_req:reply(
    %     201,
    %     #{<<"content-type">> => <<"application/json">>},
    %     jiffy:encode({#{<<"result">> => Res}}),
    %     Req
    % ).

post_data(Req, _) ->
    {ok, EncodedData, _} = cowboy_req:read_body(Req),
    DecodedData = jiffy:decode(EncodedData),

    case DecodedData of
        {[{<<"key">>, Key}, {<<"value">>, Value}]} ->
            data:post(Key, Value),
            io:format("POST /process add a key/value ~w/~w~n", [Key, Value])
    end,

    cowboy_req:reply(
        201,
        Req
    ).

known_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.
