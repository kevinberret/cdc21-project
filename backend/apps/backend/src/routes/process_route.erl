-module(process_route).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2, known_methods/2, get_process/2, post_process/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, post_process}],
     Req,
     State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_process}],
     Req,
     State}.

get_process(Req, State) ->
    Message = {[{process, <<"ok">>}]},
    {jiffy:encode(Message), Req, State}.

post_process(Req, _) ->
    {ok, EncodedData, _} = cowboy_req:read_body(Req),
    DecodedData = jiffy:decode(EncodedData),
    case DecodedData of
        {[{<<"id">>, Id}]} ->
            process:post(Id),
            io:format("POST /process add a new node with Id ~w~n", [Id])
    end,
    cowboy_req:reply(
        201,
        Req
    ).

known_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.
