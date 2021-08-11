-module(eirc_client).
-export([start_nano_client/1]).

start_nano_client(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345,
                                   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client receive binary = ~p~n", [Bin]),
            Value = binary_to_term(Bin),
            io:format("Client results = ~p~n", [Value]),
            gen_tcp:close(Socket)
    end.
