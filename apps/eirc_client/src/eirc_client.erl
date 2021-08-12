-module(eirc_client).
-export([start_client/0]).

-define(SERVER, "localhost").
-define(PORT, 2345).

start_client() ->
    {ok, Socket} = gen_tcp:connect(?SERVER, ?PORT,
                                   [binary, {packet, 4}]),
    Pid = spawn(fun() -> loop() end),
    gen_tcp:controlling_process(Socket, Pid),
    %% One tcp_packet per message(register; send message; ...)
    send_msg_packet(Socket).

    %% receive
    %%     {tcp, Socket, Bin} ->
    %%         io:format("Client receive binary = ~p~n", [Bin]),
    %%         Value = binary_to_term(Bin),
    %%         io:format("Client results = ~p~n", [Value]),
    %%         gen_tcp:close(Socket)
    %% end.


loop() ->
    receive
        {tcp, _Socket, Bin} ->
            %% WHY "_Socket"???  All received tcp packets???
            Res = binary_to_term(Bin),
            io:format("Message info:::>>> ~p ~n", [Res]),
            loop();
        {tcp_closed, _Socket} ->
            io:format("Socket closed.~n")
    end.


send_msg_packet(Socket) ->
    UserOperation = io:get_line("Select your operation:\n  1: (Register) 2: TODO\n"),
    {Option, _Info} = string:to_integer(UserOperation),
    SendPacket = user_edit_message(Option),
    gen_tcp:send(Socket, term_to_binary(SendPacket)),
    send_msg_packet(Socket).


user_edit_message(1) ->
    I = io:get_line("Id(integer): "),
    %% "_Info" mains not important?? colul be ignored like variable "_"??
    {Id, _Info} = string:to_integer(I),
    [Id, register_user, 0, 0].
