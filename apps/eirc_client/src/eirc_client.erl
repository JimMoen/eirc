-module(eirc_client).
-export([start_client/0]).

-define(SERVER, "localhost").
-define(PORT, 2345).

start_client() ->
    SenderId = string:strip(io:get_line("Your nick name: "), right, $\n),
    {ok, Socket} = gen_tcp:connect(?SERVER, ?PORT,
                                   [binary, {packet, 4}]),
    Pid = spawn(fun() -> loop() end),
    gen_tcp:controlling_process(Socket, Pid),
    %% One tcp_packet per message(register; send message; ...)
    %% Auto Connect.
    gen_tcp:send(Socket, term_to_binary(user_edit_message(SenderId, "C"))),

    send_msg_packet(SenderId, Socket).


%% get_server_ip() ->
%%     UserInput = string:strip(io:get_line("Your Server ip: "), right, $\n),
%%     io:format(UserInput),
%%     case inet:parse_address("127.0.0.1") of
%%         {error, einval} ->
%%             io:format("Input error"),
%%             get_server_ip();
%%         {ok, Server_ip} ->
%%             Server_ip
%%     end.

loop() ->
    receive
        {tcp, _Socket, Bin} ->
            %% WHY "_Socket"???  All received tcp packets???
            Res = binary_to_term(Bin),
            io:format("Message info:::>>> ~p\n", [Res]),
            loop();
        {tcp_closed, _Socket} ->
            io:format("Socket closed.\n")
    end.


send_msg_packet(SenderId, Socket) ->
    Prompt = "Select your operation:\n" ++
             "  (S)end to specific user\n" ++
             "  (R)oom chat\n" ++
             "  (D)isconnect\n" ++
             "(TODO)Your Option: ",
    UserOptionChar = string:strip(io:get_line(Prompt), right, $\n),
    Option = string:to_upper(UserOptionChar),
    io:format(Option),
    %% SendPacket = user_edit_message(Option),
    %% gen_tcp:send(Socket, term_to_binary(SendPacket)),

    %% SendPacket = user_edit_message(Option),
    gen_tcp:send(Socket,
                 term_to_binary(user_edit_message(SenderId, Option))),
    send_msg_packet(SenderId, Socket).

%% Auto connect
user_edit_message(SenderId, "C") ->
    [SenderId, client_connect, 0, 0];
%% specific user private message
user_edit_message(SenderId, "S") ->
    ReceiverId = string:strip(io:get_line("Chat with: "), right, $\n),
    Message    = string:strip(io:get_line("Message:   "), right, $\n),
    %% I = io:get_line("Id(integer): "),
    %% %% "_Info" mains not important?? colul be ignored like variable "_"??
    %% {Id, _Info} = string:to_integer(I),
    [SenderId, private_chat, ReceiverId, Message];
%% room
user_edit_message(SenderId, "R") ->
    Message    = string:strip(io:get_line("Message:   "), right, $\n),
    [SenderId, room_chat, 0, Message];
%% disconnect
user_edit_message(SenderId, "D") ->
    %% I = io:get_line("Id(integer): "),
    %% {Id, _Info} = string:to_integer(I),
    [SenderId, client_disconnect, 0, 0];
%% invalid
user_edit_message(_, _) ->
    io:format("invalid operation. ~n"),
    [0, invalid_opreation, 0, 0].
