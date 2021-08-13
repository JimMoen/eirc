-module(eirc_server).
-export([start_server/0]).

-import(ets, []).

-define(LISTEN_PORT, 2345).

% start server and Listen port 2345.
start_server() ->
    %% maintain user data.
    ets:new(user_table, [ordered_set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    %%   atom(),           ordered     \|/        named               \                          /
    %%      list_of_options[],any process could read|write             \_  Performance tuning. _/
    case gen_tcp:listen(?LISTEN_PORT,
                   [binary, {packet, 4},
                    {reuseaddr, true},
                    {active, true}])
    of
        {ok, ListenSocket} ->
            % gen_tcp listen socket successful.
            % spawn means a independent to handle listen.
            spawn(fun() -> client_connect(ListenSocket) end);
        {error, Reason} ->
            %% gen_tcp error.
            io:format("Server error. ~p~n", [Reason])
    end.


client_connect(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        % When the listen socket accept a connect.
        {ok, Socket} ->
            % generate a progress to handle a session with the client.
            spawn(fun() -> client_connect(ListenSocket) end),
            % handle the specific ssession
            loop(Socket);
        {error, Reason} ->
            io:format("Connect error. ~p~n", [Reason])
    end.


loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            [SenderId, Signal, ReceiverId, Message] = binary_to_term(Bin),
            if
                Signal =:= client_connect ->
                    ConnectResult = client_connect(SenderId, Socket),
                    gen_tcp:send(Socket, term_to_binary(ConnectResult)),
                    loop(Socket);
                Signal =:= client_disconnect ->
                    DisconnectResult = client_disconnect(SenderId, Socket),
                    gen_tcp:send(Socket, term_to_binary(DisconnectResult)),
                    loop(Socket);
                Signal =:= private_chat ->
                    PrivateResult = private_chat(SenderId, ReceiverId, Message),
                    gen_tcp:send(Socket, term_to_binary(PrivateResult)),
                    loop(Socket);
                Signal =:= room_chat ->
                    RoomChatRes = room_chat(SenderId, Message),
                    gen_tcp:send(Socket, term_to_binary(RoomChatRes)),
                    loop(Socket);
                %% and so on behavior
                true ->
                    io:format("Error signal."),
                    loop(Socket)
            end;
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

%% user connect and auto login
%% return a string to describe login result.
client_connect(SenderId, Socket) ->
    case ets:match_object(user_table, {SenderId, '_', '_'}) of
        [] ->
            ets:insert(user_table, {SenderId, 1, Socket}),
            io:format("User ~p logined in.\n", [SenderId]),
            "Your named " ++ SenderId ++ " register just now and logined.";
        [{_, 1, _}] ->
            "Name has already used and connected";
        [_] ->
            ets:update_element(user_table, SenderId, [{2, 1}, {3, Socket}]),
            "Your named " ++ SenderId ++ " logined."
    end.

%% user disconnect
client_disconnect(SenderId, Socket) ->
    case ets:match_object(user_table, {SenderId, 1, Socket}) of
        [{_, _, _}] ->
            %% gen_tcp:close(Socket),
            ets:update_element(user_table, {SenderId, {2, 0}, {3, 0}}),
            "Your name " ++ SenderId ++ "logouted."
    end.

private_chat(SenderId, ReceiverId, Message) ->
    %% case receiver.
    case ets:match_object(user_table, {ReceiverId, '_', '_'}) of
        %% Does not exist.
        [] ->
            DoesNotExist = "Receiver does not exist.",
            io:format(DoesNotExist),
            DoesNotExist;
        %% OffLine.
        [{'_', 0, '_'}] ->
            "Receiver offline.";
        %% Online. And send message.
        [{_, 1, ReceiverSocket}] ->
            message_send(SenderId, ReceiverSocket, Message),
%            gen_tcp:send(ReceiverSocket, term_to_binary(SenderId ++ " said: " ++ Message)),
            "Send to" ++ ReceiverId ++ " Succeed."
    end.

room_chat(SenderId, Message) ->
    lists:foreach(fun({_, 1, Socket}) ->
                          gen_tcp:send(Socket, term_to_binary(SenderId ++ " said to everyone: " ++ Message)) end,
                          ets:match_object(user_table, {'_', 1, '_'})).

message_send(SenderId, ReceiverSocket, Message) ->
    gen_tcp:send(ReceiverSocket, term_to_binary(SenderId ++ " said: " ++ Message)).
