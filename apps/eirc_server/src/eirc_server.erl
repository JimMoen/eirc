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
                    client_connect(SenderId, Socket),
                    loop(Socket);
                Signal =:= client_disconnect ->
                    client_disconnect(SenderId, Socket),
                    loop(Socket);
                Signal =:= private_chat ->
                    private_chat(SenderId, ReceiverId, Message),
                    loop(Socket);
                Signal =:= room_chat ->
                    room_chat(SenderId, Message),
                    loop(Socket);
                %% Signal =:= register_user -> register_user(SenderId, Socket);
                %% Signal =:= login_user -> login_user(SenderId, Socket);
                %% Signal =:= logout_user -> logout_user(SenderId, Socket);
                %% and so on behavior
                true -> io:format("Error signal ~p~n"),
                        loop(Socket)
            end;
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

%% user connect and auto login

client_connect(SenderId, Socket) ->
    case ets:match_object(user_table, {SenderId, '_', '_'}) of
        [] ->
            ets:insert(user_table, {SenderId, 1, Socket}),
            gen_tcp:send(Socket, term_to_binary("Your named " ++ SenderId ++ " logined.\n")),
            io:format("User ~p logined in\n", [SenderId]);
        [_, 1, _] ->
            io:format("Name has already used and connected");
        [_Ok] ->
            ets:update(user_table, SenderId, [{2, 1}, {3, Socket}])
        end.

%% user disconnect
client_disconnect(SenderId, Socket) ->
    case ets:match_object(user_table, {SenderId, 1, Socket}) of
        [_, _, _] ->
            ets:delete(user_table, {SenderId, '_', '_'}),
            gen_tcp:close(Socket)
    end.

private_chat(SenderId, ReceiverId, Message) ->
    %% if receiver is online.
    case ets:match_object(user_table, {ReceiverId, 1, '_'}) of
        [] ->
            io:format("Receiver is offline~n");
        [_, _, ReceiverSocket] ->
            gen_tcp:send(ReceiverSocket, term_to_binary(SenderId ++ "said:\n" ++ Message))
        end.

room_chat(SenderId, Message) ->
    case ets:match_object(user_table, {'_', 1, '_'}) of
        [_, 1, Socket] ->
            gen_tcp:send(Socket, term_to_binary(SenderId ++ "said:\n" ++ Message));
        [] ->
            "No one online."
        end.
