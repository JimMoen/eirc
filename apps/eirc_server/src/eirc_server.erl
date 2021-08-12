-module(eirc_server).
-export([start_server/0]).

-import(ets, []).

-define(LISTEN_PORT, 2345).

% start server and Listen port 2345.
start_server() ->
    %% maintain user data.
    ets:new(id, [ordered_set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
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
            [Id, Sign, SendId, Message] = binary_to_term(Bin),
            if
                Sign =:= register_user -> register_user(Id, Socket);
                true -> io:format("Error sign ~p~n"),
                        loop(Socket)
            end;
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

register_user(Id, Socket) ->
    case ets:lookup(id, Id) of
        [_Ok] ->
            io:format("Account already exists. ~n");
        _ ->
            ets:insert(id, {Id, 0, Socket}),
            io:format("User register succeed. ~n")
    end.



%% chat_with_user(SendId, Socket, Message) ->
%%     case ets:match_object(id, {'_', '_', 1, Socket}) of
%%         % match the specific connection.
%%         [{Id, _, _, _}] ->
%%             Result = ets:match_object(id, {SendId, '_', 1, '_'}),
%%             case Result =:= [] of
%%                 true ->
%%                     io:format("The user is offline ~p ~n", [Result]);
%%                 _    ->
%%                     send_to_user(Result, Id, Message)
%%             end;
%%         _ -> io:format("Private chat fails~n")
%%     end.


%% send_to_user([Info], Id, Message) ->
%%     {_, _, _, Socket} = Info,
%%     gen_tcp:send(Socket, term_to_binary("Message from" ++ integer_to_list(Id) ++ ": " ++ Message)).
