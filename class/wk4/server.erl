%%% @doc Palindrome client and server.
%%% Part 2 of exercise.
-module(server).
-export([server/0, client/1]).


%% @doc the palindrome server.
server() ->
    receive
        {check, Str, From}  ->
            From ! {result, palindrome_check(Str), Str},
            server();
        _ ->
           ok
    end.

%% @doc a client to send palindrome server a message.
client(Pal_server) ->
    receive
        {send, Str} ->
            Pal_server ! {check, Str, self()},
            client(Pal_server);
        {result, Is_pal, Msg} ->
            case Is_pal of
                true  ->
                    io:format("(~s) is a palindrome~n",[Msg]);
                false -> 
                    io:format("(~s) is not a palindrome~n",[Msg])
            end,
            client(Pal_server);
        _ -> ok
    end.


%%% Palindrome functions

rem_punct(String) ->
    lists:filter(fun (Ch) ->
                         not(lists:member(Ch,"\"\'\t\n "))
                 end,
                 String).

to_small(String) ->
    lists:map(fun(Ch) ->
                      case ($A =< Ch andalso Ch =< $Z) of
                          true -> Ch+32;
                          false -> Ch
                      end
              end,
              String).

palindrome_check(String) ->
    Normalise = to_small(rem_punct(String)),
    lists:reverse(Normalise) == Normalise.

%% example run:
%% 2> Server = spawn(server,server,[]).
%% <0.38.0>
%% 3> Client = spawn(server,client,[Server]).
%% <0.40.0>
%% 4> Client ! {send, "hello"}.
%% (hello) is not a palindrome
%% {send,"hello"}
%% 5> Client ! {send, "Madam"}.
%% (Madam) is a palindrome
%% {send,"Madam"}
%% 6> Server ! stop.
%% stop
%% 7> Client ! stop.
%% stop
%% 8>
