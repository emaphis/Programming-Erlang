%%% @doc  Palindrome server
%%% Part1 of exercise

-module(palin).
-export([server/1, client/1]).


%% @doc the palindrome server.
server(From) ->
    receive
        {check, Str}  ->
            case palindrome_check(Str) of
                true  ->
                    From ! {result, "(" ++ Str ++ ") is a palindrome"};
                false ->
                    From ! {result, "(" ++ Str ++ ") is not a palindrome"}
            end,
            server(From);
        _ ->
           ok
    end.

%% test run:
%% 11> Self = self().
%% <0.31.0>
%% 12> Server = spawn(palin,server,[Self]).
%% <0.57.0>
%% 13> Server ! {check, "Madam"}.
%% {check,"Madam"}
%% 14> Sever ! {check, "dog"}.
%% * 1: variable 'Sever' is unbound
%% 15> Server ! {check, "dog"}.
%% {check,"dog"}
%% 16> Server ! {check, "Madam I\'m Adam"}.
%% {check,"Madam I'm Adam"}
%% 17> Server ! stop.
%% stop
%% 18> flush().
%% Shell got {result,"(Madam) is a palindrome"}
%% Shell got {result,"(dog) is not a palindrome"}
%% Shell got {result,"(Madam I'm Adam) is a palindrome"}
%% ok


%% @doc a client to send palindrome server a message.
client(Pal_server) ->
    receive
        {send, Str} ->
            Pal_server ! {check, Str},
            client(Pal_server);
        {result, Msg} ->
            io:format("recieved message: ~w~n", [Msg]),
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
