-module(palin).
-export([palin/1,nopunct/1,palindrome/1,server/0,client/2]).


server() ->
    receive
        {check, Pid, Str} ->
            case palindrome(Str) of
                true ->
                    Pid ! {result, Str ++ " is a palindrome"};
                false ->
                    Pid ! {result, Str ++ " is not a palindrome"}
            end,
            server();
        _ ->
            ok
    end.


client(Pid,Str) ->
    Pid ! {check, self(), Str},
    receive
        {result, Result} ->
              io:format("Result from server: ~p~n", [Result])
    end.
    
%% test run
%% (emacs@kimba01)2> Palin = spawn(palin,server,[]).
%% <0.44.0>
%% (emacs@kimba01)3> spawn(palin,client,[Palin,"hello world"]).
%% Result from server: "hello world is not a palindrome"
%% <0.46.0>
%% (emacs@kimba01)4> spawn(palin,client,[Palin,"Madam I\'m Adam"]).
%% Result from server: "Madam I'm Adam is a palindrome"
%% <0.48.0>
%% (emacs@kimba01)5> Palin ! stop.
%% stop
%% (emacs@kimba01)6> 


% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

 
	


