%% Chapter 4 - Counting using recursion
-module(count).
-export([countdown/1, countup/1]).

%% Counting down
countdown(From) when From > 0 ->
    io:format("~w!~n", [From]),
    countdown(From-1);
countdown(_From) ->
    io:format("blastoff!~n").


%% Counting up.
countup(Limit) ->
    countup(1, Limit).

countup(Count, Limit) when Count =< Limit ->
    io:format("~w!~n", [Count]),
    countup(Count+1, Limit);
countup(_Count, _Limit) ->
    io:format("Finished.~n").
