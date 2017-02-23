%% Exercises.

-module(exercises).
-export([sum/1, sum/2, create/1, reverse_create/1, print_num/1, print_even/1]).
-include_lib("eunit/include/eunit.hrl").


%% 3-1:

sum(0) -> 0;
sum(N) -> N + sum(N-1).

sum_test_() ->
    [?_assertEqual(0, sum(0)),
     ?_assertEqual(1, sum(1)),
     ?_assertEqual(6, sum(3)),
     ?_assertEqual(15, sum(5))].


sum(N,N) -> N;
sum(N,M) when N>M -> throw({error, "bad numbers"});
sum(N,M) -> M + sum(N,M-1).

sum2_test_() ->
    [?_assertEqual(6, sum(1,3)),
     ?_assertEqual(6, sum(6,6)),
     ?_assertEqual({error,"bad numbers"}, catch sum(7,6))].


%% Ex 3-2:

create(N) -> create(N,[]).

create(0,Acc) -> Acc;
create(N,Acc) -> create(N-1, [N|Acc]).

create_test_() ->
    [?_assertEqual([], create(0)),   % base case
     ?_assertEqual([1], create(1)),
     ?_assertEqual([1,2,3], create(3))].


reverse_create(0) -> [];
reverse_create(N) -> [N | reverse_create(N-1)].

create_reverse_test_() ->
    [?_assertEqual([], reverse_create(0)),
     ?_assertEqual([3,2,1], reverse_create(3))].


%% Ex 3-3

print_num(N) ->  print_num(N,1).

print_num(1,Acc) -> io:format("Number:~p~n", [Acc]);
print_num(N,Acc) -> io:format("Number:~p~n", [Acc]),
                    print_num(N-1,  Acc+1).

% io:format("Number:~p~n", [Acc+1])

print_even(N) ->  print_even(N,1).


print_even(1,Acc) when Acc rem 2 == 0 ->
    io:format("Number:~p~n", [Acc]);
print_even(1,_) -> io:format("", []);
print_even(N,Acc) when Acc rem 2 == 0 ->
    io:format("Number:~p~n", [Acc]),
    print_even(N-1,  Acc+1);
print_even(N,Acc) -> print_even(N-1, Acc+1).
