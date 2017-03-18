%% Exercises.

-module(exercises).
-export([sum/1, sum/2, create/1, reverse_create/1, print_num/1, print_even/1,filter/2,reverse/1,join/2,concat/1,flatten/1]).
-include_lib("eunit/include/eunit.hrl").


%% 3-1:
%% evaluating expressions:

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
%% creating lists:

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
%% side effects:

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

%% Ex 3-4 Db
%% see: db.erl


%% Ex 3-5
%% manipulating lists:

%% filter all numbers in a list less than or equal to a given
%% number.

filter([],_) ->
    [];
filter([H|T],N) when H =< N ->
    [H|filter(T,N)];
filter([_|T],N) ->
    filter(T,N).

filter_test_() ->
    [?_assertEqual([], filter([],3)),
     ?_assertEqual([], filter([4,5,6],3)),
     ?_assertEqual([1,2,3], filter([1,2,3],3)),
     ?_assertEqual([1,2,3], filter([1,2,3,4,5],3))].


%% reverse a list
reverse(Lst) ->
    reverse(Lst,[]).
reverse([],Acc) ->
    Acc;
reverse([H|L],Acc) ->
    reverse(L, [H|Acc]).

reverse_test_() ->
    [?_assertEqual([], reverse([])),
     ?_assertEqual([1], reverse([1])),
     ?_assertEqual([3,2,1], reverse([1,2,3]))]. 


%% join two lists
join([],Ys) ->
    Ys;
join(Xs,[]) ->
    Xs;
join([X|Xs],Ys) ->
    [X | join(Xs, Ys)].

join_test_() ->
    [?_assertEqual([], join([],[])),
     ?_assertEqual([1,2,3], join([1,2,3],[])),
     ?_assertEqual([1,2,3], join([],[1,2,3])),
     ?_assertEqual([1,2,3,4,5,6], join([1,2,3],[4,5,6]))].

%% concatinate a list of lists into on list
concat(Lst) ->
    concat(Lst,[]).
concat([],Acc) ->
    Acc;
concat([H|T],Acc) ->
    concat(T,Acc++H).

concat_test_() ->
    [?_assertEqual([], concat([])),
     ?_assertEqual([1,2,3,4,five], concat([[1,2,3],[],[4,five]]))].


flatten([]) ->
    [];
flatten([H|T]) ->
    concat(flatten(H), flatten(T));
flatten(X) ->
    [X].

flatten_test_() ->
    [?_assertEqual([5], flatten([5])),
     ?_assertEqual([1,2,3], flatten([[1],[2],[3]])),
     ?_assertEqual([1,2,3,4,5,6], flatten([[1,[2,[3],[]]], [[[4]]], [5,6]]))].
