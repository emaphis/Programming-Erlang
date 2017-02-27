%% list stuff.

-module(list).

-export([head/1,tail/1, second/1, second2/1, sum/1, sum_tc/1]).
-export([list_test/0, sum_test/0]).

%% list - order matters and multiplicity matters
%% [] - empty list
%% [1,2,3] - nonempty list
%% [X|Xs] - pattern matches a non empty list.


head([X|_Xs]) -> X.
tail([_X|Xs]) -> Xs.

second(Xs) -> head(tail(Xs)).
second2([_X,Y|_Zs]) ->Y.

list_test() ->
    1 = head([1]),
    1 = head([1,2,3]),
    [] = tail([1]),
    [2,3] = tail([1,2,3]),
    2 = second([1,2,3]),
    2 = second2([1,2,3]),
    ok.

%% building list and defining funtions over lists.

%% [X|Xs] -- cons operator

%% template for recursion over lists
%% define a function by recursion over lists:
%% foo([])     -> .....;
%% foo([X|Xs]) -> ... foo(Xs) ...

sum([])     -> 0;
sum([X|Xs]) -> X + sum(Xs).

sum_test() ->
    0 = sum([]),
    14 = sum([2,3,4,5]),
    ok.

%% tail-call sum
sum_tc(Xs)       -> sum_tc(Xs,0).

sum_tc([],S)     -> S;
sum_tc([X|Xs],S) -> sum_tc(Xs, X+S).
