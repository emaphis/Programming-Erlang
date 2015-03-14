%% some example list processing functions

-module(mylists).
-export([sum/1, map/2]).
-export([test_sum/0, test_map/0]).

sum([H|T]) -> H + sum(T);
sum([])    -> 0.    % base case

test_sum() ->
    0  = sum([]),   % base case
    1  = sum([1]),
    14 = sum([1,3,10]),
    tests_passed.

map(_,[])     -> [];
map(F, [H|T]) -> [F(H) | map(F,T)].


test_map() ->
    [] = map(fun(X) -> X end, []),   % base case
    [2,4,6,8,10] =
        map(fun(X) -> 2*X end, [1,2,3,4,5]),
    [1,4,9,16,25] =
        map(fun(X) -> X*X end, [1,2,3,4,5]),
    tests_passed.
