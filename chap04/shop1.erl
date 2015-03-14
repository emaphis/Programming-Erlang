-module(shop1).
-export([total/1, test/0]).

total([{What, N} | T]) -> shop:cost(What) * N + total(T);
total([])              -> 0.



%% a testing function

test() ->
    0   = total([]),   % the base case.
    21  = total([{milk,3}]),
    75  = total([{pears,6}, {milk,3}]),
    tests_passed.
