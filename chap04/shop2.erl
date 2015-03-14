%%% shopping 'total' function reimplemented using map ans sum.

-module(shop2).
-export([total/1, test_total/0]).
-import(lists, [map/2, sum/1]).  % so we don't have to qualify names

total(L) ->
    sum(map(fun({What, N}) ->
                    shop:cost(What) * N end, L)).

test_total() ->
    0   = total([]),   % the base case.
    21  = total([{milk,3}]),
    75  = total([{pears,6}, {milk,3}]),
    tests_passed.

