-module(mylist_tests).
-include_lib("eunit/include/eunit.hrl").

sum_test() ->
    ?assertEqual(0, mylist:sum([])),
    ?assertEqual(0, mylist:sum([0])),
    ?assertEqual(6, mylist:sum([1,2,3,4,-4])).

product_test() ->
    ?assertEqual(1, mylist:product([])),
    ?assertEqual(2, mylist:product([2])),
    ?assertEqual(36, mylist:product([2,3,2,3])).

odds_test() ->
    ?assertEqual([], mylist:odds([])),
    ?assertEqual([1], mylist:odds([1])),
    ?assertEqual([1,3,5], mylist:odds([1,2,3,4,5])).
