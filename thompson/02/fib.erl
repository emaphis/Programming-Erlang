-module(fib).
-export([fib/1]).
-include_lib("eunit/include/eunit.hrl").

fib(0) -> 1;
fib(1) -> 1;
fib(N) when N>1 -> fib(N-1) + fib(N-2).

fib0_test() -> ?assert(fib(0) == 1).
fib1_test() -> ?assert(fib(1) == 1).
fib2_test() -> ?assert(fib(2) == 2).

fib_test_() ->
    [?_assert(fib(0) == 1),
     ?_assert(fib(1) == 1),
     ?_assert(fib(2) == 2),
     ?_assert(fib(3) == 3),
     ?_assert(fib(4) == 5),
     ?_assert(fib(5) == 8),
     ?_assertError(function_clause, fib(-1))].
