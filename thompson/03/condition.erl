%% conditional expressions

-module(condition).
-export([listlen1/1,listlen/1,index/2,factorial/1, even/1, number/1]).
-include_lib("eunit/include/eunit.hrl").


listlen1([]) -> 0;
listlen1([_|Xs]) -> 1 + listlen1(Xs).

listlen1_test_() ->
    [?_assert(listlen1([]) == 0),
     ?_assert(listlen1([1]) == 1),
     ?_assert(listlen1([1,2,3,4,5]) == 5)].

listlen(Y) ->
    case Y of
        []     -> 0;
        [_|Xs] -> 1 + listlen(Xs)
    end.

listlen_test_() ->
    [?_assert(listlen([]) == 0),
     ?_assert(listlen([1]) == 1),
     ?_assert(listlen([1,2,3,4,5]) == 5)].


% match on multiple parameters
index(X,Y) ->
    index({X,Y}).

index(Z) ->
    case Z of
        {0,[X|_]}           -> X;
        {N,[_|Xs]} when N>0 -> index(N-1,Xs)
    end.

index_test_() ->
    [?_assert(index(0, [1,2,3]) == 1),
     ?_assert(index(1, [1,2,3]) == 2)].


%unsafe(X) ->
%    case X of
%        one -> Y = true;
%        _   -> Z = two
%    end,
%    Y.

%% factorial using a guard.
factorial(N) when N > 0 ->
    N * factorial(N - 1);
factorial(0) -> 1.


even(Int) when Int rem 2 == 0 -> true;
even(Int) when Int rem 2 == 1 -> false.

even_test_() ->
    [ ?_assert(even(2)),
      ?_assertNot(even(3)) ].

number(Num) when is_integer(Num) -> integer;
number(Num) when is_float(Num)   -> float;
number(_Other) -> false.

number_test_() ->
    [ ?_assertEqual(number(10), integer),
      ?_assertEqual(number(1.0), float),
      ?_assertNot(number(number)) ].
