%%% implementations of math functions.

-module(math_functions).
-export([even/1, odd/1, filter/2, split/1]).
-export([test_even/0, test_odd/0, test_filter/0, test_split/0]).

%% exercise 5, even and odd functions:
even(Num) when Num rem 2 =:= 0 -> true;
even(_) -> false.

odd(Num ) when Num rem 2 =:= 1 -> true;
odd(_)  -> false.

test_even() ->
    true = even(4),
    false = even(1),
    tests_passed.

test_odd() ->
    false = odd(4),
    true  = odd(1),
    tests_passed.

%% exercise 6 - a filter function:
filter(_, []) -> [];
filter(P, [H|T]) ->
    case P(H) of
        true  -> [H | filter(P, T)];
        false -> filter(P, T)
    end.

test_filter() ->
    [] = filter(fun(N) -> N rem 2 =:= 0 end, []),  % base case.
    [2,4,6] =
        filter(fun(N) -> N rem 2 =:= 0 end, [1,2,3,4,5,6]),
    tests_passed.

%% exercise 7 - split function
split(L) ->
    split_acc(L, [], []).

split_acc([H|T], Evens, Odds) ->
    case (H rem 2) of
        1 -> split_acc(T, Evens, [H|Odds]);
        0 -> split_acc(T, [H|Evens], Odds)
    end;
split_acc([], Evens, Odds) ->
    {lists:reverse(Evens), lists:reverse(Odds)}.

test_split() ->
    {[],[]} = split([]),
    {[2,4,6], []} = split([2,4,6]),
    {[], [1,3,5]} = split([1,3,5]),
    {[2,4,6], [1,3,5]} = split([1,2,3,4,5,6]),
    tests_passed.
