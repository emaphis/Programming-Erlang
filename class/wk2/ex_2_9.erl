%% exercises 2.9 - constructing lists.

-module(ex_2_9).

-export([double/1, evens/1]).
-export([double_test/0, evens_test/0]).

%% double every item on a list
double([]) -> [];
double([X|Xs]) ->
    [2*X | double(Xs)].

double_test() ->
    [] = double([]),
    [2,4,6] = double([1,2,3]),
    ok.

%% return a list filtered for evens.
evens([]) -> [];
evens([X|Xs]) ->
    case X rem 2 == 0 of
        true  -> [X|evens(Xs)];
        false -> evens(Xs)
    end.

evens_test() ->
    [] = evens([]),
    [] = evens([1,3,5]),
    [2,4,6] = evens([2,4,6]),
    [2,4,6] = evens([1,2,3,4,5,6]),
    ok.


%% find the median of a list
