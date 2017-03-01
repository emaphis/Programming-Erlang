%% nub definition - 2.13, 2.14

-module(nub).
-author('emaphis').

-export([nub/1, bun/1]).
-export([nub_test/0, bun_test/0]).

%% nub - the crux or central poin of a matter.

%% remove all duplicates from a list.
nub([]) -> [];
nub([X|Xs]) -> [X | nub(remove(X, Xs))].

remove(_, []) -> [];
remove(X, [X|Xs]) -> remove(X,Xs); 
remove(Y, [X|Xs]) -> [X | remove(Y,Xs)]. 

nub_test() ->
    [] = remove(1,[]),
    [2,3,4] = remove(1, [2,3,4]),
    [2,3,4] = remove(1, [1,2,3,4]),
    [2,3,4] = remove(1, [1,2,1,3,1,4,1]),
    [] = nub([]),
    [1] = nub([1,1,1]),
    [1,2] = nub([1,2,1,2,1,2]),
    [2,4,1,3] = nub([2,4,1,3,3,1]),
    %[2,4,3,1] = nub([2,4,1,3,3,1]),
    ok.


bun([]) -> [];
bun([X|Xs]) ->
    case member(X, Xs) of
        true  -> bun(Xs);
        false -> [X|bun(Xs)]
    end.

member(_, []) -> false;
member(X, [X|_]) -> true; 
member(X, [_|Xs]) -> member(X,Xs). 

bun_test() ->
    false = member(1,[]),
    false = member(1, [2,3,4]),
    true = member(1, [1,2,3,4]),
    true = member(1, [1,2,1,3,1,4,1]),
    [] = bun([]),
    [1] = bun([1,1,1]),
    [1,2] = bun([1,2,1,2,1,2]),
    %[2,4,1,3] = bun([2,4,1,3,3,1]),
    [2,4,3,1] = bun([2,4,1,3,3,1]),
    ok.

