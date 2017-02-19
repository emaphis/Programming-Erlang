%% conditional expressions

-module(condition).
-export([listlen1/1,listlen/1,index/2]).
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


unsafe(X) ->
    case X of
        one -> Y = true;
        _   -> Z = two
    end,
    Y.
