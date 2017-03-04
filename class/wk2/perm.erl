
-module(perm).

-import(lists,[map/2,seq/2,flatmap/2,split/2]).
-export([perms0/1,perms1/1,take_nth/2]).


perms0([]) -> [[]];
perms0(L)  -> [[H|T] || H <- L, T <- perms0(L--[H])].
    

take_nth(N,Xs) ->
    {L,[R|Rs]} = split(N,Xs),
    {R,L ++ Rs}.

perms1([]) ->
    [[]];

perms1(Xs) ->
    S = map(fun(N) -> take_nth(N,Xs) end, seq(0, length(Xs)-1)),
    flatmap(fun({E,L}) -> map(fun(P) -> [E | P] end, perms1(L)) end, S).
