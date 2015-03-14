%% a 'for loop' in erlang

-module(lib_misc).
-export([for/3, qsort/1, pythag/1, perms/1]).
-export([test_for/0, test_qsort/0, test_pythag/0, test_perms/0]).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I) | for(I+1, Max, F)].

test_for() ->
    [1] = for(1, 1, fun(I) -> I end),  % base case.
    [1,2,3,4,5,6,7,8,9,10] =
        for(1, 10, fun(I) -> I end),
    [1,4,9,16,25,36,49,64,81,100] =
        lib_misc:for(1, 10, fun(I) -> I*I end),
    test_complete.


qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T, X >= Pivot]).

test_qsort() ->
    [2,6,9,14,23,27,45,61,78,82,400] =
        qsort([23,6,2,9,27,400,78,45,61,82,14]),
    test_complete.


pythag(N) ->
    [ {A,B,C} ||
        A <- lists:seq(1,N),
        B <- lists:seq(1,N),
        C <- lists:seq(1,N),
        A+B+C =< N,
        A*A+B*B =:= C*C
    ].

test_pythag() ->
    [{3,4,5},{4,3,5}] =
        pythag(16),
    [{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{12,5,13}] =
        pythag(30),
    test_complete.


perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

test_perms() ->
    [[]]  = perms(""),  % base case.
    ["123","132","213","231","312","321"] = perms("123"),
    ["cats", "cast", "ctas", "ctsa", "csat", "csta", "acts", "acst",
     "atcs", "atsc", "asct", "astc", "tcas", "tcsa", "tacs", "tasc",
     "tsca", "tsac", "scat", "scta", "sact", "satc", "stca", "stac"] =
        perms("cats"),
    test_complete.
