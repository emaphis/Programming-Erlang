-module(fold).

%% folding experiments

-export([sum_r/2, sum_l/2, foldr/3, foldl/3,
         sum0/2, sum1/2, sum2/2, test/0]).

plus(X, Acc) -> X+Acc.

%% direct recursive sum. pattern for fold right
sum_r([], Acc)     -> Acc ;
sum_r([X|Xs], Acc) ->
    plus(X, sum_r(Xs, Acc)) .

%% tail-call recursive sum. pattern for fold left
sum_l([], Acc)     -> Acc ;
sum_l([X|Xs], Acc) ->
    sum_l(Xs, plus(X, Acc)).


%% fold right - direct recursive
%% 'starting value'(Acc) and 'combinding function' (Fn)
foldr(_Fn, [], Acc)    -> Acc;
foldr(Fn, [X|Xs], Acc) ->
    Fn(X, foldr(Fn, Xs, Acc)).

%% fold left - tail-call recursive
foldl(_Fn, [], Acc)    -> Acc;
foldl(Fn, [X|Xs], Acc) ->
    foldl(Fn, Xs, Fn(X, Acc)).

sum0(Xs, Acc) -> foldr(fun plus/2, Xs, Acc).

sum1(Xs, Acc) -> foldl(fun plus/2, Xs, Acc).

sum2(Xs, Acc) -> foldl(fun (X,Y) -> X+Y end, Xs, Acc).


test() ->
    0 = sum_r([],0),
    6 = sum_r([1,2,3],0),

    0 = sum_l([],0),
    6 = sum_l([1,2,3],0),

    0 = sum0([],0),
    6 = sum0([1,2,3],0),

    0 = sum1([],0),
    6 = sum1([1,2,3],0),

    0 = sum2([],0),
    6 = sum2([1,2,3],0),
    ok.
