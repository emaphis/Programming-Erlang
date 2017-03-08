%% higher-order function practice -- 3.5
-module(practice).

-export([doubleAll/1, evens/1, product/1, zip/2]).
-export([doubleAll_h/1, evens_h/1, product_h/1]).
-export([zip_with/3, zip_with2/3, zip2/2]).

-include_lib("eunit/include/eunit.hrl").


-spec doubleAll([number()]) -> [number()].
doubleAll([]) -> [];
doubleAll([X|Xs]) ->
    [ 2*X | doubleAll(Xs) ].

-spec doubleAll_h([number()]) -> [number()].
doubleAll_h(Xs) -> lists:map(fun (N) -> 2*N end, Xs).

doubleAll_test() ->
    ?assertEqual([], doubleAll([])),
    ?assertEqual([2,4,6], doubleAll([1,2,3])),
    ?assertEqual([], doubleAll_h([])),
    ?assertEqual([2,4,6], doubleAll_h([1,2,3])).


-spec evens([integer()]) -> [integer()].
evens([]) -> [];
evens([X|Xs]) when X rem 2 == 0 ->
    [X | evens(Xs) ];
evens([_|Xs]) ->
    evens(Xs).


-spec evens_h([integer()]) -> [integer()].
evens_h(Xs) -> lists:filter(fun (N) -> N rem 2 == 0 end, Xs).

evens_test() ->
    ?assertEqual([], evens([])),
    ?assertEqual([2,4,6], evens([1,2,3,4,5,6])),
    ?assertEqual([], evens_h([])),
    ?assertEqual([2,4,6], evens_h([1,2,3,4,5,6])).


-spec product([number()]) -> number().
product([]) -> 1;
product([X|Xs]) -> X * product(Xs).


-spec product_h([number()]) -> number().
product_h(Xs) -> lists:foldr(fun (N,M) -> N*M end, 1, Xs).

product_test() ->
    ?assertEqual(1, product([])),
    ?assertEqual(6, product([1,2,3])),
    ?assertEqual(1, product_h([])),
    ?assertEqual(6, product_h([1,2,3])).



%% recursive zip function

-spec zip([A],[B]) -> [{A,B}].
zip([],_) -> [];
zip(_,[]) -> [];
zip([X|Xs],[Y|Ys]) ->
    [{X,Y} | zip(Xs,Ys)].

zip_test() ->
    ?assertEqual([], zip([1,3,5],[])),
    ?assertEqual([], zip([], [2,4,6])),
    ?assertEqual([{1,2}], zip([1],[2])),
    ?assertEqual([{1,2},{3,4}], zip([1,3],[2,4])),
    ?assertEqual([{1,2},{3,4}], zip([1,3],[2,4,6,8])).


%% higher order zip that takes a function to combine two lists
-spec zip_with(fun((A,B) -> [C]),[A],[B]) -> [C].
zip_with(_F,_,[]) -> [];
zip_with(_F,[],_) -> [];
zip_with(F,[X|Xs],[Y|Ys]) ->
    [F(X,Y) | zip_with(F,Xs,Ys)].

zip_with_test() ->
    ?assertEqual([], zip_with(fun(X,Y) -> X + Y end,[1,3,5],[])),
    ?assertEqual([], zip_with(fun(X,Y) -> X + Y end,[], [2,4,6])),
    ?assertEqual([3], zip_with(fun(X,Y) -> X + Y end,[1],[2])),
    ?assertEqual([3,7], zip_with(fun(X,Y) -> X + Y end,[1,3],[2,4])),
    ?assertEqual([3,7], zip_with(fun(X,Y) -> X + Y end,[1,3,5,7],[2,4])),
    ?assertEqual([3,7], zip_with(fun(X,Y) -> X + Y end,[1,3],[2,4,6,8])),
    ?assertEqual([2,12], zip_with(fun(X,Y) -> X * Y end,[1,3],[2,4,6,8])).


%% redifine zip_with using map and zip
-spec zip_with2(fun(({A,B}) -> [C]),[A],[B]) -> [C].
zip_with2(F,Xs,Ys) -> lists:map(F, zip(Xs,Ys)).

zip_with2_test() ->
    ?assertEqual([], zip_with2(fun({X,Y}) -> X + Y end,[1,3,5],[])),
    ?assertEqual([], zip_with2(fun({X,Y}) -> X + Y end,[], [2,4,6])),
    ?assertEqual([3], zip_with2(fun({X,Y}) -> X + Y end,[1],[2])),
    ?assertEqual([3,7], zip_with2(fun({X,Y}) -> X + Y end,[1,3],[2,4])),
    ?assertEqual([3,7], zip_with2(fun({X,Y}) -> X + Y end,[1,3,5,7],[2,4])),
    ?assertEqual([3,7], zip_with2(fun({X,Y}) -> X + Y end,[1,3],[2,4,6,8])),
    ?assertEqual([2,12], zip_with2(fun({X,Y}) -> X * Y end,[1,3],[2,4,6,8])).


%% redfine zip using zip_with
-spec zip2([A],[B]) -> [{A,B}].
zip2(Xs,Ys) -> zip_with(fun (X,Y) -> {X,Y} end, Xs, Ys).

zip2_test() ->
    ?assertEqual([], zip2([1,3,5],[])),
    ?assertEqual([], zip2([], [2,4,6])),
    ?assertEqual([{1,2}], zip2([1],[2])),
    ?assertEqual([{1,2},{3,4}], zip2([1,3],[2,4])),
    ?assertEqual([{1,2},{3,4}], zip2([1,3],[2,4,6,8])).
