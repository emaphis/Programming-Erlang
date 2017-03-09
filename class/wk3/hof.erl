%% higher order functions  --  3.10, 3.11
%% functions as results.

-module(hof).

-export([add/1,add3/1,add_one_to_all/1,add_to_all/2,compose/2]).

-include_lib("eunit/include/eunit.hrl").


%% partialy applied functions.

add(X) ->
    fun(Y) -> X+Y end.

add3(Y) -> (add(3))(Y).

add_one_to_all(Xs) ->
    lists:map(add(1), Xs).

add_to_all(N,Xs) ->
    lists:map(add(N), Xs).


add_test() ->
    ?assertEqual(7, add3(4)),
    ?assertEqual([2,3,4], add_one_to_all([1,2,3])),
    ?assertEqual([2,3,4], add_to_all(1,[1,2,3])),
    ?assertEqual([3,4,5], add_to_all(2, [1,2,3])).


%% composing functions, take functions as arguments and
%% return functions

compose(F, G) ->
    fun(X) -> G(F(X)) end.

