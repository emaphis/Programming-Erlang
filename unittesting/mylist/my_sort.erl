% property based testing
-module(my_sort).
-export([sort/1,sortnd/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec sort([T]) -> [T].
sort([])     -> [];
sort([P|Xs]) ->
    sort([X || X <- Xs, X=<P]) ++ [P] ++ sort([X || X <- Xs, P < X]).


sort_test() ->
    [test_zero(), test_two(), test_four()].

test_zero() ->
    ?_assertEqual([], sort([])). % notice underscore
test_two() ->
    [?_assertEqual([17,42], sort([X,Y])) || {X,Y} <- [{17,42}, {42,17}]].
test_four() ->
    [?_assertEqual([1,2,3,4], sort([3,1,4,2]))].

prop_ordered() ->
    ?FORALL(L, list(integer()), ordered(sort(L))).

ordered([]) -> true;
ordered([_]) -> true;
ordered([A,B|T]) -> A =< B andalso ordered([B|T]).

prop_same_length() ->
    ?FORALL(L, list(integer()), length(L) =:= length(sort(L))).


% sort - no duplicates
-spec sortnd([T]) -> [T].
sortnd([])     -> [];
sortnd([P|Xs]) ->
    sortnd([X || X <- Xs, X<P]) ++ [P] ++ sortnd([X || X <- Xs, P < X]).


prop_same_length_conditional_check() ->
    ?FORALL(L, list(integer()),
            ?IMPLIES(no_duplicates(L), length(L) =:= length(sortnd(L)))).

no_duplicates([]) -> true;
no_duplicates([A|T]) ->
    not lists:member(A,T) andalso no_duplicates(T).


% custom generator
list_no_dupls(T) ->
    ?LET(L, list(T), remove_duplicates(L)).

remove_duplicates([]) -> [];
remove_duplicates([A|T]) ->
    case lists:member(A,T) of
        true  -> remove_duplicates(T);
        false -> [A|remove_duplicates(T)]
    end.

prop_same_length_no_dupls() ->
    ?FORALL(L, list_no_dupls(integer()), length(L) =:= length(sortnd(L))).


prop_equiv_usort() ->
    ?FORALL(L, list(integer()), sortnd(L) =:= lists:usort(L)).
