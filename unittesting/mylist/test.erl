%% four levels of testing.

-module(test).
-export([take/2, test1/0, test2/0, initial/2, drop/2]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


take(0,_)     -> [];
take(_,[])    -> [];
take(N,[X|Xs]) -> [X | take(N-1,Xs)].


%% testing as we go in the REPL.
% ...

%% adhoc functions


test1() -> take(0,[]) == [].

test2() -> take(0,[3,4,5]) == [].


%% using a test framework.

take_1_test() -> ?assertEqual(take(0,[]),[]).

take_2_test() -> ?assertEqual(take(0,[3,4,5]),[]).

take_3_test() -> ?assertEqual(take(1,[3,4,5]), [3]).


%% property based testing

prop_take2() ->
    ?FORALL(N, nat(),
            ?FORALL(Xs, list(nat()), length(take(N,Xs)) == N)).

prop_take3() ->
    ?FORALL(N, nat(),
            ?FORALL(Xs, list(nat()), length(take(N,Xs)) == min(N,length(Xs)))).


%% Define a property: normal Erlang definition:

initial([],_)          -> true;
initial([X|Xs],[X|Ys]) -> initial(Xs,Ys);
initial(_,_)           -> false.

prop_initial() ->
    ?FORALL(Xs, list(nat()),
            ?FORALL(Ys, list(nat()), initial(Xs, Xs++Ys))).

prop_initial2() ->
    ?FORALL(Xs, list(nat()),
            ?FORALL(Ys, list(nat()), initial(Xs, Ys++Xs))).


% take gives an initial segment.
prop_initial_take() ->
    ?FORALL(N, nat(),
            ?FORALL(Xs, list(nat()), initial(take(N,Xs),Xs))).


% relating take and drop

drop(_,[])     -> [];
drop(0,Xs)     -> Xs;
drop(N,[_|Xs]) -> drop(N-1,Xs).

prop_take_drop() ->
    ?FORALL(N, nat(),
            ?FORALL(Xs, list(nat()), take(N,Xs)++drop(N,Xs) == Xs)).
