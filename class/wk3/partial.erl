%% Funtions as results homework - 3.11

-module(partial).

-export([comp/2,
         comp_all_r/1, comp_all/1,
         twice/1, quad/1,
         iteration/2, iteration2/2]).

-include_lib("eunit/include/eunit.hrl").

%% shell examples recast as a unit test.
shell_test() ->
    Add = fun (X,Y) -> X+Y end,
    3  = Add(1,2),
    7  = Add(3,4),

    Sum = fun (Xs) -> lists:foldr(Add,0,Xs) end,
    74 = Sum([3,4,67]),
    10 = Sum([1,2,3,4]),

    Double = fun (Xs) -> lists:map(fun (X) -> 2*X end, Xs) end,
    [2,4,6,8] = Double([1,2,3,4]),  % who do we appreciate?

    %% fun exressions can use pattern matching
    EmptyTest = fun ([]) -> true ; ([_|_]) -> false end,
    true  = EmptyTest([]),
    false = EmptyTest([1,2,3,4]),

    %% fun expressios can be recursive.
    Foo = fun Product([]) -> 1; Product([X|Xs]) -> X*Product(Xs) end,
    0   = Foo([0,1,2]),
    20  = Foo([10,1,2]),

    %% defined and appied in same expressio.
    10  = fun (X) -> X+1 end (9),
    Inc = fun (X) -> X+1 end,
    10  = Inc(9),

    %% twice the fun.
    IncBy =  fun (X) -> (fun (Y) -> X+Y end) end,
    IncBy3 = IncBy(3),
    7 = IncBy3(4).



%% compostion
-spec comp(fun((B) -> C), fun((A) -> B)) -> fun((A) -> C).
comp(F, G) ->
    fun (X) ->  G(F(X)) end.


comp_test() ->
    Add3 = fun (X) -> 3+X end,
    Add4 = fun (X) -> 4+X end,
    Mult3 = fun (X) -> 3*X end,

    %% add is commutative:
    ?assertEqual(12, Add3(Add4(5))),
    ?assertEqual(12, Add4(Add3(5))),

    ?assertEqual(12, (comp(Add3, Add4)) (5)),
    ?assertEqual(12, (comp(Add4, Add3)) (5)),

    %% Non commutative example:
    ?assertEqual(9,  Add3(Mult3(2))), % non commutative
    ?assertEqual(15, Mult3(Add3(2))),

    ?assertEqual(9,  (comp(Mult3, Add3)) (2)),
    ?assertEqual(15, (comp(Add3, Mult3)) (2)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given a list of functions,  return a new function that composes
%% those functions, 'reduces' or 'folds' those functions into an
%% accumulation of nested function calls. Applying to an empty list
%% will return the id function for no effect.

%% SO:
%% comp_all([]) (X)  -> id(X)
%% comp_all([H,G,F]) (X) -> id(F(G(H(X))))

%% [Fn | FNs]  -- list of functions.
%% 'Acc'  -- accumulation of nested function calls.

%% an identity function for comp_all base case.
-spec id(T) -> T.
id(I) -> I.

%% comp all, compose a list of functions  -- recursive
-spec comp_all_r([fun((T) -> T)]) -> fun((T) -> T).
comp_all_r(FNs) ->
    comp_all_r(fun id/1, FNs).

comp_all_r(Acc, []) -> Acc;
comp_all_r(Acc, [FN|FNs]) ->
    comp_all_r(comp(FN, Acc), FNs).


%% comp all  -- higher order using fold and predefined comp.
-spec comp_all([fun((T) -> T)]) -> fun((T) -> T).
comp_all(FNs)->
    lists:foldl(fun comp/2, fun id/1, FNs).


comp_all_test() ->
    Add3 = fun (X) -> X+3 end,
    Mult3 = fun (X) -> X*3 end,
    ?assertEqual(15, Mult3(Add3(2))),

    ?assertEqual(15, (comp(Add3, Mult3)) (2)),

    ?assertEqual(15, (comp_all_r([Mult3, Add3])) (2)),
    ?assertEqual(15, (comp_all([Mult3, Add3])) (2)),

    ?assertEqual(18, (comp_all([Mult3, Add3, fun(X) -> X+1 end])) (2)),

    ?assertEqual(2,  (comp_all_r([])) (2)),    % base case -- id(2) -> 2
    ?assertEqual(2,  (comp_all([])) (2)).      % base case.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% twice/2 -- apply a function to an argrument twice.
%% twice(F) ->  F(F())
-spec twice(fun((T) -> T)) -> fun((T) -> T).
twice(Fn) -> comp(Fn,Fn).

%% quad/1 -- apply twice twice
-spec quad(fun((T) -> T)) -> fun((T) -> T).
quad(Fn) -> twice(twice(Fn)).

twice_test() ->
    Mult3 = fun (X) -> 3*X end,
    ?assertEqual(6, Mult3(2)),
    ?assertEqual(18, (twice(Mult3)) (2)),

    ?assertEqual(162, (quad(Mult3)) (2)).


%% iteration/2 -- recursive
%% applies a given function (Fn) a given number (N) of times.
%% the function parameter acts as the body of a loop.
-spec iteration(fun((T) -> T), non_neg_integer()) -> fun((T) -> T).
iteration(Fn, N) ->
    iteration(Fn, fun id/1, N).

iteration(_, Acc, 0) -> Acc;
iteration(Fn, Acc, N) ->
    iteration(Fn, comp(Fn, Acc), N-1).

%% iteration using foldr
iteration2(Fn, N) ->
    lists:foldr(fun comp/2, fun id/1, lists:duplicate(N, Fn)).

iteration_test() ->
    Mult3 = fun (X) -> X*3 end,

    ?assertEqual(2,  (iteration(Mult3, 0)) (2)),  % base case

    ?assertEqual(18, (comp(Mult3,Mult3)) (2)),  % compare
    ?assertEqual(18, (iteration(Mult3, 2)) (2)),
    ?assertEqual(54, (iteration(Mult3, 3)) (2)),
    
    ?assertEqual(2,  (iteration2(Mult3, 0)) (2)),  % base case
    ?assertEqual(18, (iteration2(Mult3, 2)) (2)),
    ?assertEqual(54, (iteration2(Mult3, 3)) (2)).

