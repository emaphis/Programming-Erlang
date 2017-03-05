%% experiments.

-module(stuff).
-export([nand1/2, nand2/2, nand3/2,  nanda/2, nand_test/0, square/2, merge/2]).
-export([foo/2, bar/2, baz/1]).

nand1(A,B) -> (not(A) or not(B)). % good
nand2(A,B) ->  not(A and B). % good
nand3(A,B) -> not(A andalso B). % good
nanda(A,B) -> (not(A) and B) or (not(B) and A). % bad

nand_test() ->
    true = nand1(false, false),
    true = nand1(false, true),
    true = nand1(true, false),
    false = nand1(true, true),
    ok.

%% are both arguement equal.
square(X,X) ->
    false;
square(_X,_Y) ->
    true.


merge([],Ys) -> Ys;
merge(Xs,[]) -> Xs;
merge([X|Xs],[Y|Ys]) when X<Y ->
    [ X | merge(Xs,[Y|Ys]) ];
merge([X|Xs],[Y|Ys]) when X>Y ->
    [ Y | merge([X|Xs],Ys) ];
merge([X|Xs],[_Y|Ys]) ->
    [ X | merge(Xs,Ys) ].


foo(_,[])              -> [];
foo(Y,[X|_]) when X==Y -> [X];
foo(Y,[X|Xs])          -> [X | foo(Y,Xs) ].
bar (N, [N]) ->
	[];
bar (_, [Y]) ->
 	[Y];
bar (N, [Y|Ys]) when N =/= Y ->
	[Y|bar (N, Ys)];
bar (_, [_|Ys]) ->
	Ys.



baz([])     -> [];
baz([X|Xs]) -> [X | baz(zab(X,Xs))].

zab(N,[])     -> [];
zab(N,[N|Xs]) -> zab(N,Xs);
zab(N,[X|Xs]) -> [X | zab(N,Xs)].
