%% pattern matching and recursion

-module(complex).

-export([fib/1,fibA/1]).
-export([fibA_test/0]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when n>0 ->
    fib(N-2).

%% adjacent pares.

fibP(0) ->
    {0,1};
fibP(N) ->
    {P,C}= fibP(N-1),
    {C,P+C}.

fibA(N) ->
    {P,_} = fibP(N),
    P.

fibA_test() ->
    0 = fibA(0),
    1 = fibA(1),
    1 = fibA(2),
    2 = fibA(3),
    3 = fibA(4),
    ok.



