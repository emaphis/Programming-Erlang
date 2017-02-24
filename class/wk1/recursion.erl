%% recursion on integers.

-module(recursion).

-export([fact/1, fibo/1, eval_fibo/0]).
-export([fact_test/0, fibo_test/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lecture
%%

%% factorial defined on natural numbers. let it fail.
fact(1) ->
    1;
fact(N) when N>0 ->
    fact(N-1) * N.

fact_test() ->
    1 = fact(1),
    6 = fact(3),
    24 = fact(4),
    120 = fact(5),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Homework
%%

%% Fibonacci numbers - defined on natural numbers.

fibo(0) ->
    0;
fibo(1) ->
    1;
fibo(N) when N>1 ->
    fibo(N-1) + fibo(N-2).

fibo_test() ->
    0 = fibo(0),
    1 = fibo(1),
    1 = fibo(2),
    2 = fibo(3),
    3 = fibo(4),
    ok.

% evaluate fibo(4):            fib(4)
%            fib(3)      +      fib(2)
%      fib(2)  +  fib(1) + fib(1) + fib(0)
%  fib(1) + fib(0) + 1   +   1    +   0
%     1   +   0    + 1   +   1    +   0
%     3


eval_fibo() ->
    3 = fibo(4),
    3 = fibo(3)                     + fibo(2),
    3 = fibo(2)           + fibo(1) + fibo(1) + fibo(0),
    3 = fibo(1) + fibo(0) + 1       + 1       + 0,
    3 = 1       + 0       + 1       + 1       + 0,
    3 = 3.
