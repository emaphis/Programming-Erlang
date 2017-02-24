%% tail-call recursion.

-module(tail).

-export([fac1/1, fac2/1, loop1/1, sum/1, max/1, fibo/1, is_perfect/1]).
-export([sum_test/0, fibo_test/0, is_perfect_test/0]).



%% direct recursion

fac1(0) ->
    1;
fac1(N) when N>0 ->
    fac1(N-1) * N.

fac2(N) ->
    fac2(N,1).


%% tail-call recursion.

fac2(0,P) ->
    P;
fac2(N,P) when N>0 ->
    fac2(N-1,P*N).


%% looping the loop
%% how to loop using recursion.
loop1(N) when N>0 ->
    io:format("~p~n", [N]),
    loop1(N-1);
loop1(_) ->
    io:format("bye~n").


%%%%%%%%%%%%%%%%%%%%%%%
%% examples to try

%% a simple function

%%  Int -> Int

f(N) -> N+1.

sum(N) -> sum(N,0).
sum(0,S) -> S;
sum(N,S) -> sum(N-1, S+f(N)).

sum_test() ->
    0 = sum(0),
    2 = sum(1),
    5 = sum(2),
    9 = sum(3),
    14 = sum(4),
    65 = sum(10),
    ok.



%% not sure how useful max is without having higher-order function yer, lol.
max(_N) -> no_op.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Home work

%% tail call fibonacci

fibo(N) ->
    fibo(N, 1, 0).

fibo(0, _A, B) ->
    B;
fibo(N, A, B) ->
    fibo(N-1, A+B, A).

fibo_test() ->
    0 = fibo(0),
    1 = fibo(1),
    1 = fibo(2),
    2 = fibo(3),
    3 = fibo(4),
    5 = fibo(5),
    8 = fibo(6),
    13 = fibo(7),
    fibo_passed.


%% calculating perfect numbers:
%% http://en.wikipedia.org/wiki/Perfect_number

%% return true if number is perfact
is_perfect(N) ->
    N == sum_factors(1, N, 0).

%% sum factors of a given number 'N'.
sum_factors(N, N, Acc) ->
    Acc;                   % Factor eq N, return sum.
sum_factors(Fac, N, Acc) when N rem Fac == 0 ->
    sum_factors(Fac+1, N, Acc+Fac);   % Accumulate then recure.
sum_factors(Fac, N, Acc) ->
    sum_factors(Fac+1, N, Acc).   % Increment Factor then recure.


is_perfect_test() ->
    true = is_perfect(6),
    true = is_perfect(28),
    true = is_perfect(496),
    true = is_perfect(8128),
    %% true = is_perfect(33550336),  % slow.
    false = is_perfect(4),
    false = is_perfect(30),
    false = is_perfect(500),
    false = is_perfect(8200),
    is_perfect_passed.
