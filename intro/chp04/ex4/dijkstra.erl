%%%--------------------------------------------------------------
%%% @author Ed Maphis <emaphis85@gmail.com>
%%% @copyright (C) 2017, Ed Maphis
%%% @version 0.1
%%% @doc Ex 4.2 - GCD function using the dijkstra algorithm.
%%% Use the if expression.
%%% @end
%%%-------------------------------------------------------------
-module(dijkstra).
-export([gcd/2]).


%%% @doc Calculates the greates common divisor of two numbers
%%% using the dijkstra algorithm.

-spec(gcd(number(),number()) -> number()).

gcd(M,N) ->
    if
        M == N  -> M;
        M > N   -> gcd(M-N, N);
        true    -> gcd(M,N-M)
    end.
