%%%--------------------------------------------------------------
%%% @author Ed Maphis <emaphis85@gmail.com>
%%% @copyright (C) 2017, Ed Maphis
%%% @version 0.1
%%% @doc Ex 4.3,4 - a powers function, and a root findeing function
%%% Non-tail recursive and tail recursive functions
%%% @end
%%%-------------------------------------------------------------
-module(powers).
-export([raise/2, raise_2/2]).

%% @doc Raise a number to a given power.

-spec(raise(number(), integer()) -> number()).

raise(_, 0) -> 1;
raise(X, 1) -> X; 
raise(X, N) when N > 0 -> X * raise(X, N-1);  
raise(X, N) when N < 0 -> 1.0 / raise(X, -N). 


%% @doc Raise a number to a given power.
%% Tail recursive version

-spec(raise_2(number(), integer()) -> number()).

raise_2(_, 0) -> 1;
raise_2(X, N) when N > 0 -> raise_2(X, N, 1);
raise_2(X, N) when N < 0 -> 1.0 / raise_2(X, -N). 

raise_2(_, 0, Acc) -> Acc;
raise_2(X, N, Acc)-> raise_2(X, N-1, X * Acc).  
