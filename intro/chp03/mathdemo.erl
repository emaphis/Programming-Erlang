%% Chapter 4 - guards.
-module(mathdemo).
-export([absolute_value/1]).

%% @doc Calculating the absolute value with guards
%% and pattern matching.
absolute_value(Number) when Number < 0 -> -Number;
absolute_value(0) -> 0;
absolute_value(Number) -> Number.
