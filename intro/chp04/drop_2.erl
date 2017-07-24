%% chapter 4 - Using the return value from the case statent.
-module(drop_2).
-export([fall_velocity/2]).


fall_velocity(Planemo, Distance) when Distance >= 0 ->
    Gravity = case Planemo of
        earth -> 9.8;
        moon  -> 1.6;
        mars  -> 3.71
    end,
    math:sqrt(2 * Gravity * Distance).
