%%%------------------------------------------------------------
%%% @author Ed Maphis <emaphis85@gmail.com>
%%% @copyright (C) 2017, Ed Maphis
%%% @doc Functions calculating velocities achieved by objects
%%% @version 0.1
%%% @end
%%%-------------------------------------------------------------

-module(combined).
-export([height_to_mph/1]).
-import(drop, [fall_velocity/1]).
-import(convert, [mps_to_mph/1]).

%% @doc Calculates the velocity of an object falling on Earth 
%% as if it was in a vacuum (no air resistance).  The distance is
%% the height from which the object falls, specified in meters,
%% and the function returns a velocity in miles per hour.

-spec(height_to_mph(number()) -> number()).

height_to_mph(Meters) ->
    mps_to_mph(fall_velocity(Meters)).
