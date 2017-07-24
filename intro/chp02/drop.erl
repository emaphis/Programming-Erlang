%%%------------------------------------------------------------
%%% @author Ed Maphis <emaphis85@gmail.com>
%%% @copyright (C) 2017, Ed Maphis
%%% @doc Functions calculating velocities achieved by objects
%%% dropped in a vacuum
%%% @version 0.1
%%% @end
%%%-------------------------------------------------------------
-module(drop).
-export([fall_velocity/1]).

%%--------------------------------------------------------------
%% @doc Calulates the velocity of an object falling on Earth
%% as if it were in a vacuume (no air resistance). The distance is
%% the height from which the object falls, specified in meters,
%% and the function returs a velelocity in meters per second
%% @end
%%--------------------------------------------------------------

-spec fall_velocity(number()) -> number().

fall_velocity(Distance) ->
    math:sqrt(2 * 9.8 * Distance).
