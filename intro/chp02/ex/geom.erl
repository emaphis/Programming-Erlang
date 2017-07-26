%%%--------------------------------------------------------------
%%% @author Ed Maphis <emaphis85@gmail.com>
%%% @copyright (C) 2017, Ed Maphis
%%% @version 0.1
%%% @doc Exercise 2.1 - Functions to calulate the area of various
%%% shapes.
%%% @end
%%%-------------------------------------------------------------
-module(geom).
-export([area/2]).

%%% @doc Calculate the area of a rectangle
%%% given length and width returning the produce
%%% of it's arguments.

-spec area(number(),number()) -> number().

area(Length, Width) -> Length * Width.
