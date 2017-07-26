%%%--------------------------------------------------------------
%%% @author Ed Maphis <emaphis85@gmail.com>
%%% @copyright (C) 2017, Ed Maphis
%%% @version 0.1
%%% @doc Ex 3.1 - Functions to calulate the areas of geometric
%%% shapes.
%%% @end
%%%-------------------------------------------------------------
-module(geom).
-export([area/3]).

%%% @doc Calculate the area of a shape iven the shape tag and
%%% two of its demensions. returns the product of it's arguments.

-spec(area(atom(), number(),number()) -> number()).

area(rectangle, L, W) -> L * W;
area(triangle, B, H) -> B * H / 2.0;
area(ellipse, A, B) -> math:pi() * A * B.
