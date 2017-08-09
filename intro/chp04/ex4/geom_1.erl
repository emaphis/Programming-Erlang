%g%%--------------------------------------------------------------
%%% @author Ed Maphis <emaphis85@gmail.com>
%%% @copyright (C) 2017, Ed Maphis
%%% @version 0.1
%%% @doc Ex 4.1 - Functions to calulate the areas of geometric
%%% shapes. Use the case expression.
%%% @end
%%%-------------------------------------------------------------
-module(geom_1).
-export([area/3]).

%%% @doc Calculate the area of a shape iven the shape tag and
%%% two of its demensions. returns the product of it's arguments.
%%% Ensure arguements are greater than or equal to 0, if not
%%% return 0.

-spec(area(atom(), number(),number()) -> number()).

area(Shape, A, B) when A >= 0, B >= 0 ->
    case Shape of
        rectangle -> A * B;
        triangle  -> A * B / 2.0; 
        ellipse   -> math:pi() * A * B;
        _         -> 0
    end.
