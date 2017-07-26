%%%--------------------------------------------------------------
%%% @author Ed Maphis <emaphis85@gmail.com>
%%% @copyright (C) 2017, Ed Maphis
%%% @version 0.1
%%% @doc Ex 3.4 Program to calulate the areas of geometric
%%% shapes. Use tuples as paramers.
%%% @end
%%%-------------------------------------------------------------
-module(geom_4).
-export([area/1]).

%%% @doc Calculate the area of a shape iven the shape tag and
%%% two of its demensions. returns the product of it's arguments.
%%% Ensure arguements are greater than or equal to 0, if not
%%% return 0.

-spec(area({atom(), number(),number()}) -> number()).

area({rectangle, L, W}) when L >= 0, W >= 0 -> L * W;
area({triangle, B, H}) when B >= 0, H >= 0 -> B * H / 2.0; 
area({ellipse, A, B}) when A >= 0, B >= 0 ->  math:pi() * A * B;
area({_, _, _}) -> 0.
