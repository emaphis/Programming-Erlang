%%% a first module.
%%% calculate the area of several geometric figures.

-module(geometry).
-export([area/1, perimeter/1]).
-export([test_area/0, test_perimeter/0]).

area({rectangle, Width, Heigth}) ->
    Width * Heigth;
area({square, Side}) ->
    Side * Side;
%% exercise 1.
area({circle, Radius}) ->
    3.14158 * Radius * Radius;
area({rt_triangle, Width, Heigth}) ->
    Width * Heigth / 2.0.


%% testing function:

test_area() ->
    12   = area({rectangle, 3, 4}),
    144  = area({square, 12}),
    %%   true = 314.158 =:= area({circle, 10}),  % no floating point compare
    6.0  = area({rt_triangle, 3, 4}),
    tests_pass.


%% exercise 1

hypotenuse(A, B) ->
    math:sqrt(A * A + B * B).

perimeter({rectangle, Width, Heigth}) ->
    2 * (Width + Heigth);
perimeter({square, Side}) ->
    4 * Side;
perimeter({circle, Radius}) ->
    2 * 3.14158 * Radius;
perimeter({rt_triangle, Width, Heigth}) ->
    Width + Heigth + hypotenuse(Width, Heigth).

test_perimeter() ->
    14  = perimeter({rectangle, 3, 4}),
    48  = perimeter({square, 12}),
    %% circle
    12.0 = perimeter({rt_triangle, 3, 4}),
    tests_pass.
