%%% a first module.
%%% calculate the area of several geometric figures.

-module(geometry).
-export([area/1, test/0]).

area({rectangle, Width, Height}) ->
    Width * Height;
area({square, Side}) ->
    Side * Side.


%% testing function:

test() ->
    12  = area({rectangle, 3, 4}),
    144 = area({square, 12}),
    tests_worked.
