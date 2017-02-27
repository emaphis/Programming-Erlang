%% shapes project

-module(shapes).

-export([area/1, perimeter/1, enclose/1, bits_d/1, bits_tc/1]).
-export([area_test/0, perimeter_test/0, bits_d_test/0, bits_tc_test/0]).

%% geometric shapes
%% {circle, R}           % radius
%% {rectangle, H, W}.    % height, width
%% {triangle, A, B, C}   % sides


area({circle, R}) ->
    math:pi() * R * R;
area({rectangle, H, W}) ->
    H*W;
area({triangle, A, B, C}) ->   % using Heron's Formula
    S = (A + B + C) / 2,
    math:sqrt(S * (S-A) * (S-B) * (S-C)).


area_test() ->
    28.274333882308138 = shapes:area({circle, 3.0}),
    12.0 = area({rectangle, 3.0, 4.0}),
    24.0 = area({triangle, 4.0, 13.0, 15.0}),
    ok.


%% calculate the perimeter given a shape tuple

perimeter({circle, R}) ->
    2.0 * math:pi() * R;
perimeter({rectangle, H, W}) ->
    2.0 * H * W;
perimeter({triangle, A, B, C}) ->
    A + B + C.

perimeter_test() ->
    25.132741228718345 = perimeter({circle, 4.0}),
    24.0 = perimeter({rectangle, 3.0, 4.0}),
    12.0 = perimeter({triangle, 3.0, 4.0, 5.0}),
    ok.


%% enclose a shape in a rectangle.
%% https://en.wikipedia.org/wiki/Minimum_bounding_box_algorithms

enclose({circle, R}) ->
    {rectangle, 2*R, 2*R};

enclose({rectangle, H, W}) ->
    {rectangle, H, W};

enclose({triangle, A, B, C}) ->
    {rectangle, A, triangleHeight(A,B,C)}.

triangleHeight(Base, S1, S2) ->
    2 * area({triangle, Base, S1, S2}) / Base.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% count the ones in a binary number:
%%
%% erlang has bit oporators,
%% look up 'band' 'bor' 'bxor' 'bsl' 'bsr'.
%% http://erlang.org/doc/reference_manual/expressions.html#id78950
%%
%% which means that I can use...
%% ...Kernighan's algorithm:
%% http://stackoverflow.com/questions/12380478/

bits_d(0) ->
    0;
bits_d(N) ->
    1 + bits_d(N band (N-1)).

bits_d_test() ->
    0 = bits_d(2#0),    % base
    1 = bits_d(2#1),
    1 = bits_d(2#10),
    2 = bits_d(2#11),
    3 = bits_d(2#111),  % test 7
    1 = bits_d(2#1000), % test 8
    bits_d_passed.


%% simple tail call fix.
bits_tc(N) ->
    bits_tc_acc(N, 0).

bits_tc_acc(0, Acc) ->
    Acc;
bits_tc_acc(N, Acc) ->
    bits_tc_acc(N band (N-1), Acc+1).

bits_tc_test() ->
    0 = bits_tc(0), % base
    1 = bits_tc(1),
    1 = bits_tc(2),
    2 = bits_tc(3),
    3 = bits_tc(7), % test 2#111
    1 = bits_tc(8), % test 2#1000
    bits_tc_passed.
