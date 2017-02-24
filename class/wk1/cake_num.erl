%% calculate the number of piece an object can be cut into.

-module(cake_num).

-export([pieces/1, dem_1/1, dem_2/1, dem_3/1, dem_4/1]).
-export([pieces_test/0, dem_0_test/0, dem_1_test/0, dem_2_test/0, dem_3_test/0, dem_4_test/0 ]).

pieces(0) ->
    1;
pieces(N) when N > 0 ->
     pieces(N-1) + N.

pieces_test() ->
     1 = pieces(0), % base case.
     2 = pieces(1),
     4 = pieces(2),
     7 = pieces(3),
    11 = pieces(4),
    16 = pieces(5),
    pieces_passed.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% solving plane cut division for a cube
%% I couldn't figure this out myself, so I looked up online.
%% http://mathmidway.org/math-midway-puzzles-century1.php
%% This site suggested subtracting each succeeding number from the previous
%% and looking for a pattern. (I really should take a course on discrete math)
%%
%% Calculating each proceding dimension from the third
%% should produce:
%%  1,  2,  4,  8, 15, 26, 42, 64, 93, 130, ...  - third dimension, cake numbers
%%    1,  2,  4,  7, 11, 16, 22, 29, 37, ...    - second dimension, pizza numbers
%%      1,  2,  3,  4,  5,  6,  7,  8, ...     - first dimension, string numbers
%%        1,  1, 1,  1,  1,  1,   1, ...      - zeroth dimension, atomic
%%          0,  0, 0,  0,  0,  0, ...       - uhm.
%% also from: http://mathworld.wolfram.com/SpaceDivisionbyPlanes.html

%% This suggests to calculate any number for any dimension you take a previous number,
%% and add the corresponding number from the next lower dimension.

%% So lets start with the base dimension:

%% solve for 0 demension
%% no cuts are possible, atomic:  1,1,1,1,1,.....
dem_0(0) ->
    1;
dem_0(N) ->
    dem_0(N-1) + 0.

dem_0_test() ->
    1 = dem_0(0),
    1 = dem_0(1),
    1 = dem_0(2),
    dem_0_passes.

%% solve one 1 demension.
%% each cut add one  1,2,3,4,5,...    (spaghetti numbers?)
dem_1(0) ->
    1;
dem_1(N) ->
    dem_1(N-1) + dem_0(N-1).

dem_1_test() ->
    1 = dem_1(0), % base case: 0 cuts always produce 1 piece
    2 = dem_1(1), % 1 cut always produses 2 pieces.
    3 = dem_1(2),
    dem_1_passes.

%% solve for 2 dimensions, pizza numbers
%%  should produce same output as 'piece/1'
%% 1, 2, 4, 7, 11, 16, 22, ...
dem_2(0) ->
    1;
dem_2(N) ->
    dem_2(N-1) + dem_1(N-1).

dem_2_test() ->
     1 = dem_2(0), % base case: 0 cuts, 1 piece.
     2 = dem_2(1), % 1 cut, 2 pieces.
     4 = dem_2(2),
     7 = dem_2(3),
    11 = dem_2(4),
    16 = dem_2(5),
    dem_2_passes.

%% solve for 3 dimensions - produces 'cake' numbers:
%%  1, 2, 4, 8, 15, 26, ...
dem_3(0) ->
    1;
dem_3(N) ->
    dem_3(N-1) + dem_2(N-1).

dem_3_test() ->
     1 = dem_3(0),
     2 = dem_3(1),
     4 = dem_3(2),
     8 = dem_3(3),
    15 = dem_3(4),
    26 = dem_3(5),
    dem_3_passed.

%% solve for 4 dimensions.
%% 4 dimensional food?  hyper-pumkin numbers?
%% 1, 2, 4, 8, 16, 31, ...
dem_4(0) ->
    1;
dem_4(N) ->
    dem_4(N-1) + dem_3(N-1).

dem_4_test() ->
     1 = dem_4(0),
     2 = dem_4(1),
     4 = dem_4(2),
     8 = dem_4(3),
    16 = dem_4(4),
    31 = dem_4(5), % that would feed a lot of people!
    dem_4_passes.


%% I don't know (yet) how to write a completly general solution using functions.
