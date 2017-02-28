-module(shapes).

-export([circles/1, sum/1, total_area/1]).
-export([area/1]).
-export([area_test/0, total_area_test/0]).

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

%% like accumulate
sum(Ns) -> sum(Ns,0.0).
sum([],A)-> A;
sum([N|Ns],A) -> sum(Ns,N+A).

total_area(Shapes) ->
    AllAreas = all_areas(Shapes),
    sum(AllAreas).

%% like map
all_areas([]) -> [];
all_areas([X|Xs]) ->
    [area(X) | all_areas(Xs)].

total_area_test() ->
    0.0 = total_area([]),
    18.566370614359172 = total_area([{circle,2}, {rectangle,3,2}]),
    ok.

%% like filter
circles([])     -> [] ;
circles( [X | Xs] ) ->
    case X of
	{circle,{_,_},_}=C ->
	    [ C | circles(Xs) ];
	_ ->
	    circles(Xs)
end.
