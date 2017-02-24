-module(patterns).

-export([is_zero/1, test_is_zero/0,
         xor1/2, test_xor1/0,
         xor2/2, test_xor2/0,
         xor3/2, test_xor3/0,
         xor4/2, test_xor4/0,
         xor5/2, test_xor5/0,
         xor6/2, test_xor6/0,
         max_three/3, test_max_three/0,
         how_many_equal/3, test_how_many_equal/0]).


%%% code from the lecture

is_zero(0) ->
    true;
is_zero(_) ->
    false.

test_is_zero()->
    true = is_zero(0),
    false = is_zero(1),
    false = is_zero($a),
    false = is_zero("0"),   % doesn't have to be an integer
    false = is_zero('0'),   % dynamic typing
    false = is_zero(fun(N) -> N+0 end),  % functions are data
    is_zero_passed.

%% exclusive 'or' only one of each.
xor1(true, false) ->
    true;
xor1(false,true) ->
    true;
xor1(_,_) ->   % don't care clause
    false.

test_xor1() ->
    false = xor1(true, true),
    false = xor1(false, false),
    true = xor1(true, false),
    true = xor1(false, true),
    xor1_passed.


%% checking equality between parts of a pattern.
xor2(X,X) ->
    false;
xor2(_,_) ->
    true.

test_xor2() ->
    false = xor2(true, true),
    false = xor2(false, false),
    true = xor2(true, false),
    true = xor2(false, true),
    xor2_passed.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% homework:

%% my 'xor's
xor3(X,Y) ->
    X =/= Y.

test_xor3() ->
    false = xor3(true, true),
    false = xor3(false, false),
    true = xor3(true, false),
    true = xor3(false, true),
    xor3_passed.


xor4(X,Y) ->
    not (X == Y).

test_xor4() ->
    false = xor4(true, true),
    false = xor4(false, false),
    true = xor4(true, false),
    true = xor4(false, true),
    xor4_passed.

xor5(X,Y) ->
    (X or Y) and not (X and Y).

test_xor5() ->
    false = xor5(true, true),
    false = xor5(false, false),
    true = xor5(true, false),
    true = xor5(false, true),
    xor5_passed.

xor6(X,Y)->
    (X and not Y) or (not X and Y).  % now my head hurts.

test_xor6() ->
    false = xor6(true, true),
    false = xor6(false, false),
    true = xor6(true, false),
    true = xor6(false, true),
    xor6_passed.


%% maximum_of_three
%% Oops, I couldn't think of a clever pattern matching solution.
max_three(X, Y, Z) ->
    max(X, max(Y, Z)).

test_max_three() ->
    3 = max_three(1,2,3),
    3 = max_three(1,3,2),
    3 = max_three(3,1,2),
    3 = max_three(3,3,3),
    36 = max_three(34,25,36),
    max_three_passed.


%% how_many_equal using pattern matching

how_many_equal(X,X,X) -> 3;
how_many_equal(X,X,_) -> 2;
how_many_equal(X,_,X) -> 2;
how_many_equal(_,X,X) -> 2;
how_many_equal(_,_,_) -> 0.

test_how_many_equal() ->
    3 = how_many_equal(1,1,1),
    2 = how_many_equal(1,1,0),
    2 = how_many_equal(1,0,1),
    2 = how_many_equal(0,1,1),
    0 = how_many_equal(0,1,2),
    how_many_equal_passed.
