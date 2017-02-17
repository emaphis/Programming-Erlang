-module(sut_tests).
-include_lib("eunit/include/eunit.hrl").

call_double_test() ->  sut:double(2).
double_2_test() -> 4 = sut:double(2).
double_4_test() -> 8 = sut:double(4).
%double_fail_test() -> 7 = sut:double(3).

% assert macros
triple_3_test() -> ?assert(sut:triple(3) =:= 9).
triple_fail_test() -> ?assertNot(sut:triple(3) =:= 10).

% test generators
double_gens_test_() ->
    [?_assert(sut:double(2) =:= 4),
     ?_assert(sut:double(3) =:= 6),
     ?_assert(sut:double(4) =:= 8),
     ?_assert(sut:double(5) =:= 10)].

% programatic test generators
double_gen_test_() ->
    [?_assert(sut:double(X) =:= X * 2) || X <- lists:seq(1, 10)].

reverse_gen2_test_() ->
    [?_assert(sut:my_reverse(List) =:= lists:reverse(List)) || List <- [lists:seq(1, Max) || Max <- lists:seq(1, 10)]].
