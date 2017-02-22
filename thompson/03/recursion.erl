%% recursion practice.

-module(recursion).

-export([bump/1, average/1, sum/1, len/1, even/1, member/2,
         sum_tr/1, bump_tr/1, merge/2, average_tr/1]).

-include_lib("eunit/include/eunit.hrl").


bump([])    -> [];  % base case
bump([H|T]) -> [H+1 | bump(T)].

bump_test_() ->
    [?_assertEqual([], bump([])),  % base case
     ?_assertEqual([2], bump([1])),
     ?_assertEqual([2,3,4], bump([1,2,3]))].



average(List) ->
    case N = len(List) of
        0 -> 0.0;
        _ ->  sum(List) / N
    end.

average_test_() ->
    [?_assertEqual(0.0, average([])),
     ?_assertEqual(1.0, average([1])),
     ?_assertEqual(2.0, average([1,2,3]))].


sum([])    -> 0.0;   % base case
sum([H|T]) -> H + sum(T).

sum_test_() ->
    [?_assertEqual(0.0, sum([])),    % base case
     ?_assertEqual(1.0, sum([1])),
     ?_assertEqual(6.0, sum([1,2,3]))].


len([])     -> 0;   % base case
len([_|T]) -> 1 + len(T).

len_test_() ->
    [?_assertEqual(0, len([])),   % base case
     ?_assertEqual(1, len([1])),
     ?_assertEqual(3, len([1,2,3]))].


even([])     -> [];  % base case
even([H|T]) when H rem 2 == 0 -> [H | even(T)];
even([_|T]) -> even(T).

even_test_() ->
    [?_assertEqual([], even([])),  % base case
     ?_assertEqual([], even([1,3,5])),
     ?_assertEqual([2,4,6], even([2,4,6])),
     ?_assertEqual([2,4,6], even([1,2,3,4,5,6]))].



member(_, [])    -> false;
member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T).

member_test_() ->
    [?_assertNot(member(1, [])),  % base case
     ?_assert(member(1, [1])),
     ?_assertNot(member(1, [2,2,2,2])),
     ?_assert(member(1, [2,2,1,2]))].


%% Tail recursion:

sum_tr(List) -> sum_acc(List, 0.0).


sum_tr_test_r_test_() ->
    [?_assertEqual(0.0, sum_tr([])),    % base case
     ?_assertEqual(1.0, sum_tr([1])),
     ?_assertEqual(6.0, sum_tr([1,2,3]))].


sum_acc([], Acc)    -> Acc;   % base case
sum_acc([H|T], Acc) -> sum_acc(T, H+Acc).


bump_tr(List) -> bump_acc(List, []).

bump_tr_test_() ->
    [?_assertEqual([], bump_tr([])),  % base case
     ?_assertEqual([2], bump_tr([1])),
     ?_assertEqual([2,3,4], bump_tr([1,2,3]))].

bump_acc([], Acc) -> lists:reverse(Acc);
bump_acc([H|T], Acc) -> bump_acc(T, [H+1 | Acc]).


%% indirectly tail recursive

merge(Xs,Ys) ->
    lists:reverse(mergeL(Xs,Ys,[])).

mergeL([X|Xs],Ys,Zs) ->
    mergeR(Xs,Ys,[X|Zs]);
mergeL([],[],Zs) ->
    Zs.

mergeR(Xs,[Y|Ys],Zs) ->
    mergeL(Xs,Ys,[Y|Zs]);
mergeR([],[],Zs) ->
    Zs.

merge_test_() ->
    [?_assertEqual([], merge([],[])),
     ?_assertEqual([1,2,3,4,5,6], merge([1,3,5],[2,4,6]))].



%% two accumulators

average_tr(List) -> average_acc(List, 0.0, 0.0).

average_acc([], Sum, Length) ->
    Sum / Length;
average_acc([H | T], Sum, Length) ->
    average_acc(T, Sum + H, Length + 1).

average_tr_test_() ->
    [%?_assertEqual(0.0, average_tr([])),
     ?_assertEqual(1.0, average_tr([1])),
     ?_assertEqual(2.0, average_tr([1,2,3]))].
