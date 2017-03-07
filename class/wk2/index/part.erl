-module(part).
-compile(export_all).


-type line_no() :: any().
-type range() :: {line_no(), line_no()}.


%% partition a list of numbers into a list of Number Range Pairs.
%% See test function for examples.
-spec partition([line_no()]) -> [range()].
partition(Lst) -> partition(lists:sort(Lst), []).

-spec partition([line_no()],[range()]) -> [range()].
partition([], Acc) -> lists:reverse(Acc);
partition([X|Xs], [{Start}|Acc]) when X == Start+1 ->
	partition(Xs, [{Start,X}|Acc]);
partition([X|Xs], [{Start,X}|Acc]) ->
	partition(Xs, [{Start,X}|Acc]);
partition([X|Xs], [{Start,End}|Acc]) when X == End+1 ->
	partition(Xs, [{Start,X}|Acc]);
partition([X|Xs], Acc) ->
	partition(Xs, [{X,X}|Acc]).


partition_test() ->
    [{1,1}] = partition([1]),
    [{1,1}] = partition([1,1,1]),
    [{1,2}] = partition([2,1,1]),
    [{1,3}] = partition([3,2,2,1,1]),
    [{1,3},{5,5}] = partition([5,5,3,2,2,1]),
    [{1,1},{3,3},{5,7}] = partition([7,7,6,5,3,1,1]).
