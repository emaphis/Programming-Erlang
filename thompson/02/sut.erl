-module(sut).
-export([double/1, triple/1, my_reverse/1]).


double(X) ->
    X * 2.

triple(X) ->
    X * 3.

my_reverse(X) ->
    my_reverse(X,[]).
my_reverse([], Acc) ->
    Acc;
my_reverse([Head | Tail], Acc) ->
    my_reverse(Tail, [Head | Acc]).

