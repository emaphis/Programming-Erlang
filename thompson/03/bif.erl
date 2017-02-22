%% BIF - Built in functions:

-module(bif).

-export([hello/0]).
-include_lib("eunit/include/eunit.hrl").

hello() ->
    io:format("hello, world!~n").

%% object access and examination

access_list_test_() ->
    List = [one,two,three,four,five],
    [ ?_assert(hd(List) == one),
      ?_assert(tl(List) == [two,three,four,five]),
      ?_assert(length(List) == 5),
      ?_assert(hd(tl(List)) == two) ].

access_tuple_test_() ->
    Tuple = {1,2,3,4,5},
    [ ?_assert(tuple_size(Tuple) == 5),
      ?_assert(element(2, Tuple) == 2),
      ?_assert(setelement(3, Tuple, three) == {1,2,three,4,5}),
      ?_assert(erlang:append_element(Tuple, 6) == {1,2,3,4,5,6}) ].


type_conversion_test_() ->
    [ ?_assert(atom_to_list(monday) == "monday"), 
      ?_assert(list_to_existing_atom("monday") == monday),
      ?_assert(list_to_tuple([one,two,three]) == {one,two,three}),
      ?_assert(float(1) == 1.0),
      ?_assert(round(10.5) == 11),
      ?_assert(trunc(10.5) == 10) ].

apply_test_() ->
    Module = condition,
    Function = even,
    Arguemts = [10],
    [ ?_assert(apply(Module, Function, Arguemts) == true),
      ?_assert(apply(condition, listlen, [[2,3,4]]) == 3) ].
