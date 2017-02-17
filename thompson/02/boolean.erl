-module(boolean).
-export([b_not/1,b_and/2,b_or/2]).
-include_lib("eunit/include/eunit.hrl").

b_not(true) ->
    false;
b_not(false) ->
    true.

not_test_() ->
    [?_assert(b_not(true) =:= false),
     ?_assert(b_not(false) =:= true)].


b_and(true, A) ->
    A;
b_and(false,_) ->
    false.

and_test_() ->
    [?_assert(b_and(true,true) =:= true),
     ?_assert(b_and(true,false) =:= false),
     ?_assert(b_and(false,false) =:= false),
     ?_assert(b_and(false,true) =:= false)].

b_or(false, A)->
    A;
b_or(true, _) ->
    true.

or_test_() ->
    [?_assert(b_or(true,true) =:= true),
     ?_assert(b_or(true,false) =:= true),
     ?_assert(b_or(false,false) =:= false),
     ?_assert(b_or(false,true) =:= true)].
