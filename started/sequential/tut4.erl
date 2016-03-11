%% tut4 -- exercising lists

-module(tut4).

-export([list_length/1,tc_length/1]).

list_length([]) ->
    0;
list_length([_|XS]) ->
    1 + list_length(XS).



%% tail call length function
tc_length(XS) ->
    tc_length(XS,0).

tc_length([], N) ->
    N;
tc_length([_|XS],N) ->
    tc_length(XS, N+1).
