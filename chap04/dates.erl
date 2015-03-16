%%% date functions for chapter 4 exercise 3.

-module(dates).
-export([my_time_func/1, my_date_string/0]).

my_time_func(F) ->
    Now = erlang:now(),
    F(),
    Later = erlang:now (),
    {N1, N2, N3} = Now,
    {L1, L2, L3} = Later,
    Span = {L1-N1, L2-N2, L3-N3},
    Span.

my_date_string() ->
    {Yr, Mo, Da} = erlang:date(),
    {Hr, Mn, Sc} = erlang:time(),
    io:fwrite("~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B~n", [Mo, Da, Yr, Hr, Mn, Sc]).
