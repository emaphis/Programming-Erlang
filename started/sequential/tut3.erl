%% tut2 -- exercising tuples

-module(tut3).

-export([convert_length/1]).


%% length conversion
convert_length({centemeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.
