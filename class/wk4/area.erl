%% area example

-module(area).

-export([area/0]).


area() ->
    receive
        {square,X} ->
            X*X;
        {rectangle,X,Y} ->
            X*Y
    end,
    area().
