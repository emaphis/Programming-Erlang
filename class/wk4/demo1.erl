%% area example

-module(demo1).

-export([area/0, client/1]).


area() ->
    receive
        {From, {square,X}} ->
            From ! {self(), X*X};
        {From, {rectangle,X,Y}} ->
            From ! {self(), X*Y}
    end,
    area().


client(Pid) ->
    Pid ! {self(), {square, 10}},
    receive
        {Pid, Reply} ->
            Reply
    end.
