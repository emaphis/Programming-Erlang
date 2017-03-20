%% the echo example with process registry.

-module(echo2).

-export([go/0, loop/0]).

go() ->
    register(echo, spawn(echo2, loop, [])),
    echo ! {self(), hello},
    receive
        {_Pid, Msg} ->
            io:format("~w~n",[Msg])
    end.
    %Pid ! stop.

loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop ->
            true
    end.


%% (emacs@kimba01)69> echo2:go().
%% hello
%% ok
%% (emacs@kimba01)71> whereis(echo).
%% <0.1216.0>
%% (emacs@kimba01)72> echo ! stop.
%% stop
%% (emacs@kimba01)73> whereis(echo).
%% undefined
