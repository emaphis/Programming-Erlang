%% the echo example

-module(echo).

-export([go/0, loop/0]).

go() ->
    Pid = spawn(echo, loop, []),
    Pid ! {self(), hello},
    receive
        {Pid, Msg} ->
            io:format("~w~n",[Msg])
    end,
    Pid ! stop.

loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop ->
            true
    end.

%% (emacs@kimba01)52> Pid = self().
%% <0.1128.0>
%% (emacs@kimba01)53> Pid ! hello.
%% hello
%% (emacs@kimba01)54> flush().
%% Shell got hello
%% ok
%% (emacs@kimba01)55> Pid2 = pid(0,1128,0).
%% <0.1128.0>
%% (emacs@kimba01)56> Pid2 ! hello2.
%% hello2
%% (emacs@kimba01)57> flush().
%% Shell got hello2
%% ok
%% (emacs@kimba01)58> self().
%% <0.1128.0>
%% (emacs@kimba01)59> 1/0.
%% ** exception error: an error occurred when evaluating an arithmetic expression
%%      in operator  '/'/2
%%         called as 1 / 0
%% (emacs@kimba01)60> self().
%% <0.1188.0>

%%(emacs@kimba01)62> echo:go().
%%hello
%%stop
