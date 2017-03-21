%% exercise 4-1
%% an echo server.

-module(echo3).
-export([start/0, print/1, stop/0, loop/0]).

start() ->
    register(echo, spawn(echo3, loop, [])),
    ok.

print(Term) ->
    echo ! {print, Term},
    ok.

stop() ->
    echo ! stop,
    ok.

loop() ->
    receive
        {print, Term} ->
            io:format("~w~n", [Term]),
            loop();
        stop  ->
            ok;
        _Any ->
            loop()
    end.


%% (emacs@kimba01)85> echo3:start().
%% ok
%% (emacs@kimba01)86> echo3:print(hello).
%% hello
%% ok
%% (emacs@kimba01)87> echo3:print("hello").
%% [104,101,108,108,111]
%% ok
%% (emacs@kimba01)88> echo3:print(3+4).
%% 7
%% ok
%% (emacs@kimba01)89> echo3:print("Hello, world").
%% [72,101,108,108,111,44,32,119,111,114,108,100]
%% ok
%% (emacs@kimba01)90> echo3:stop().
%% ok
%% (emacs@kimba01)91> echo3:print(hello).
%% ** exception error: bad argument
%%      in function  echo3:print/1 (c:/Users/emaphis/src/Programming-Erlang/thompson/04/echo3.erl, line 12)
