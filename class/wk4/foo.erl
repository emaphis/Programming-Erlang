%% concurrency example

-module(foo).
-export([bar/0,bar/1,baz/0,bazz/0]).

bar() ->
    timer:sleep(500),
    io:format("bar started~n"),
    io:format("bar working~n"),
    io:format("bar finished~n").


bar(Pid) ->
    Pid ! "bar started~n",
    Pid ! "bar working~n",
    Pid ! "bar finished~n".

%% (emacs@kimba01)7> spawn(foo,bar,[self()]).
%% <0.61.0>
%% (emacs@kimba01)8> flush().
%% Shell got "bar started~n"
%% Shell got "bar working~n"
%% Shell got "bar finished~n"
%% ok


baz() ->
    receive
        Msg ->
            io:format("got: ~s~n",[Msg])
    end,
    baz().


bazz() ->
    receive
        stop ->
            io:format("stopped~n");
        Msg ->
            io:format("got: ~s~n",[Msg]),
            bazz()
    end.

%% (emacs@kimba01)12> Bazz = spawn(foo,bazz,[]).
%% <0.79.0>
%% (emacs@kimba01)13> Bazz ! hello.
%% got: hello
%% hello
%% (emacs@kimba01)14> Bazz ! ola.
%% got: ola
%% ola
%% (emacs@kimba01)15> Bazz ! stop.
%% stopped
%% stop

