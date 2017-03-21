%% Ex 4-2 ring server exercise.

-module(myring2).
-export([start/3, loop/2, spawn_proc/3]).

%% interface to spawn first process in the ring
%% NP - number of processes
%% NM - number of messages to pass
%% Msg - the message passed around the ring.
start(NP, NM, Msg) ->
    spawn(myring2, spawn_proc, [NP, NM, Msg]).

%% start first processes which spawns N-1 subproces in a ring.
spawn_proc(NP, NM, Msg) ->
    spawn_proc(NP, NM, Msg, self()).

%% spawn N-1 linked processes, then start proc N
spawn_proc(1 ,NM, Msg, Pid) ->
    io:format("proc 1, ~p sends first message~p to ~p~n",
              [self(), Msg, Pid]),
    Pid ! Msg,
    loop(Pid, NM-1);
spawn_proc(NP, NM, Msg, Pid) ->
    PidNext = spawn_link(myring2, loop, [Pid, NM]),
    io:format("start proc ~p, ~p and connect to proc ~p~n",
              [NP, PidNext, Pid]),
    spawn_proc(NP-1, NM, Msg, PidNext).

%% main loop, block on messages.
%% Pid - next process in the ring
%% NM - number of message sends left.
loop(Pid, 1) ->
    receive
        Msg ->
            io:format("~p sends message ~p, num 1 to ~p~n",
                      [self(), Msg, Pid]),
            Pid ! Msg,
            ok
    end;

loop(Pid, NM) ->
    receive
        Msg ->
            io:format("~p sends message ~p, num ~p to ~p~n",
                      [self(), Msg, NM, Pid]),
            Pid ! Msg,
            loop(Pid, NM-1)
    end.


%% Start 5 processes sending 3 messages of "hello"

%% > myring2:start(5,3,"hello").
%% start proc 5, <0.152.0> and connect to proc <0.151.0>
%% start proc 4, <0.153.0> and connect to proc <0.152.0>
%% <0.151.0>
%% start proc 3, <0.154.0> and connect to proc <0.153.0>
%% start proc 2, <0.156.0> and connect to proc <0.154.0>
%% proc 1, <0.151.0> sends first message "hello" to <0.156.0>
%% <0.156.0> sends message "hello", num 3 to <0.154.0>
%% <0.154.0> sends message "hello", num 3 to <0.153.0>
%% <0.153.0> sends message "hello", num 3 to <0.152.0>
%% <0.152.0> sends message "hello", num 3 to <0.151.0>
%% <0.151.0> sends message "hello", num 2 to <0.156.0>
%% <0.156.0> sends message "hello", num 2 to <0.154.0>
%% <0.154.0> sends message "hello", num 2 to <0.153.0>
%% <0.153.0> sends message "hello", num 2 to <0.152.0>
%% <0.152.0> sends message "hello", num 2 to <0.151.0>
%% <0.151.0> sends message "hello", num 1 to <0.156.0>
%% <0.156.0> sends message "hello", num 1 to <0.154.0>
%% <0.154.0> sends message "hello", num 1 to <0.153.0>
%% <0.153.0> sends message "hello", num 1 to <0.152.0>
%% <0.152.0> sends message "hello", num 1 to <0.151.0>
