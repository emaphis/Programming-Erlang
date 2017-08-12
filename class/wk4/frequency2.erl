%%% @doc frequency server step two
%%% allocate limited frequencies to mobile phone users.
%%% Server version 2
%%% Exercise 1.15 - refactering server

-module(frequency2).
-export([start/0,  allocate/0, deallocate/1, stop/0, clear/0]).
-export([init/0]).
-export([test_server/0]).


%%% @doc a function for testing the server.
test_server()->
    io:format("~w~n", [frequency2:start()]),
    io:format("~w~n", [frequency2:clear()]),
    io:format("~w~n", [frequency2:allocate()]),
    io:format("~w~n", [frequency2:allocate()]),
    io:format("~w~n", [frequency2:deallocate(10)]),
    io:format("~w~n", [frequency2:allocate()]),
    io:format("~w~n", [frequency2:stop()]),
    done.


%%% @doc start the named server.

start() ->
    register(frequency2, spawn(frequency2, init, [])).


%%% @doc initialize the server with frequencies and start the main loop.

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

%% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].


%%% @doc the main server loop.

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
%            timer:sleep(1000),    % simulate a slow server.
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);

        {request, Pid, {deallocate, Freq}} ->
%            timer:sleep(1000),    % simulate a slow server.
            NewFrequencies = deallocate(Frequencies, Freq),
            Pid ! {reply, ok},
            loop(NewFrequencies);

        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end.


%%% @doc Clear the message queue, or skip if no messages.
clear() ->
    receive
        Msg ->
            io:format("clearing message:~w~n", [Msg]),
            clear()   % recurse
    after 0 ->    % skip if no messages
            ok
    end.


%% a functional message interface

allocate() ->
    clear(),    % clear the message queue.
    frequency2 ! {request, self(), allocate},
    receive
        {reply, Reply} -> Reply
    after 500 ->
        time_out
    end.

deallocate(Freq) ->
    clear(),    % clear the message queue.
    frequency2 ! {request, self(), {deallocate, Freq}},
    receive
        {reply, Reply} -> Reply
    after 500 ->
        time_out
    end.

stop() ->
    clear(),
    frequency2 ! {request, self(), stop},
    receive
        {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.


deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.


%% example run no delay:
%% 31> frequency2:test_server().
%% true
%% ok
%% {ok,10}
%% {ok,11}
%% ok
%% {ok,10}
%% stopped
%% ok

%% example run with server delay.
%% 33> frequency2:test_server().
%% true
%% ok
%% time_out
%% time_out
%% {ok,10}
%% time_out
%% {ok,11}
%% ok
%% 34>
