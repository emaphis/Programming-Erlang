%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%% 1.13 -- modified version
%% 1. register a process name.

-module(frequency1).
-export([start/0,init/0,loop/1]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(frequency1, spawn(frequency1, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

%% check first if frequency is already allocated
allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case lists:keyfind(Pid, 2, Allocated) of
    false ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
    _ ->
      {{[Freq|Free], Allocated}, {error, already_allocated}}    
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  case lists:keyfind(Pid, 2, Allocated) of
    {Freq, Pid} ->
       NewAllocated=lists:keydelete(Freq, 1, Allocated),
          {[Freq|Free],  NewAllocated};
    false  ->
       {{Free, Allocated}, {error, no_allocation}}
  end.

%% example run:
%% frequency1:start().
%% true
%% frequency1 ! {request,self(),allocate}.
%% {request,<0.37.0>,allocate}
%% receive {reply,Msg} -> Msg end.
%% {ok,10}
%% frequency1 ! {request,self(),{deallocate,10}}.
%% {request,<0.37.0>,{deallocate,10}}
%% f(Msg).
%% ok.
%% receive {reply,Msg} -> Msg end.
%% ok.
%% frequency1 ! {request,self(),allocate}.
%% {request,<0.37.0>,allocate}
%% f(Msg).
%% ok.
%% receive {reply,Msg} -> Msg end.
%% {ok,10}
%% frequency1 ! {request,self(),allocate}.
%% {error,already_allocated}
%% frequency1 ! {request,self(),stop}.
%% {request,<0.37.0>,stop}
%%
