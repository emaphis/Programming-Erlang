%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%% 1.13 -- modified version

-module(frequency1).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

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
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
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


%% example run:
%% Freq = spawn(frequency1,init,[]).
%% <0.49.0>
%% Freq ! {request,self(),allocate}.
%% {request,<0.37.0>,allocate}
%% receive {reply,Msg} -> Msg end.
%% {ok,10}
%% Freq ! {request,self(),{deallocate,10}}.
%% {request,<0.37.0>,{deallocate,10}}
%% f(Msg).
%% ok.
%% receive {reply,Msg} -> Msg end.
%% ok.
%% Freq ! {request,self(),allocate}.
%% {request,<0.37.0>,allocate}
%% f(Msg).
%% ok.
%% receive {reply,Msg} -> Msg end.
%% {ok,10}
%% Freq ! {request,self(),allocate}.
%% {ok,11}
%% Freq ! {request,self(),stop}.
%% {request,<0.37.0>,stop}
%%
