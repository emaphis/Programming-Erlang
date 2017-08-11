%%% @doc frequency server step one
%%% allocate limited frequencies to mobile phone users.
%%% Server version 0 - the original
%%% Module 1.12
-module(frequency).
-export([init/0]).

%% Communication mechnism
%% Communication protocal
%% Fault tolerance
%% Library support: OTP
%% Scaling

%% the functional model of the state of the server
%%  ... a pair of lists {Free, Allocated} ...
%%  ... a function to 'allocate' a frequency, if possible.
%%  ... a function to 'deallocate' a frequencies
%%  ... both functions return a new state (and a result).

%% to start.....
%% Freq =  spawn(frequency, init, []).

-type freq() :: integer().
%-type frequencies() :: {[freq(),freq()]}.

%%% @doc initialize the server with frequencies and start the main loop.
-spec init() -> {reply, stopped}.
init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

%% Hard Coded
-spec get_frequencies() -> [freq()].
get_frequencies() -> [10,11,12,13,14,15].

%%% @doc the main server loop.
-spec loop({[freq()],[freq()]}) -> {reply, stopped}.

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);

        {request, Pid, {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            Pid ! {reply, ok},
            loop(NewFrequencies);

        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end.

%%% @doc A client can request that a frequency can be allocated.
-spec allocate({[freq()],[freq()]}, pid()) ->
                      {{[freq()],[freq()]},{error,no_frequency} | {ok,freq()}}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.


%%% @doc A client can request that a frequency can be deallocated.
-spec deallocate({[freq()],[freq()]}, freq()) -> {[freq()],[freq()]}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.

%% example run:
%% 10> Freq = spawn(frequency,init,[]).
%% <0.49.0>
%% 11> Freq ! {request,self(),allocate}.
%% {request,<0.37.0>,allocate}
%% 12> receive {reply,Msg} -> Msg end.
%% {ok,10}
%% 13> Freq ! {request,self(),{deallocate,10}}.
%% {request,<0.37.0>,{deallocate,10}}
%% 15> f(Msg).
%% ok.
%% 16> receive {reply,Msg} -> Msg end.
%% ok.
%% 17> Freq ! {request,self(),allocate}.
%% {request,<0.37.0>,allocate}
%% 18> f(Msg).
%% ok.
%% 19> receive {reply,Msg} -> Msg end.
%% {ok,10}
%% 20> Freq ! {request,self(),allocate}.
%% {ok,11}
%% 21> Freq ! {request,self(),stop}.
%% {request,<0.37.0>,stop}
%%
