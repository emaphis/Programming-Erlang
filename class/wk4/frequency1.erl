%%% @doc frequency server step one
%%% allocate limited frequencies to mobile phone users.
%%% Server version 0
%%% Module 1.13 - modifying the frequency server.
-module(frequency1).
-export([start/0, init/0]).


%% to start.....
%% Freq =  spawn(frequency, init, []).

-type freq() :: integer().

%%% frequencies is a pair of unallocated and allocated frequencies.
%-type frequencies() :: {[freq()], [freq()]}.

%%% @doc start the named server.
start() ->
    register(frequency1, spawn(frequency1, init, [])).


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
                      {{[freq()],[freq()]},{error,no_frequency} |
                       {[freq()],[freq()], {error,already_allocated}} |
                       {ok,freq()}}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};

allocate({[Freq|Free], Allocated}, Pid) ->
    case lists:keymember(Pid, 2, Allocated) of
        true  -> {{[Freq|Free], Allocated}, {error, already_allocated}};
        false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
    end.

%%% @doc A client can request that a frequency can be deallocated.
-spec deallocate({[freq()],[freq()]}, freq()) -> {[freq()],[freq()]}.

deallocate({Free, Allocated}, Freq) ->
    case lists:keyfind(Freq, 2, Allocated) of
        true ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {[Freq|Free], NewAllocated};
        false -> {Free, Allocated}
    end.

%% example run:
%% 10> frequency1:start().
%% true
%% 11> frequency1 ! {request,self(),allocate}.
%% {request,<0.37.0>,allocate}
%% 12> receive {reply,Msg} -> Msg end.
%% {ok,10}
%% 13> f(Msg).
%% ok.
%% 14> frequency1 ! {request,self(),{deallocate,10}}.
%% {request,<0.37.0>,{deallocate,10}}
%% 15> receive {reply,Msg} -> Msg end.
%% ok.
%% 16> f(Msg).
%% ok.
%% 17> frequency1 ! {request,self(),allocate}.
%% {request,<0.37.0>,allocate}
%% 18> receive {reply,Msg} -> Msg end.
%% {ok,10}
%% 19> f(Msg).
%% ok.
%% 20> frequency1 ! {request,self(),allocate}.
%% {request,<0.31.0>,allocate}
%% 18> receive {reply,Msg} -> Msg end.
%% {ok,10}
%% f(Msg).
%% ok.
%% 22> frequency1 ! {request,self(),stop}.
%% {request,<0.37.0>,stop}
%%
