%%% concurrent abstractions in Erlang

-module(abtractions).
-export([for/3, test_for/0, rpc/2, promise/2, yield/1, pmap/1, do/2]).


%% a for loop

for(Max,Max,F) ->
    [F(Max)];
for(I,Max,F) ->
    [F(I) | for(I+1,Max,F)].

test_for() ->
    [1,4,9,16,25]  = for(1,5, fun(I) -> I*I end).


%% RPC abtraction - remote procedure call.

rpc(Pid, Request) ->
    Tag = erlang:make_ref(),
    Pid ! {self(), Tag, Request},
    receive
        {Tag, Response} ->
            Response
    after 100 -> time_out
    end.


%% Futures

promise(Pid, Request) ->
    Tag = erlang:make_ref(),
    Pid ! {self(), Tag, Request},
    Tag.

yield(Tag) ->
    receive
        {Tag, Response} ->
            Response
    after 100 -> time_out
    end.

%% Tag = promise(Pid, fun() -> ... end),
%% ... do some other computations ..
%% Val = yield(Tag).

%% Parallel computations

%% par begin
%%   F1,
%%   F2,
%%   F3,
%% par end.


pmap(L) ->
    S = self(),
    Pids = [do(S,F) || F <- L],
    [receive {Pid,Val} -> Val end || Pid <- Pids].

do(Parent, F) ->
    spawn(fun() ->
            Parent !{self(), F()}
          end).
