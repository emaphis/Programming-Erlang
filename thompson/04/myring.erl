%% benchmarking message send.

-module(myring).
-export([start/1, start_proc/2]).

start(Num) ->
    start_proc(Num,self()).

start_proc(0, Pid) ->
    Pid ! ok;
start_proc(Num,Pid) ->
    NPid = spawn(?MODULE, start_proc, [Num-1,Pid]),
    NPid ! ok,
    receive ok -> ok end.

%% (emacs@kimba01)79> timer:tc(myring, start, [100000]).
%% {1046000,ok}  %- 1 sec for 100000 proceses
%% (emacs@kimba01)80> timer:tc(myring, start, [1000000]).
%% {9953000,ok}  %- 10 sec for a million proceses
%% (emacs@kimba01)81> timer:tc(myring, start, [10000000]).
%% {97641000,ok} %- 97 sec for 10 million processes. 
%% (emacs@kimba01)82> 
