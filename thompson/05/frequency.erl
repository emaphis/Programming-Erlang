%% client server example

-module(frequency).

%% start server.
-export([start/0]).

%% client functions
-export([stop/0, allocate/0, deallocate/1]).

-export([init/0]).


%% These are the start functions use to create and
%% initialize the server

start() -> % register a process with the same name as the module
    register(frequency, spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

%% Hard Coded
%% state or loop data.
get_frequencies() -> [10,11,12,13,14,15].


%% the client functions

stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).


%% Hide all message passing and the message
%% protocol in a functional interface.

call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.


%% The main loop

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            reply(Pid, ok),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.


%% The internal help fuctions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.


%% 1> c(frequency). {ok,frequency}
%% 2> frequency:start(). true
%% 3> frequency:allocate(). {ok,10}
%% 4> frequency:allocate(). {ok,11}
%% 5> frequency:allocate(). {ok,12}
%% 6> frequency:allocate(). {ok,13}
%% 7> frequency:allocate(). {ok,14}
%% 8> frequency:allocate(). {ok,15}
%% 9> frequency:allocate(). {error,no_frequency}
%% 10> frequency:deallocate(11). ok
%% 11> frequency:allocate(). {ok,11}
%% 12> frequency:stop().
%% ok
