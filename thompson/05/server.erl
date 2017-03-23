%% a server example

-module(server).

-export([start/2, stop/1, call/2]).

-export([init/1]).


start(Name, Data) ->
    Pid = spawn(genric_handler, init, [Data]),
    register(Name, Pid),
    ok.

stop(Name) ->
    Name ! {stop, self()},
    ok.

call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
    To ! {reply, Msg}.

init(Data) ->
    loop(initialize(Data)).

loop(State) ->
    receive
        {request, From, Msg} ->
            {Reply, NewState} = handle_msg(Msg, State),
            reply(From, Reply),
            loop(NewState);
        {stop, From} ->
            reply(From, terminate(State))
    end.

initialize(Data) -> Data.

handle_msg(Msg, State) -> {Msg, State}.

terminate(State) -> State.
