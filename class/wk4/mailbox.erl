%% 1.8 -- mailbox handling experiments

-module(mailbox).

-export([receiver/0,sender/1,receiver2/0,sender2/1]).


receiver() ->
    receive
        stop ->
            ok;
        Msg ->
            io:format("message:~w~n", [Msg]),
            receiver()
    end.

%% test run
%% (emacs@kimba01)25> Pid = spawn(mailbox,receiver,[]).
%% <0.99.0>
%% (emacs@kimba01)26> Pid ! hello.
%% message:hello
%% hello
%% (emacs@kimba01)27> Pid ! world.
%% message:world
%% world
%% (emacs@kimba01)28> Pid ! goodbye_world.
%% message:goodbye_world
%% goodbye_world
%% (emacs@kimba01)29> Pid ! stop.
%% stop
%% (emacs@kimba01)30>


sender(Pid) ->
    Pid ! {ok,40},
    Pid ! {ok,41},
    Pid ! {ok,42},
    Pid ! {ok,43}.

%% second test run:
%% (emacs@kimba01)20> f().
%% ok
%% (emacs@kimba01)21> Pid = spawn(mailbox,receiver,[]).
%% <0.94.0>
%% (emacs@kimba01)22> mailbox:sender(Pid).
%% message:{ok,40}
%% message:{ok,41}
%% {ok,43}
%% message:{ok,42}
%% (emacs@kimba01)23> message:{ok,43}
%% (emacs@kimba01)23> Pid ! stop.
%% stop
%% (emacs@kimba01)24>


%% Priority receive,
%% please see:
%% http://stackoverflow.com/questions/8699498/is-there-a-clever-way-to-give-messages-different-priorities

receiver2() ->
    receive
        stop ->
            ok;
        {first, Str} ->
            io:format("first - message:~s~n", [Str]),
            receiver2()
    after 0 ->
         receive
            stop ->
               ok;
            {Priority, Str} ->
                io:format("~w - message:~s~n", [Priority, Str]),
                receiver2()
        end
    end.

sender2(Pid) ->
    Pid ! {first, "FirstString"},
    Pid ! {second, "SecondString"},
    Pid ! {first, "ThirdString"}.


%% (emacs@kimba01)46> Pid = spawn(mailbox,receiver2,[]).
%% <0.144.0>
%% (emacs@kimba01)47> mailbox:sender2(Pid).
%% first - message:FirstString
%% first - message:ThirdString
%% {first,"ThirdString"}
%% second - message:SecondString
%% (emacs@kimba01)48> Pid ! stop.
%% stop
