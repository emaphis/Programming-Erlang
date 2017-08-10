%%% @doc mailbox processing.

-module(mailbox).
-export([tester/1, receiver1/0, receiver2/0, tester2/1, receiver3/0]).

%%% @doc a function to test receiver processes, send 4 messages
%%% in quick succession.
tester(Pid) ->
    Pid ! one,
    Pid ! two,
    Pid ! three,
    Pid ! four.


%%% @doc receiver with no delay
receiver1() ->
    receive
        stop -> ok;
        Msg ->
            io:format("message:~w~n", [Msg]),
            receiver1()
    end.


%% test receiver1
%% 17> Pid = spawn(mailbox, receiver1, []).
%% <0.70.0>
%% 18> mailbox:tester(Pid).
%% message:one
%% message:two
%% four
%% message:three
%% 19> message:four


%%% %doc receiver with a delay
receiver2() ->
    timer:sleep(1000),
    receive
        stop -> ok;
        Msg ->
            io:format("message:~w~n", [Msg]),
            receiver2()
    end.

%% test receiver2
%% 3> Pid = spawn(mailbox, receiver2, []).
%% <0.79.0>
%% 24> mailbox:tester(Pid).
%% message:one
%% four
%% 25> message:two
%% 25> message:three
%% 25> message:four
%% 25>
%%% signifcant delay between handling each message
%%% but in same order as 1


%%% @doc tester function for receiver3
tester2(Pid) ->
    Pid ! {second, "SecondString"},
    Pid ! {first, "FirstString"}.

%%% @doc receiver process testing message order.
%%% had to look up this method online.
receiver3() ->
    receive
        stop -> ok;
        {first, Msg1} ->
            io:format("~s~n", [Msg1])
    end,
    receive
        {second, Msg2} ->
            io:format("~s~n", [Msg2])
    end,
    receiver3().

%% 28> Pid = spawn(mailbox, receiver3, []).
%% <0.89.0>
%% 29> mailbox:tester2(Pid).
%% FirstString
%% SecondString
%% {first,"FirstString"}
%% 30> Pid ! stop.
%% stop
%% 31>
