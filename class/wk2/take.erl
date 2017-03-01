%% 2.11, 2.12 - take definition

-module(take).
-author('emaphis').

-export([take/2, take_test/0, take_tc/2, take_tc_test/0, split1/2, take3/2]).
-export([examples_test/0, take2/2, take2_test/0]).

%% direct recursion
take(0, _) -> [];
take(_, []) -> [];
take(N, [X|Xs]) when N>0 ->
    [X | take(N-1, Xs)].

take_test() ->
    []      = take(0, "hello"),
    "h"     = take(1, "hello"),
    "hell"  = take(4, "hello"),
    "hello" = take(5, "hello"),
    "hello" = take(9, "hello"),
    ok.


%% tail-call version
take_tc(N, Xs) -> lists:reverse(take_tc(N, Xs, [])).
take_tc(0, _, Acc) -> Acc;
take_tc(_, [], Acc) -> Acc;
take_tc(N, [X|Xs], Acc) ->
     take_tc(N-1, Xs, [X | Acc]).

take_tc_test() ->
    []      = take_tc(0, "hello"),
    "h"     = take_tc(1, "hello"),
    "hell"  = take_tc(4, "hello"),
    "hello" = take_tc(5, "hello"),
    "hello" = take_tc(9, "hello"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%
%% some examples

examples_test() ->
    {"hell", "o"} = lists:split(4, "hello"),
    ok.

take2(N,Xs) ->
    {Front, _Back} = lists:split(N,Xs),
    Front.

take2_test() ->
    []      = take2(0, "hello"),
    "h"     = take2(1, "hello"),
    "hell"  = take2(4, "hello"),
    "hello" = take2(5, "hello"),
    ok.

split1(N,Xs) -> split1(N, Xs, []).

split1(0, Xs, R) -> {lists:reverse(R, []), Xs};
split1(N, [X|Xs], R) -> split1(N-1, Xs, [X|R]);
split1(_, [], _) -> badarg.


take3(N, Xs) -> take3(N, Xs, []).

take3(0, _Xs, Acc) -> listS:reverse(take3(Acc, []));
take3(N, [X|Xs], Acc) -> take3(N-1, Xs, [X|Acc]);
take3(_, [], Acc) -> list:reverse(Acc,[]). 
