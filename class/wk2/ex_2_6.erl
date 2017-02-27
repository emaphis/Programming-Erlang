%% exercises for 2.6

-module(ex_2_6).
-export([prod/1, max/1, prod_tc/1, max_tc/1]).
-export([prod_test/0, max_test/0,prod_tc_test/0, max_tc_test/0]).

%% template:
%% foo([])     -> .....;
%% foo([X|Xs]) -> ... foo(Xs) ...

%% product of a list
prod([]) -> 1;
prod([N|Ns]) -> N * prod(Ns).

prod_test() ->
    1 = prod([]),
    6 = prod([1,2,3]),
    ok.

%% maximum of a list
max([N]) -> N;
max([N|Ns]) ->
   max(N, max(Ns)).

max_test() ->
    1 = max([1]),
    3 = max([1,2,3]),
    3 = max([3,2,1]),
    3 = max([2,3,1]),
    ok.


%% tail-call examples
prod_tc(Lst) -> prod_tc(Lst, 1).

prod_tc([],P) -> P;
prod_tc([N|Ns],P) ->  prod_tc(Ns, N*P).

prod_tc_test() ->
    1 = prod_tc([]),
    6 = prod_tc([1,2,3]),
    ok.


max_tc([N|Ns]) -> max_tc(Ns,N).

max_tc([], M) -> M;
max_tc([N|Ns], M) ->
  max_tc(Ns, max(N,M)).

max_tc_test() ->
    1 = max_tc([1]),
    3 = max_tc([1,2,3]),
    3 = max_tc([3,2,1]),
    3 = max_tc([2,3,1]),
    ok.

%% I think direct recursion is eaier to use. The desing of the
%% fuction matches the data defintion more directly.
